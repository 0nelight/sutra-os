import json
import os
import subprocess
import sys
import tempfile

import sutra_transforms as S

_fails = []


def check(name, ok):
    print(("  ok: " if ok else "  FAIL: ") + name)
    if not ok:
        _fails.append(name)


MODULE = """
from sutra_transforms import transform, Output

@transform(inputs={}, output=Output(kind='object-type', object_type='t', primary_key='id'))
def t():
    return [{'id': '1'}]
"""


def run(version):
    with tempfile.NamedTemporaryFile('w', suffix='.py', delete=False) as f:
        f.write(MODULE)
    env = dict(os.environ)
    env.pop('SUTRA_PROTOCOL_VERSION', None)
    if version is not None:
        env['SUTRA_PROTOCOL_VERSION'] = version
    try:
        return subprocess.run([sys.executable, '-m', 'sutra_transforms', '--run', f.name],
                              capture_output=True, text=True, env=env)
    finally:
        os.unlink(f.name)


ok = run(S.PROTOCOL_VERSION)
check("a run under the same version exits 0", ok.returncode == 0)
check("its result names the version it speaks",
      json.loads(ok.stdout).get('protocol_version') == S.PROTOCOL_VERSION)

minor = run(S.PROTOCOL_VERSION.split('.')[0] + '.99')
check("a host of a later MINOR is served", minor.returncode == 0)

legacy = run(None)
check("a host that names no version is served", legacy.returncode == 0)

bad = run('99.0')
last = [l for l in bad.stderr.splitlines() if l.strip()][-1]
check("a host of another MAJOR is refused with exit 1", bad.returncode == 1)
check("the refusal is an error line naming the mismatch",
      json.loads(last).get('type') == 'ProtocolVersionMismatch')

if _fails:
    sys.exit(1)
