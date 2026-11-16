from __future__ import annotations

import argparse
import sys


def _connect(args):
    from .client import SutraClient
    return SutraClient(args.base_url, token=args.token,
                       username=args.username, password=args.password)


def _auth_flags(parser):
    parser.add_argument("--base-url", default="http://localhost:8030")
    parser.add_argument("--token")
    parser.add_argument("--username")
    parser.add_argument("--password")


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(prog="sutra-platform-sdk")
    sub = ap.add_subparsers(dest="cmd", required=True)

    g = sub.add_parser("generate", help="generate an client SDK from a live ontology")
    _auth_flags(g)
    g.add_argument("--ontology", help="name stamped into the generated module")
    g.add_argument("--out", help="output .py path")
    g.add_argument("--package", help="build an installable package tree here instead")
    g.add_argument("--name", help="package name for --package (a Python identifier)")

    v = sub.add_parser("verify", help="check a generated client SDK against the live ontology")
    _auth_flags(v)
    v.add_argument("--module", required=True, help="path of the generated .py")

    f = sub.add_parser("fingerprint", help="print the live ontology fingerprint")
    _auth_flags(f)

    args = ap.parse_args(argv)

    if args.cmd == "generate":
        if bool(args.out) == bool(args.package):
            ap.error("pass either --out <file.py> or --package <dir>")
        client = _connect(args)
        if args.package:
            if not args.name:
                ap.error("--package needs --name")
            from .package import write_package
            manifest = write_package(client.schema(refresh=True), args.package,
                                     name=args.name,
                                     ontology=args.ontology or "sutra")
            print(f"wrote {manifest['dir']}  ·  {manifest['dist']} "
                  f"{manifest['version']}")
            print(f"  pip install {manifest['dir']}")
            return 0
        from .generate import generate_from_client
        generate_from_client(client, args.out, ontology=args.ontology)
        print(f"wrote {args.out}")
        return 0

    if args.cmd == "verify":
        from .schema import fingerprint
        stamped = _stamped_fingerprint(args.module)
        live = fingerprint(_connect(args).schema(refresh=True))
        if stamped == live:
            print(f"ok — {args.module} matches the live ontology ({live})")
            return 0
        print(f"DRIFT — {args.module} was generated against {stamped}, "
              f"the server now serves {live}")
        return 1

    if args.cmd == "fingerprint":
        from .schema import fingerprint
        print(fingerprint(_connect(args).schema(refresh=True)))
        return 0

    return 1


def _stamped_fingerprint(module_path: str) -> str | None:
    import ast
    with open(module_path, "r", encoding="utf-8") as handle:
        tree = ast.parse(handle.read())
    for node in tree.body:
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name) and target.id == "__ontology_fingerprint__":
                    return ast.literal_eval(node.value)
    return None


if __name__ == "__main__":
    sys.exit(main())
