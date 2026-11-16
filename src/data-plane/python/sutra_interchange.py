import base64
import datetime
import decimal
import io
import json
import sys

_TEXT = {'string', 'text', 'keyword', 'uuid', 'geoshape', 'geopoint', 'geohash',
         'media-reference', 'attachment', 'timeseries', 'marking', 'cipher'}

ARROW_FILE = 'application/vnd.apache.arrow.file'
PARQUET = 'application/vnd.apache.parquet'


def _pa():
    import pyarrow
    return pyarrow


def arrow_type(t):
    pa = _pa()
    if isinstance(t, list):
        head = t[0]
        if head == 'decimal':
            return pa.decimal128(int(t[1]), int(t[2]))
        if head == 'array':
            return pa.list_(arrow_type(t[1]))
        if head == 'vector':
            return pa.list_(pa.float64(), int(t[1]))
        if head == 'struct':
            return pa.struct([(k, arrow_type(v)) for k, v in t[1].items()])
        return pa.string()
    return {
        'number': pa.decimal128(38, 10), 'decimal': pa.decimal128(38, 10),
        'byte': pa.int8(), 'short': pa.int16(), 'integer': pa.int32(), 'long': pa.int64(),
        'float': pa.float64(), 'double': pa.float64(), 'boolean': pa.bool_(),
        'date': pa.date32(),
        'timestamp': pa.timestamp('us', tz='UTC'), 'instant': pa.timestamp('us', tz='UTC'),
    }.get(t, pa.string())


def value_type(at):
    pa = _pa()
    t = pa.types
    if t.is_boolean(at):
        return 'boolean'
    if t.is_int8(at) or t.is_uint8(at):
        return 'byte'
    if t.is_int16(at) or t.is_uint16(at):
        return 'short'
    if t.is_int32(at):
        return 'integer'
    if t.is_integer(at):
        return 'long'
    if t.is_floating(at):
        return 'double'
    if t.is_decimal(at):
        return ['decimal', at.precision, at.scale]
    if t.is_date(at):
        return 'date'
    if t.is_timestamp(at):
        return 'timestamp'
    if t.is_fixed_size_list(at):
        return ['vector', at.list_size]
    if t.is_list(at) or t.is_large_list(at):
        return ['array', value_type(at.value_type)]
    if t.is_struct(at):
        return ['struct', {at.field(i).name: value_type(at.field(i).type)
                           for i in range(at.num_fields)}]
    return 'string'


def _instant(s):
    return datetime.datetime.fromisoformat(s.replace('Z', '+00:00'))


def to_arrow_value(v, t):
    if v is None:
        return None
    if isinstance(t, list):
        head = t[0]
        if head == 'decimal':
            return decimal.Decimal(str(v)).quantize(decimal.Decimal(1).scaleb(-int(t[2])))
        if head == 'array':
            return [to_arrow_value(x, t[1]) for x in v]
        if head == 'vector':
            return [float(x) for x in v]
        if head == 'struct':
            return {k: to_arrow_value(v.get(k), st) for k, st in t[1].items()}
        return json.dumps(v) if not isinstance(v, str) else v
    if t in ('number', 'decimal'):
        return decimal.Decimal(str(v)).quantize(decimal.Decimal(1).scaleb(-10))
    if t == 'date':
        return datetime.date.fromisoformat(str(v)[:10])
    if t in ('timestamp', 'instant'):
        return _instant(str(v)) if isinstance(v, str) else v
    if t in ('byte', 'short', 'integer', 'long'):
        return int(v)
    if t in ('float', 'double'):
        return float(v)
    if t == 'boolean':
        return bool(v)
    return v if isinstance(v, str) else json.dumps(v)


def plain(v):
    if v is None:
        return None
    if isinstance(v, decimal.Decimal):
        return format(v, 'f')
    if isinstance(v, datetime.datetime):
        if v.tzinfo is None:
            v = v.replace(tzinfo=datetime.timezone.utc)
        return v.astimezone(datetime.timezone.utc).isoformat().replace('+00:00', 'Z')
    if isinstance(v, datetime.date):
        return v.isoformat()
    if isinstance(v, float) and v != v:
        return None
    if isinstance(v, (list, tuple)):
        return [plain(x) for x in v]
    if isinstance(v, dict):
        return {k: plain(x) for k, x in v.items()}
    item = getattr(v, 'item', None)
    if callable(item) and type(v).__module__ == 'numpy':
        return plain(item())
    if type(v).__name__ in ('NaTType',):
        return None
    if hasattr(v, 'to_pydatetime'):
        return plain(v.to_pydatetime())
    return v


def table(columns, rows):
    pa = _pa()
    fields = [pa.field(c['name'], arrow_type(c['type'])) for c in columns]
    arrays = [pa.array([to_arrow_value(r.get(c['name']), c['type']) for r in rows],
                       type=f.type)
              for c, f in zip(columns, fields)]
    return pa.Table.from_arrays(arrays, schema=pa.schema(fields))


def rows_of(tbl):
    return [{k: plain(v) for k, v in r.items()} for r in tbl.to_pylist()]


def columns_of(tbl):
    return [{'name': f.name, 'type': value_type(f.type)} for f in tbl.schema]


def encode(fmt, tbl):
    pa = _pa()
    sink = io.BytesIO()
    if fmt == 'parquet':
        import pyarrow.parquet as pq
        pq.write_table(tbl, sink)
    elif fmt == 'arrow':
        with pa.ipc.new_file(sink, tbl.schema) as w:
            w.write_table(tbl)
    else:
        raise ValueError(f"unknown format {fmt!r}: parquet or arrow")
    return sink.getvalue()


def decode(fmt, data):
    pa = _pa()
    if fmt == 'parquet':
        import pyarrow.parquet as pq
        return pq.read_table(io.BytesIO(data), use_threads=False)
    if fmt == 'arrow':
        return pa.ipc.open_file(pa.BufferReader(data)).read_all()
    raise ValueError(f"unknown format {fmt!r}: parquet or arrow")


def run(job):
    fmt = job.get('format')
    if job.get('op') == 'encode':
        data = encode(fmt, table(job['columns'], job.get('rows') or []))
        return {'ok': True, 'bytes': base64.b64encode(data).decode('ascii'),
                'rows': len(job.get('rows') or [])}
    if job.get('op') == 'decode':
        tbl = decode(fmt, base64.b64decode(job['bytes']))
        return {'ok': True, 'columns': columns_of(tbl), 'rows': rows_of(tbl)}
    raise ValueError(f"unknown op {job.get('op')!r}: encode or decode")


def main():
    try:
        out = run(json.load(sys.stdin))
    except ImportError as e:
        out = {'ok': False, 'error': f"pyarrow is not installed in this runtime: {e}"}
    except Exception as e:  # noqa: BLE001 — the host shows the message
        out = {'ok': False, 'error': f"{type(e).__name__}: {e}"}
    json.dump(out, sys.stdout)
    sys.stdout.write('\n')
