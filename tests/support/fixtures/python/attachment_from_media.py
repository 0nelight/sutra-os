from sutra_transforms import transform, Input, Output


@transform(
    inputs={'atts': Input(kind='media-set', dataset='raw_attachments')},
    output=Output(kind='object-type', object_type='attachment', primary_key='pk'))
def compute(atts):
    out = []
    for row in atts.rows():
        data = atts.bytes(row['media_ref'])
        out.append({'pk': row['filename'],
                    'filename': row['filename'],
                    'nbytes': len(data)})
    return out
