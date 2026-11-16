from sutra_transforms import transform, Input, Output, put_media_item


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed')},
    outputs={
        'raw_messages': Output(kind='dataset', dataset='raw_messages'),
        'raw_attachments': Output(kind='dataset', dataset='raw_attachments'),
    })
def produce(seed):
    msgs, atts = [], []
    for row in seed:
        mid = row['message_id']
        msgs.append({'message_id': mid, 'betreff': row.get('betreff', '')})
        for i in range(int(row.get('n_att', '1') or '1')):
            filename = f"{mid}-{i}.pdf"
            content = (f"%PDF-1.4 {mid} attachment {i}\n%%EOF\n").encode('utf-8')
            ref = put_media_item(content, mime='application/pdf')
            atts.append({'message_id': mid, 'filename': filename, 'media_ref': ref})
    return {'raw_messages': msgs, 'raw_attachments': atts}
