from sutra_transforms import transform, Input, Output


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed')},
    outputs={'raw_messages': Output(kind='dataset', dataset='raw_messages')})
def produce(seed):
    msgs = []
    for row in seed:
        mid = row['message_id']
        msgs.append({'message_id': mid,
                     'betreff': row.get('betreff', ''),
                     'postfach': f"box-{mid}"})
    return {'raw_messages': msgs}
