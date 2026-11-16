from sutra_transforms import transform, Input, Output


@transform(
    inputs={'msgs': Input(kind='dataset', dataset='raw_messages')},
    output=Output(kind='object-type', object_type='message', primary_key='pk'))
def compute(msgs):
    return [{'pk': m['message_id'], 'betreff': m.get('betreff', '')} for m in msgs]
