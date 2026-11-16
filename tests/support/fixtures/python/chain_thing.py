from sutra_transforms import transform, Input, Output


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed')},
    output=Output(kind='object-type', object_type='thing', primary_key='pk'))
def compute(seed):
    return [{'pk': r['tid'], 'label': r.get('label', '')} for r in seed]
