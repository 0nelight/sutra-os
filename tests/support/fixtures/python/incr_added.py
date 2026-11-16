from sutra_transforms import transform, Input, Output


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed_ai')},
    output=Output(kind='object-type', object_type='widget', primary_key='pk'),
    incremental=True)
def compute(seed):
    return [{'pk': r['tid'], 'label': r.get('label', '')} for r in seed]
