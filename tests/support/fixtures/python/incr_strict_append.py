from sutra_transforms import transform, Input, Output


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed_t')},
    output=Output(kind='object-type', object_type='widget_t', primary_key='pk'),
    incremental={'strict_append': True})
def compute(seed):
    return [{'pk': 'a' if r['tid'] == 'dup' else r['tid'], 'label': ''} for r in seed]
