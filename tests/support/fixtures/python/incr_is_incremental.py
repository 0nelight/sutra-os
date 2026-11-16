from sutra_transforms import transform, Input, Output, is_incremental


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed_i')},
    output=Output(kind='object-type', object_type='widget_i', primary_key='pk'),
    incremental=True)
def compute(seed):
    return [{'pk': r['tid'], 'label': str(is_incremental())} for r in seed]
