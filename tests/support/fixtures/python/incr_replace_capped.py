from sutra_transforms import transform, Input, Output, previous_output, cap_batch


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed_c')},
    output=Output(kind='object-type', object_type='widget_c', primary_key='pk'),
    incremental=True)
def compute(seed):
    done = {r.get('pk') for r in previous_output()}
    pending = cap_batch([r for r in seed if r['tid'] not in done], 2)
    return [{'pk': r['tid'], 'label': r.get('label', '')} for r in pending]
