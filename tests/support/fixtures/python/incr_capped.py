from sutra_transforms import transform, Input, Output, previous_output, cap_batch

CAP = 2


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed_ai')},
    output=Output(kind='object-type', object_type='widget', primary_key='pk'),
    incremental=True)
def compute(seed):
    done = {r.get('pk') for r in previous_output()}
    pending = [r for r in seed if r['tid'] not in done]
    pending = cap_batch(pending, CAP)
    return [{'pk': r['tid'], 'label': r.get('label', '')} for r in pending]
