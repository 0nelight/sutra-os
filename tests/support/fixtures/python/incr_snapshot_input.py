from sutra_transforms import transform, Input, Output, previous_output


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed_s')},
    output=Output(kind='object-type', object_type='widget_s', primary_key='pk'),
    incremental={'static_inputs': ['seed']})
def compute(seed):
    done = {r.get('pk') for r in previous_output()}
    return [{'pk': r['tid'], 'label': r.get('label', '')} for r in seed if r['tid'] not in done]
