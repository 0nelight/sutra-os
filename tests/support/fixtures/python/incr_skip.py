from sutra_transforms import transform, Input, Output, previous_output


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed_ai')},
    output=Output(kind='object-type', object_type='thing', primary_key='pk'),
    incremental=True)
def compute(seed):
    done = {r.get('pk') for r in previous_output()}
    return [{'pk': r['tid'], 'label': r.get('label', '')}
            for r in seed if r['tid'] not in done]
