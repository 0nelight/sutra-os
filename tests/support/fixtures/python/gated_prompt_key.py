from sutra_transforms import (transform, Input, Output, cap_batch,
                              previous_output, prompt_hash, unprocessed)


@transform(
    inputs={'seed': Input(kind='dataset', dataset='seed_ai')},
    output=Output(kind='dataset', dataset='widget_out', primary_key='pk'),
    incremental={'read_mode': 'current', 'write_mode': 'modify'},
    semantic_version=1,
    done_key='seed.tid',
    max_per_run=2,
    prompt_key=['seed.label'])
def compute(seed):
    interesting = [r for r in seed if str(r.get('label', '')).startswith('A')]
    pending = cap_batch(unprocessed(interesting, previous_output()))
    return [{'pk': r['tid'], 'label': r['label'], 'prompt_hash': prompt_hash(r)}
            for r in pending]
