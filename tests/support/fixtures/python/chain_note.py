from sutra_transforms import transform, Input, Output


@transform(
    inputs={'things': Input(kind='object-type', object_type='thing')},
    output=Output(kind='object-type', object_type='note', primary_key='pk'))
def compute(things):
    return [{'pk': 'n-' + t['pk'], 'body': 'note:' + (t.get('label') or '')} for t in things]
