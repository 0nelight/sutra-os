from sutra_transforms import transform, Input, Output


@transform(
    inputs={'things': Input(kind='object-type', object_type='thing')},
    output=Output(kind='object-type', object_type='summary', primary_key='pk'))
def compute(things):
    return [{'pk': 's-' + t['pk'], 'thing_pk': t['pk'],
             'shout': (t.get('label') or '').upper()} for t in things]
