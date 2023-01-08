def get_chord_type(globalkey_is_minor, numeral, form :str, figbass :str, changes :str):
  """
  Parameters
  ----------
  globalkey_is_minor : :bool:
    True if the global key is minor
  numeral : :str:
    Simplified numeral: eg. i I V #III bvi 
  form : :str:
    Form can be empty or o for diminished, % for half diminished
  figbass : :str:
    Figured bass
  changes : :str:
    Chord changes, eg 4 97b4 74 6b4 b6b4 4+2 

  Suspensions should be ignored:
    I(4) can be treated as I, then the chords are merged, resulting in a more coarse grained analysis.
  Added tones should also be ignored:
    I(+6) is treated as I.

  A more sophisticated approach can be developed at a later stage that incorporates these added tones.
  
  Determine what the chord type is out of the following:
  "M", "m", "Mm7", "o", "o7", "mm7", "%7", "MM7", "+", "Ger", "It", "Fr", "mM7", "+7"
  """

  chord_types = ["M", "m", "Mm7", "o", "o7", "mm7", "%7", "MM7", "+", "Ger", "It", "Fr", "mM7", "+7"]
  
  

  numeral = numeral.strip("#b")

  if (numeral.isupper()):
    major = True
  else:
    major = False
  
  seventh = str(figbass).startswith("7")
  
  diminished = form == "o"

  half_diminished = form == "%"
  
  augmented = form == "+"

  # Note: This is very basic implementation
  # Does not handle Major 7th, Ger, It, mM7 or Fr
  if major:
    if seventh:
      if augmented:
        return "+7"
      else:
        return "Mm7"
    elif augmented:
      return "+"
    else:
      return "M"
  else:
    if diminished:
      if seventh:
        return "o7"
      else:
        return "o"
    elif half_diminished:
        return "%7"
    elif seventh:
      return "mm7"
    else:
      return "m"

"""
C    0
C#   7
D  2
Eb 9
E 4
F 11
F# 6
G 1 
G# 8
A 3
A# 10
B 5
C 0
"""


def get_chord_offset(numeral: str, globalkey_is_minor):
  alteration = (numeral.count("#") - numeral.count("b")) * 7

  numeral = numeral.strip("#b")
  numeral = numeral.upper()#
  numeral_to_interval_major = {"I": 0, "II": 2, "III": 4, "IV": 5, "V":1, "VI":3, "VII":5}
  numeral_to_interval_minor = {"I": 0, "II": 2, "III": 9, "IV": 5, "V":1, "VI":8, "VII":10}

  if globalkey_is_minor:
    return int((numeral_to_interval_minor[numeral] + alteration) % 7)
  else:
    return int((numeral_to_interval_major[numeral] + alteration) % 7)


def transform_chords_abs(df):
    """
    Transposes all numerals to their position in the global major or minor scale.
    This eliminates localkeys and relativeroots. The resulting chords are defined
    by [`numeral`, `figbass`, `changes`, `globalkey_is_minor`] (and `pedal`).

    Uses: :py:func:`transform`, :py:func:`rel2abs_key^, :py:func:`resolve_relative_keys` -> :py:func:`str_is_minor()`
    :py:func:`transpose_changes`, :py:func:`series_is_minor`,

    Parameters
    ----------
    df : :obj:`pandas.DataFrame`
        
    cols : :obj:`dict`, optional
        In case the column names for ``['numeral', 'figbass', 'changes', 'relativeroot', 'localkey', 'globalkey', 'globalkey_is_minor']`` deviate, pass a dict, such as

        .. code-block:: python

            {'chord':           'chord_col_name'
             'pedal':           'pedal_col_name',
             'numeral':         'numeral_col_name',
             'form':            'form_col_name',
             'figbass':         'figbass_col_name',
             'changes':         'changes_col_name',
             'relativeroot':    'relativeroot_col_name',
             'localkey':        'localkey_col_name',
             'globalkey':       'globalkey_col_name'}}


    Returns
    -------
    :obj:`pandas.DataFrame`
    """
    
    df['rootoffset'] = df.apply(lambda x: int(get_chord_offset(x.numeral,x.globalkey_is_minor)), axis = 1)
    df['chordtype'] = df.apply(lambda x: get_chord_type(x.globalkey_is_minor, x.numeral, x.form, x.figbass, x.changes), axis = 1)
