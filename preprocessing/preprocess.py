r"""°°°
# Imports
°°°"""
# |%%--%%| <nmn2Sinrnp|F7iRptmN8b>
r"""°°°
Dependencies:
ms3 == 1.2.3
dimcat==0.0.post1.dev122+gd1e90a1
This is installed from github main branch March 29th
setuptools=65.6.3

°°°"""
# |%%--%%| <F7iRptmN8b|bHPZ739zX3>

import numpy as np
import ms3 as ms
import dimcat as dc
import os

import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import pandas as pd
pd.options.mode.chained_assignment = None

# |%%--%%| <bHPZ739zX3|sZbKt7tuyW>
r"""°°°
# Load dataset
°°°"""
# |%%--%%| <sZbKt7tuyW|cAVB9NxDgH>

def createDataset(corpi):
    dataset = dc.Dataset()

    for corpus in corpi:
        dataset.load(corpus, parse_tsv=True, parse_scores=False) 

    return dataset

# |%%--%%| <cAVB9NxDgH|S1f4uM9ksc>

# Corpuses to analyse
corpi = [ "../../romantic_piano_corpus/grieg_lyric_pieces"
        , "../../ABC/"
        , "../../romantic_piano_corpus/schumann_kinderszenen"
        , "../../romantic_piano_corpus/chopin_mazurkas"
        , ]

# Initialise dataset
dataset = createDataset(corpi)

# |%%--%%| <S1f4uM9ksc|hQl82FAXdp>
r"""°°°
# Helper Functions

°°°"""
# |%%--%%| <hQl82FAXdp|7m7xrdkNyw>

def to_pitch(midi, tpc):
    pitch_class = ms.fifths2name(tpc)
    octave = str(midi // 12)
    return pitch_class + octave 

# Returns the offset using semitones
def get_chord_offset(numeral: str, globalkey_is_minor):
    try:
        alteration = (numeral.count("#") - numeral.count("b")) * 7
    except:
        ## NOTE TO SELF WARNING : BUG INDUCED
        ## sometimes numeral is just NAN. in which case i just return 0 as a hack.
        return 0
    #print(alteration)

    numeral = numeral.strip("#b")
    numeral = numeral.upper()
    #numeral_to_interval_major = {"I": 0, "II": 2, "III": 4, "IV": 5, "V":7, "VI":9, "VII":11}
    #numeral_to_interval_minor = {"I": 0, "II": 2, "III": 3, "IV": 5, "V":7, "VI":8, "VII":10}
    numeral_to_interval_major = {"I": 0, "II": 2, "III": 4, "IV": -1, "V":1, "VI":3, "VII":8}
    numeral_to_interval_minor = {"I": 0, "II": 2, "III": -3, "IV": -1, "V":1, "VI":8, "VII":-2}

    if globalkey_is_minor:
        return (numeral_to_interval_minor[numeral] + alteration) % 12
    else:
        return (numeral_to_interval_major[numeral] + alteration) % 12

def interval_union(i1,i2):
    return pd.Interval(i1.left,i2.right,'left')

def transform_chords_abs(df):
    df['rootoffset'] = df.apply(lambda x: int(get_chord_offset(x.numeral,x.globalkey_is_minor)), axis = 1)

# |%%--%%| <7m7xrdkNyw|ntpUNEOfIt>

# Returns two dataframes, one for the chords, one for the slices
# Splits the piece into slices, simplyifing chord labels.
def preprocessPiece(corpus : str, piece : str, labels, salami_notes):
    # zoom in on the chords in one piece
    chords = labels.loc[(corpus, piece)]

    # Translate labels to absolute pitches

    desired_chord_columns = ['chord','pedal','numeral','form','figbass','changes','relativeroot','localkey','globalkey']
    chordz = chords.copy().reset_index()
    chordz = chordz.reset_index()

    chords_abs_columns = ['chord', 'globalkey','globalkey_is_minor']

    clean_chords = chordz[chordz['chord'] != '@none']
    ms.labels2global_tonic(clean_chords, inplace=True)
    clean_chords.to_csv("chordsbefore.csv")
    transform_chords_abs(clean_chords)

    # Recombine the segments with @None labels
    full_chords_abs = pd.concat([clean_chords, chordz[chordz['chord'] == '@none']]).sort_index()
    full_chords_abs.rootoffset.fillna(0, inplace=True)

    # Now we merge repeated chords
    relavant_columns = ["interval", "chord_type", "rootoffset", "globalkey"]

    dfs = pd.DataFrame()
    ind = 0
    prev = None 
    for row in full_chords_abs[relavant_columns].iterrows():
        v = row[1]
        ii = len(dfs.index) - 1
        if prev and (v.chord_type == prev[1].chord_type and v.rootoffset == prev[1].rootoffset):
            dfs.at[ii, 'interval'] = pd.Interval(dfs.iloc[ii].interval.left, v.interval.right, "left")
        else:
            new_row = pd.DataFrame({'interval':row[1].interval, 'chord_type':v.chord_type,'rootoffset':v.rootoffset,'globalkey':v.globalkey},index=[ind])
            dfs = pd.concat([dfs, new_row])
            ind += 1
    prev = row

    dfs.rootoffset = dfs.rootoffset.astype(int)
    full_chords_abs = dfs

    relavant_columns = [ "interval", "chord_type", "rootoffset", "globalkey"]

    full_chords_abs = full_chords_abs.reset_index()[relavant_columns]
    full_chords_abs.index.name ='segment_id'
    full_chords_abs[["chord_type", "rootoffset", "globalkey"]].to_csv('chords.csv')


    salamis = salami_notes.loc[(corpus, piece)]

    mini_salamis = salamis[['midi','tpc','tied']]
    mini_salamis['tied'] = mini_salamis['tied'].fillna(0).astype('bool')

    # Assigning each slice a segment id according to the chord.
    dfs = []
    for segment, interval in enumerate(full_chords_abs["interval"]):
        segMask = mini_salamis.index.get_level_values(0).overlaps(interval)
        slicesInInterval = mini_salamis[segMask]
        slicesInInterval.insert(0,'segment_id',segment)
        dfs.append(slicesInInterval)

    segmented_salamis = pd.concat(dfs)

    segmented_salamis['slice_id'] = pd.factorize(segmented_salamis.reset_index()['onset_slice'])[0]

    segmented_salamis['pitch'] = segmented_salamis.apply(lambda x: to_pitch(x.midi, x.tpc), axis=1)

    final_salamis_columns = ['segment_id','slice_id','pitch','tied']
    final_salamis = segmented_salamis.reset_index()[final_salamis_columns]

    final_salamis["new_segment"] = final_salamis["segment_id"].diff().astype(bool)
    final_salamis['new_slice'] = final_salamis["slice_id"].diff().astype(bool)


    # Capitalise Global key to fix bug with Haskell Musicology with lowercase b
    full_chords_abs.globalkey = full_chords_abs.globalkey.apply(lambda key: key.capitalize())

    # Correct the new segment and new slice fields for the first row.
    final_salamis.at[0, "new_segment"] = False
    final_salamis.at[0, "new_slice"] = False

    final_salamis.to_csv('salamis.csv',columns=["new_segment", "new_slice", "pitch","tied"], index=False)

    return (full_chords_abs[["chord_type", "rootoffset", "globalkey"]], final_salamis[["new_segment", "new_slice", "pitch", "tied"]])

# |%%--%%| <ntpUNEOfIt|catvy1VO1i>

# Given a dataset, process all pieces and return labels and slices
def processDataset(dataset):
    # 3mins
    labels = dataset.get_facet("expanded")
    
    # Process slices 
    salami_crp = dc.NoteSlicer().process_data(dataset)
    salami_notes = salami_crp.get_facet("notes")

    corpi = labels.index.unique(0).tolist()
    for corpus in corpi:
        pieces = labels.loc[corpus].index.unique(0).tolist()
        for piece in pieces:
            (chords, slices) = preprocessPiece(corpus, piece, labels, salami_notes)
            # Create Folders if necessary
            if not os.path.isdir("inputs/chords/{}".format(corpus)):
                os.makedirs("inputs/chords/{}".format(corpus))
            if not os.path.isdir("inputs/slices/{}".format(corpus)):
                os.makedirs("inputs/slices/{}".format(corpus))

            chords.to_csv("inputs/chords/{}/{}.csv".format(corpus, piece))
            slices.to_csv("inputs/slices/{}/{}.csv".format(corpus, piece),index=False)
    
    return (labels, salami_notes)

# |%%--%%| <catvy1VO1i|rYAHoMQXiT>
r"""°°°
## Generate all input data
°°°"""
# |%%--%%| <rYAHoMQXiT|xvVXToEfKO>

processDataset(dataset)
