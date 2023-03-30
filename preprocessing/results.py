r"""°°°
# Imports
°°°"""
# |%%--%%| <uUaSDRA0JE|R0uMev4MXN>

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import json
import subprocess
import seaborn as sns
import seaborn.objects as so
from glob2 import glob

# |%%--%%| <R0uMev4MXN|c1MNKqC4U3>

import mir_eval as me

# |%%--%%| <c1MNKqC4U3|J21yZcGJ8K>

dcml_chordtype_map = {
    "M": "major",
    "m": "minor",
    "Mm7": "dominant-7th",
    "o": "diminished",
    "o7": "full-diminished",
    "mm7": "minor-7th",
    "%7": "half-diminished",
    "MM7": "major-7th",
    "+": "augmented",
    "mM7": "minor-major-7th",
    "+7": "augmented-7th",
}

# |%%--%%| <J21yZcGJ8K|eOMtSxwK0r>
r"""°°°
----
°°°"""
# |%%--%%| <eOMtSxwK0r|SwmnxgHh9U>
r"""°°°
## Loading the Results
Loads all json files from the given folde
°°°"""
# |%%--%%| <SwmnxgHh9U|nhz8HdcFcn>

cols = ['corpus', 'piece', 'algorithm', 'accuracy', 'likelihood', 'chordLabels', 'slices','runTime']

# |%%--%%| <nhz8HdcFcn|ki1WK68NM4>

def load_dataset(path):
    jsonFiles = glob(path + '/*/*.json') #Can be used absolute or relative paths 
    df = pd.DataFrame(columns=cols) 
    lbls = pd.DataFrame(columns=["piece", "corpus", "labels"])
    for jsonFile in jsonFiles:
        # df = pd.read_json(jsonFile)
        try:
            with open(jsonFile, 'r') as f:
                data = json.loads(f.read())
        except:
            print("Error with {}".format(jsonFile))
            continue

        #lbls.append(data['groundTruth'])
        newLbls = pd.DataFrame([{"corpus":data['corpus'], "piece":data["piece"], "labels": data["groundTruth"]}])
        lbls = pd.concat([lbls, newLbls])

        newdf = pd.json_normalize(data, record_path = ['results'], 
            meta = ['corpus', 'piece'])
        
        #newdf['groundTruth']= data['groundTruth']
        df = pd.concat([df, newdf])
    return (df.set_index(["corpus", "piece"]),lbls.set_index(["corpus", "piece"]))

# |%%--%%| <ki1WK68NM4|Zlt9gmLSzL>

(results,groundTruth) = load_dataset("outputs")

# |%%--%%| <Zlt9gmLSzL|wTl7iVzSrg>
r"""°°°
Print each parse piece
°°°"""
# |%%--%%| <wTl7iVzSrg|w3qsmdj3Sr>

print(" ".join(list(results.index.levels[1].unique())))

# |%%--%%| <w3qsmdj3Sr|ikUSCIh1xQ>

results.drop(["chordLabels", "slices"],axis=1)

# |%%--%%| <ikUSCIh1xQ|wRjsiHake7>

results.drop(["chordLabels", "slices"], axis=1).groupby(["algorithm"]).agg([np.mean, np.std])

# |%%--%%| <wRjsiHake7|tPrPjJmMrh>

agm = results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).agg([np.mean, np.std])

# |%%--%%| <tPrPjJmMrh|p8rgzh1nIc>

#results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).errors()

# |%%--%%| <p8rgzh1nIc|Fh8SZMhoFp>

agm = results.drop(["chordLabels", "slices","runTime","likelihood"], axis=1).groupby(["corpus", "algorithm"]).agg([np.std])

# |%%--%%| <Fh8SZMhoFp|Aqp1ek5FOh>

errs = agm.reset_index()["accuracy"]["std"].fillna(0)

# |%%--%%| <Aqp1ek5FOh|2GXcsaq29Q>

a = results.drop(["chordLabels", "slices"], axis=1)

# |%%--%%| <2GXcsaq29Q|sZfeCMfSsE>

std_pivot = pd.pivot_table(a, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.std)

# |%%--%%| <sZfeCMfSsE|VD9v6zwyaY>

acc_pivot = pd.pivot_table(a, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)

# |%%--%%| <VD9v6zwyaY|YLOzgEH98j>
r"""°°°
## Plot Accuracy Chart
°°°"""
# |%%--%%| <YLOzgEH98j|OYm9JPggH2>

sns.set()  # use Seaborn styles
acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,2), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
plt.ylabel('Accuracy')
plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})
          

# |%%--%%| <OYm9JPggH2|W54SpNUNr7>

data = [[30, 25, 50, 20],
[40, 23, 51, 17],
[35, 22, 45, 19]]
X = np.arange(4)
fig = plt.figure()
ax = fig.add_axes([0,0,1,1])
ax.bar(X + 0.00, data[0], color = 'b', width = 0.25)
ax.bar(X + 0.25, data[1], color = 'g', width = 0.25)
ax.bar(X + 0.50, data[2], color = 'r', width = 0.25)

# |%%--%%| <W54SpNUNr7|iHRFncNnP0>
r"""°°°
# Close Error Analysis
°°°"""
# |%%--%%| <iHRFncNnP0|GaYVd5PvVx>
r"""°°°
Attributes: chord type, root note, etc
°°°"""
# |%%--%%| <GaYVd5PvVx|Tul5KdAHKW>
r"""°°°
#### Zoom in on ABC: n02op18-2_03
°°°"""
# |%%--%%| <Tul5KdAHKW|qMafEUWNMJ>

groundTruth.loc['ABC','n02op18-2_03']

# |%%--%%| <qMafEUWNMJ|6np0Y2BmMq>

results

# |%%--%%| <6np0Y2BmMq|VMIfKwM0Tp>

ex = results.loc['schumann_kinderszenen','n04']

# |%%--%%| <VMIfKwM0Tp|pms7SOjJQq>

ex[["algorithm", "accuracy", "likelihood", "runTime"]]
