# %% [markdown]
# # Imports

# %%
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import json
import subprocess
import os as os
import seaborn as sns
import seaborn.objects as so
import mir_eval
import collections as co
import pitchtypes as pt
import ms3
from matplotlib.colors import LogNorm, Normalize, LinearSegmentedColormap
from matplotlib.ticker import MaxNLocator
from glob2 import glob
from datetime import datetime
import pytz
from tqdm import tqdm
import random

# %% [markdown]
# # Set-up

# %% [markdown]
# ### Seaborn Plotting Settings

# %%
sns.set(rc={'text.usetex' : True, 'pgf.texsystem': 'pdflatex'},font_scale=1.3)

# %% [markdown]
# #### Chord Types

# %%
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
    "Ger": "",
    "It":"",
    "Fr":""
}
# Conversion from dcml standard to the mir_eval standard 
dcml_to_mir = {
    "M": "maj",
    "m": "min",
    "Mm7": "7",
    "o": "dim",
    "o7": "dim7",
    "mm7": "min7",
    "%7": "hdim7",
    "MM7": "maj7",
    "+": "aug",
    "mM7": "minmaj7",
    "+7": "aug7",
    "Ger": "",
}

# %% [markdown]
# # Loading Results

# %% [markdown]
# Scripts for loading all json files from the given folders

# %%
def get_experiments(experiment_name):
    cols = ['id', 'corpus', 'piece', 'algorithm', 'accuracy', 'likelihood', 'chordLabels', 'slices','runTime', 'reruns','groundTruth','iteration']
    df = pd.DataFrame(columns=cols) 
    lbls = pd.DataFrame(columns=["piece", "corpus", "labels"])
    
    path = "outputs"
    experiments, experiment_types = get_all_experiments(path)
    if experiment_name in experiment_types:
        print("Getting runs of {}".format(experiment_name))
    else:
        print("Can't find experiment {}".format(experiment_name))
        return 
    
    fails = [] 
    successes = 0
    lbl_list = []
    df_list = []
    for root, dirs, files in os.walk(path):
        for file in files:
            if experiment_name in file:
                try:
                    if file[6] == "-":
                        #ignore old format
                        continue
                    with open(os.path.join(root, file), 'r') as f:
                        data = json.loads(f.read())
                except:
                    fails.append(file)
                    continue
                
                successes += 1

                newLbls = pd.DataFrame([{"corpus":data['corpus'], "piece":data["piece"], "labels": data["groundTruth"]}])
                lbl_list.append(newLbls)

                newdf = pd.json_normalize(data, record_path = ['results'], 
                    meta = ['id','corpus', 'piece','algorithm'])
                df_list.append(newdf)
    df = pd.concat(df_list)
    lbls = pd.concat(lbl_list)
    
    df["experiment"] = df["id"].apply(lambda x: x.split("_")[1] if "_" in x else np.nan)
    df["id"] = pd.to_datetime(df["id"].apply(lambda x: x.split("_")[0] if "_" in x else np.nan))
    #df = df.dropna(subset=["experiment"])
    results = df.set_index(["id","corpus", "piece"])
    print ("Parsed {} runs succesfully, consisting of {} unique experiments with {} failed runs".format(successes, results.index.levels[0].nunique(), len(fails)))
    return (results,lbls.set_index(["corpus", "piece"]))

def get_all_experiments(path):
    experiments = set()
    experiment_types = set()
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith('.json'):
                if file[6] == "-":
                    continue
                experiments.add(file.split(".")[0])
                try:
                    experiment_types.add(file.split(".")[0].split("_")[1])
                except:
                    print(file)
    return experiments, experiment_types

def get_specific_experiment(experiment, time):
    (results, groundTruth) = get_experiments(experiment)
    print("Viewing latest result: {}".format(time))
    return (results.loc[results.index.get_level_values(0) == time], groundTruth)

def get_latest_experiment(experiment):
    (results, groundTruth) = get_experiments(experiment)
    latest_timestamp = results.index.get_level_values(0).max()
    print("Viewing latest result: {}".format(latest_timestamp))
    return (results.loc[results.index.get_level_values(0) == latest_timestamp], groundTruth)

# %% [markdown]
# # Pitches and Chord Labels

# %% [markdown]
# Pitch handling functions

# %% [markdown]
# Order of chord types for confusion heatmaps

# %%
chordtype_order_with_IR = ["IR"] + list(dcml_chordtype_map.keys())
chordtype_order = list(dcml_chordtype_map.keys())

# %%
def chordType(lbl):
    types = dcml_chordtype_map.keys()
    chord = ""
    for t in types: 
        if t in lbl and len(t) >len(chord):
            chord = t
    if chord == "":
        r = "No Chord"
    else:
        r = chord
    return r

def rootNote(lbl):
    types = dcml_chordtype_map.keys()
    chord = ""
    for t in types: 
        if t in lbl and len(t) >len(chord):
            chord = t
        
    return lbl.replace(chord,"")

def chordAndRoot(lbl):
    types = dcml_chordtype_map.keys()
    chord = ""
    for t in types: 
        if t in lbl and len(t) >len(chord):
            chord = t
    if chord == "":
        r = "No Chord"
    else:
        r = chord
    return (r, lbl.replace(chord,""))

def chordToMIR(lbl):
    (r,c) = chordAndRoot(lbl)
    if r == "No Chord":
        return c.replace("♯","#").replace("♯","#").replace("♭","b")
    else:
        return c.replace("♯","#").replace("♯","#").replace("♭","b") +":"+ dcml_to_mir[r]
    
def lof_distance(root,note):
    if root == "No Chord" or "NC" in root or note == "No Chord" or root == "C♯♯" or note == "C♯♯":
        return -14
    else:
        aa = pt.SpelledPitchClass(root)
        bb = pt.SpelledPitchClass(note)
        return (bb - aa).fifths()

# %% [markdown]
# # Evaluating Predictions
# A few scripts for computing chord-type and root note recall for predictions, as well as accuracy.

# %%
#takes two dataframes.
def label_confusion(pred,pged):
    predr = pred.reset_index()
    pgedr = pged.reset_index()
    inner_join = pd.merge(predr, 
                          pgedr, 
                          on ='piece', 
                          how ='right').drop_duplicates(subset= "piece")
    guesses = inner_join["chordLabels"]
    truth = inner_join["labels"]

    column_order = ["IR"] + list(dcml_chordtype_map.keys())
    row_order = list(dcml_chordtype_map.keys())

    return prediction_confusions(guesses,truth)

def prediction_confusions(guesses, truth):
    chordLabelCounts = co.Counter()
    chordLabelCounts[("IR","M")] = 0
    chordLabelCounts[("+7","M")] =0
    tp = co.Counter()
    fp = co.Counter()
    rootNoteCounts = co.Counter()
    for i in range(-14,15):
        for c in dcml_chordtype_map.keys():
            rootNoteCounts[(c,i)] = 0

    for (predLabels, trueLabels) in zip(guesses, truth):
        # guess is list of guesses 
        # Might be an L
        if type(predLabels) != list:
            continue
        # true is list of truths 
        for (prediction, t) in zip (predLabels, trueLabels):
            (pc,pr) = chordAndRoot(prediction)
            (tc,tr) = chordAndRoot(t)
            dist = lof_distance(chordToMIR(pr),chordToMIR(tr))
            if dist in range(-14,15): 
                rootNoteCounts[(tc, dist)] += 1
            if (pc,pr) == (tc,tr):
                tp[pc] += 1
                chordLabelCounts[(pc,tc)]+=1
            elif pr == tr:# correct root, incorrect type
                fp[pc] += 1
                chordLabelCounts[(pc,tc)]+=1
            elif pc == "" or tc == "": 
                chordLabelCounts[("No Chord",tc)]+=1
            else: # incorrect root
                chordLabelCounts[("IR",tc)]+=1
                fp[pc] += 1
                #incorrect[tr]+=1
    return (chordLabelCounts, rootNoteCounts)

# %% [markdown]
# # All Experiment

# %%
experiments, experiment_types = get_all_experiments("outputs")

# %%
experiment_types

# %% [markdown]
# # Plots

# %% [markdown]
# ## Prediction Heatmaps

# %%
def root_note_confusion_to_pivot_table(c1):
    counter_dfir = pd.DataFrame([(k[0], k[1], v) for k, v in c1.items()], columns=['True Chord Type', 'Semitones off', 'count'])
    ptir = pd.pivot_table(counter_dfir, values='count', index=['True Chord Type'], columns=['Semitones off'], aggfunc=sum, fill_value=0)
    ptir = ptir.reindex(chordtype_order,columns=list(range(-14,15))).fillna(0)
    ptir.columns = ['Correct' if x==0 else x for x in ptir.columns]
    ptir = ptir.div(ptir.sum(axis=1), axis=0).fillna(0).rename({"%7": "\%7"})
    return ptir
    

# %%
def chord_label_confusion_to_pivot_table(c0):
    counter_df = pd.DataFrame([(k[0], k[1], v) for k, v in c0.items()], columns=['predicted', 'true', 'count'])
    pivot_tablea = pd.pivot_table(counter_df, values='count', index=['true'], columns=['predicted'], aggfunc=sum, fill_value=0)
    pivot_table = pivot_tablea.reindex(chordtype_order, columns=chordtype_order_with_IR).fillna(0)
    unique_classes = list(set(dcml_chordtype_map.keys())) + ["No Chord", "IR"]    
    pivot_table_normalized = pivot_table.div(pivot_table.sum(axis=1), axis=0).fillna(0)
    pivot_table = pivot_table_normalized.rename({"%7": "\%7"})
    pivot_table.columns = ['\%7' if x=="%7" else x for x in pivot_table.columns]
    return pivot_table

# %%
def chordLabelHeatMap(pivot_table_normalized, path):
    fig, axes = plt.subplots(nrows=1, ncols=1)
    #plt.plot(figsize=(8,6))
    cmap = sns.color_palette("Blues", as_cmap=True)
    cmap.set_bad(color="black")
    linewidth = 0.1
    #lbls = pivot_table_normalized.applymap(lambda v: "" if False else '')
    lbls = pivot_table_normalized.applymap(lambda v: "{:.2f}".format(v) if v>=0.01 else '')

    a = sns.heatmap(pivot_table_normalized,
                cmap=cmap,
                ax = axes,
                square=True,
                vmin=0,
                fmt='',
                vmax=1,
                linewidth=linewidth,
                linecolor="lightgray")
        
    axes.set_ylabel("True Chord-type", fontsize=18)
    axes.set_xlabel("Predicted Chord-type", fontsize=18)
    axes.set_xticklabels(axes.get_xticklabels(), rotation=45)
    for lbl in axes.get_xticklabels():
        if lbl.get_text() == "IR":  
            lbl.set_color("red")
    
    axes.collections[0].colorbar.set_label("Recall")

    fig.tight_layout()
    axes.set_title("Chord-type Recall", fontsize=22)
    #plt.gca().set_axis_off()
    plt.subplots_adjust(top = 1, bottom = 0, right = 1, left = 0, 
            hspace = 0, wspace = 0)
    plt.margins(0,0)
    #plt.gca().xaxis.set_major_locator(plt.NullLocator())
    #plt.gca().yaxis.set_major_locator(plt.NullLocator())
    plt.savefig(path + '/chordLabelHeatMap.pdf',bbox_inches='tight', pad_inches = 0)
    

# %%
def rootNoteHeatMap(ptir, path):
    fig, axes = plt.subplots(nrows=1, ncols=1)
    #plt.plot(figsize=(8,6))
    cmap = sns.color_palette("Blues", as_cmap=True)
    cmap.set_bad(color="black")
    linewidth = 0.1
    #lbls = pivot_table_normalized.applymap(lambda v: "" if False else '')
    rootLabels = ptir.applymap(lambda v: "{:.2f}".format(v) if v>=0.01 else '')
    
    b = sns.heatmap(ptir,
                cmap=cmap,
                square=True,
                ax = axes,
                vmin=0,
                vmax=1,
                fmt='',
                linewidth=linewidth,
                linecolor="lightgray")

    axes.set_ylabel("True Chord-type", fontsize=18)
    axes.set_xlabel("Root-note Prediction Error (Line-of-fifths)", fontsize=18)
    plt.gca().set_aspect('equal')
    axes.set_yticklabels(axes.get_yticklabels(), rotation=0)
    axes.set_xticklabels(axes.get_xticklabels(), rotation=0)

    
    axes.collections[0].colorbar.set_label("Recall",fontsize=18)

    fig.tight_layout()
    axes.set_title("Root-note Recall for each Chord-type", fontsize=18)
    #plt.gca().set_axis_off()
    plt.subplots_adjust(top = 1, bottom = 0, right = 1, left = 0, 
            hspace = 0, wspace = 0)
    plt.margins(0,0)
    fig.set_figwidth(9)
    #plt.gca().xaxis.set_major_locator(plt.NullLocator())
    #plt.gca().yaxis.set_major_locator(plt.NullLocator())
    plt.savefig(path + '/rootNoteHeatMap.pdf',bbox_inches='tight', pad_inches = 0)

# %%
def plot_accuracies(pred, pged, path):
    if not os.path.exists(path):
        os.makedirs(path)
    (ct_confusion, rn_confusion) = label_confusion(pred,pged)
    ct_table = chord_label_confusion_to_pivot_table(ct_confusion)
    rn_table = root_note_confusion_to_pivot_table(rn_confusion)
    rootNoteHeatMap(rn_table, path)
    chordLabelHeatMap(ct_table, path)

# %% [markdown]
# ## Latex Prediction Tables

# %% [markdown]
# ### Show 3 forms of accuracies in table

# %%
(res,resg) = get_latest_experiment("testRandomWal")

# %%
res.groupby(["piece"]).mean()

# %%
plot_mean = 3
min_num = 30
plot1 = np.random.normal (plot_mean, 1, size = min_num)
plot2 = np.random.normal (plot_mean, 1, size = min_num)
plot3 = np.random.normal (plot_mean, 1, size = min_num)
plt.figure (figsize = (5, 4))
seaborn_plot = plt.axes (projection='3d')
print (type (seaborn_plot))
seaborn_plot.scatter3D (plot1, plot2, plot3)
seaborn_plot.set_xlabel ('x')
seaborn_plot.set_ylabel ('y')
seaborn_plot.set_zlabel ('z')
plt.show ()

# %%


# %%
def root_note_confusion_to_pivot_table_raw(c1):
    counter_dfir = pd.DataFrame([(k[0], k[1], v) for k, v in c1.items()], columns=['True Chord Type', 'Semitones off', 'count'])
    ptir = pd.pivot_table(counter_dfir, values='count', index=['True Chord Type'], columns=['Semitones off'], aggfunc=sum, fill_value=0)
    ptir = ptir.reindex(chordtype_order,columns=list(range(-14,15))).fillna(0)
    ptir.columns = ['Correct' if x==0 else x for x in ptir.columns]
    #ptir = ptir.div(ptir.sum(axis=1), axis=0).fillna(0).rename({"%7": "\%7"})
    return ptir
    

# %%
def chord_label_confusion_to_pivot_table_raw(c0):
    counter_df = pd.DataFrame([(k[0], k[1], v) for k, v in c0.items()], columns=['predicted', 'true', 'count'])
    pivot_tablea = pd.pivot_table(counter_df, values='count', index=['true'], columns=['predicted'], aggfunc=sum, fill_value=0)
    pivot_table = pivot_tablea.reindex(chordtype_order, columns=chordtype_order_with_IR).fillna(0)
    unique_classes = list(set(dcml_chordtype_map.keys())) + ["No Chord", "IR"]    
    #pivot_table_normalized = pivot_table.div(pivot_table.sum(axis=1), axis=0).fillna(0)
    #pivot_table = pivot_table_normalized.rename({"%7": "\%7"})
    pivot_table.columns = ['\%7' if x=="%7" else x for x in pivot_table.columns]
    return pivot_table

# %%
def go_root(row,pred,truth):
    (ct_confusion, rn_confusion) = label_confusion(pred.loc[pred["lmao"] == row],truth)
    rn_pt = root_note_confusion_to_pivot_table_raw (rn_confusion)
    ct_pt = chord_label_confusion_to_pivot_table_raw(ct_confusion)
    rn_acc = (rn_pt.sum()["Correct"] + rn_pt.sum()[12]+ rn_pt.sum()[-12]) / rn_pt.sum().sum()
    #ct_acc = sum([v if k[0] == k[1] else 0 for k, v in ct_confusion.items()]) / sum([v for k, v in ct_confusion.items()])
    return rn_acc

def go_chord_type(row,pred,truth):
    (ct_confusion, rn_confusion) = label_confusion(pred.loc[pred["lmao"] == row],truth)
    rn_pt = root_note_confusion_to_pivot_table_raw (rn_confusion)
    ct_pt = chord_label_confusion_to_pivot_table_raw(ct_confusion)
    #rn_acc = (rn_pt.sum()["Correct"] + rn_pt.sum()[12]+ rn_pt.sum()[-12]) / rn_pt.sum().sum()
    ct_acc = sum([v if k[0] == k[1] else 0 for k, v in ct_confusion.items()]) 
    if ct_acc == 0: 
        return None 
    else: 
        return ct_acc / sum([v for k, v in ct_confusion.items()])

    

def accuracy_table(pred, truth):
    # SELECT ONLY PARSEABLE PIECE
    
    pred = pred.reset_index().rename_axis("lmao").reset_index()
    
    
    pred= pred.loc[pred["piece"].isin(["BI16-2", "op43n04", "n13", "n04","op43n04","op12n01"])]

    pred["Root Recall"] = pred["lmao"].apply(lambda row: go_root(row,pred,truth))
    pred["Chord-type Recall"] = pred["lmao"].apply(lambda row: go_chord_type(row,pred,truth))
    ## Calculate accuracies for each piece
    #for piece in pred.reset_index().piece.unique():
       # for iteration in pred.reset_index().iteration.unique():
            
            #print(piece)
            #print(rn_acc)
            #print(ct_acc)(ct_confusion, rn_confusion) = label_confusion(pred.loc[pred["piece"] == piece],resg)
            
    agm = pred.drop(["chordLabels", "slices","reruns","runTime","likelihood","lmao"], axis=1).groupby(["corpus","iteration","algorithm"]).agg([np.mean])
    agm = agm.groupby(["corpus","algorithm"]).agg([np.mean,np.std])
    agm = agm.rename({"ABC":"Beethoven", "chopin_mazurkas":"Chopin", "grieg_lyric_pieces":"Grieg","schumann_kinderszenen":"Schumann"})
    agm = agm.rename({"DualStochasticBeamSearch_18_65":"Stochastic Beam Search", "RandomWalk":"Random Parse"})
    agm = agm.rename(columns={"corpus": "Data-set", "algorithm": "Algorithm", "mean":"Mean", "accuracy":"Accuracy"})
    
    agm['accuracy'] = agm.apply(lambda row: "${:.2f} \pm {:.2f}$".format(row['Accuracy']["Mean"]["Mean"],row['Accuracy']["Mean"]['std']), axis=1)
    agm['root Recall'] = agm.apply(lambda row: "${:.2f} \pm {:.2f}$".format(row['Root Recall']["Mean"]["Mean"],row['Root Recall']["Mean"]['std']), axis=1)
    #agm['chord-type Recall'] = agm.apply(lambda row: "${:.2f} \pm {:.2f}$".format(row['Chord-type Recall']["Mean"]["Mean"],row['Chord-type Recall']["Mean"]['std']), axis=1)
    
    #agm = agm.reset_index().drop(["Accuracy","Root Recall", "Chord-type Recall"])
    
    agm = agm.droplevel(2, axis='columns')
    #print(agm)

    print(agm[["accuracy","root Recall"]].to_latex(
                  formatters={"corpus": str.upper, "piece": str.upper},
                  float_format="{:.2f}".format, escape=False

    )) 
    return agm
    #print(ct_confusion)
    #print(rn_confusion)
    

# %%
a = accuracy_table(res.loc[res["algorithm"].isin(["DualStochasticBeamSearch_18_65","RandomWalk", "RandomReduction"])],resg)

# %%
#plot_accuracies(rest,pged,"plots/heuristic")

# %%
#plot_accuracies(resr,pged,"plots/heuristic")

# %% [markdown]
# # PERFECT REDUCTION

# %%


# %% [markdown]
# Results from the perfect reduction experiment

# %%
(predt,pged) = get_latest_experiment("perfectvstemplating")

# %%
pred = predt.loc[predt["algorithm"] == "PerfectReduction"]
#pred = predt.loc[predt["algorithm"] == "Templating"]


# %%
pa = accuracy_table(predt,pged)

# %%
pa

# %%
plot_accuracies(predt,pged,"plots/informed")

# %% [markdown]
# ## PREDICTED VS TRUE

# %%
(c0, c1) = labelConfusion(pred, pged)

# %%
ss = 0
t = 0
for ((r,c),o) in c1.items():
    if (r in ["+7","mM7","Ger","It","Fr","+"]):
        ss += o
    t +=o
print(ss/t)

# %% [markdown]
# ## Truncate root note error from -14 to 14

# %% [markdown]
# ## PERFECT REDUCTION: Confusion Matrix

# %%
## Correlation

# %%
x = pred.reset_index()

# %%
x = x[x["likelihood"]>-20]

# %%
x["likelihood"].min().min()

# %%
sns.lmplot(data=x,x='accuracy',y='likelihood', col='corpus')
plt.ylim(-20, 0)

# %%
#fig, ax = plt.subplots(1, 1, figsize=(9,3))

cmap = sns.color_palette("viridis", as_cmap=True)
cmap.set_bad(color="black")

axx = sns.lmplot(x='accuracy',y='likelihood', data=x, fit_reg=True)
#sns.scatterplot(data=pred, x="accuracy", y="likelihood")

plt.xlabel('Accuracy')
plt.ylabel('Average Segment Log Likelihood')
#a[0].set_xlabel('Beam Width')
#a[0].set_ylabel('Reservoir Size')


plt.tight_layout()
plt.savefig('plots/perfectAccuracyChordType.png', dpi=180)

# %% [markdown]
# ## Group by algorithm for each corpus

# %%
agm = predt.drop(["chordLabels", "slices","reruns","runTime","segmentTimes","iteration","likelihood"], axis=1).groupby(["corpus", "algorithm"]).agg([np.mean,np.std])
agm = agm.rename({"ABC":"Beethoven", "chopin_mazurkas":"Chopin", "grieg_lyric_pieces":"Grieg","schumann_kinderszenen":"Schumann"})
agm = agm.rename(columns={"corpus": "Data-set", "algorithm": "Algorithm", "mean":"Mean", "accuracy":"Accuracy"})

# %%


# %%
print(agm.to_latex(
                  formatters={"name": str.upper},

                  float_format="{:.2f}".format,

))  

# %%
agm.to_latex(formatters={"name": str.upper},float_format="{:.0f}".format)

# %%


# %% [markdown]
# ## Corellation between Accuracy and Likelihood

# %% [markdown]
# ## Analyse Accuracy By ChordType

# %%


# %% [markdown]
# # Test On Just a few

# %%
res.loc[res["accuracy"].isna()]

# %% [markdown]
# # Extension accuracy Experiment

# %%


# %%
(res,resg) = get_latest_experiment("extensionExp")

# %%
ea = accuracy_table(res,pged)

# %%
ea[["accuracy","root Recall"]]

# %% [markdown]
# ## Print 

# %%
resr = res.loc[res["algorithm"].isin(["RandomWalk","DualStochasticBeamSearch_18_65"])].reset_index()

# %%
#resr

# %%
r = res.reset_index().groupby(["piece","algorithm"]).mean().reset_index()

# %%
resr= res.reset_index()

# %%
fig, axes = plt.subplots(nrows=1, ncols=1,figsize=(29,7))

g = sns.violinplot( x='piece', y='accuracy',hue='algorithm', data=res.reset_index())

# %%
resr["corpus"].unique()

# %%
#fig, ax = plt.subplots(nrows=1, ncols=1,figsize=(29,7))
fig, ax = plt.subplots(1, 3, figsize=(20,7), sharey=True)

cmap = sns.color_palette("viridis", as_cmap=True)
cmres = resr.loc[resr["corpus"]=="chopin_mazurkas"]
glres = resr.loc[resr["corpus"]=="grieg_lyric_pieces"]
skres = resr.loc[resr["corpus"]=="schumann_kinderszenen"]

c = sns.violinplot(ax=ax[0], data=cmres, x='piece', pallete=cmap, y='accuracy',hue='algorithm', split=True)
g = sns.violinplot(ax=ax[1], data=glres, x='piece', pallete=cmap, y='accuracy',hue='algorithm', split=True)
s = sns.violinplot(ax=ax[2], data=skres, x='piece', pallete=cmap, y='accuracy',hue='algorithm', split=True)

ax[0]
#ax.set_ylabel("Prediction Accuracy")
#ax.get_legend().set_title("Algorithm")
#ax.set_xlabel("Piece")
fig.suptitle("Extension vs. Baseline: Prediction accuracy of Random-Parse vs. Stochastic Beam Search")
plt.tight_layout()
plt.savefig('plots/extVsBaseline.png', dpi=180)


# %%
res.to_csv("smth.csv")

# %% [markdown]
# # Length of piece scaling

# %%
(res,resg) = get_latest_experiment("extlengthpiece")

# %%
res = res.loc[res["runTime"] < 1300]

# %%
res["length"] = res["slices"].apply(lambda slices: 0 if slices == None else len(slices))
res["maximalSlice"] = res["slices"].apply(lambda slices: 0 if slices == None else len(max(slices, key=len)))

# %%
fig, axes = plt.subplots(nrows=1, ncols=2,sharey=True)


a = sns.regplot(ax= axes[0], data=res, x="length",y="runTime",fit_reg=True,color="royalblue")

b = sns.regplot(ax= axes[1],data=res, x="maximalSlice",y="runTime", fit_reg=True,color="royalblue")

axes[0].set_ylabel("Run-time ($s$)", fontsize=18)
axes[0].set_xlabel("Number of Slices in Piece ($n$)", fontsize=18)
axes[1].set_xlabel("Number of Notes in Maximal Size Slice ($m$)", fontsize=18)
axes[1].set_ylabel("")

fig.tight_layout()
#axes.set_title("Root-note Recall for each Chord-type", fontsize=18)
#plt.gca().set_axis_off()
#plt.subplots_adjust(top = 1, bottom = 0, right = 1, left = 0, 
#        hspace = 0, wspace = 0)
#plt.margins(0,0)
fig.set_figwidth(14)
fig.set_figheight(4)

#plt.gca().xaxis.set_major_locator(plt.NullLocator())
#plt.gca().yaxis.set_major_locator(plt.NullLocator())
plt.savefig('plots/heuristic/scaling.pdf',bbox_inches='tight', pad_inches = 0)

# %% [markdown]
# # Random walk all pieces
# Let's look at all experiments that have been run

# %%
(results, groundTruth) = get_experiments("parseable")

# %%
results.loc[results["accuracy"].notna()].to_csv("resres.csv")

# %%
results

# %%
(results,pg) = get_latest_experiment("parseable")

# %%
results.to_csv("test.csv")

# %% [markdown]
# ## Number of Pieces in Total

# %%
results.reset_index()["piece"].nunique()

# %% [markdown]
# ## Number of Pieces that failed to parse

# %%
results.loc[results.accuracy.isna()].reset_index()["piece"].nunique()

# %%
succ = results.loc[results.accuracy.notna()].reset_index().drop_duplicates(subset=["piece"]).sort_values(by="runTime")

# %% [markdown]
# ## Number of Pieces that parsed succesfully

# %%
succ["piece"].nunique()

# %%
succ.corr()

# %%
succ[["corpus","piece"]].to_csv("successful parses.csv")

# %%
s = succ[["corpus","piece"]]

# %%
pairs = list(map(tuple, s[['corpus', 'piece']].values.tolist()))

# %%
#pairs

# %%
#s.values.tolist()

# %%
succ = succ.head(150)

# %%
succg = succ.groupby(["corpus", "algorithm"])

# %% [markdown]
# # Baseline Experiments

# %%
(res,gt) = get_latest_experiment("baselineExp")

# %%
r = res.reset_index().groupby(["algorithm","corpus","iteration"]).mean().reset_index()

# %%
r['loglikelihood']=r['likelihood']

# %%
r['likelihood'] = np.exp(r['loglikelihood'])

# %%
fig, axs = plt.subplots(ncols=2)

sns.barplot(ax= axs[1],x='corpus', y='likelihood',hue='algorithm', data=r, log=True)
sns.barplot(ax= axs[0], x='corpus', y='accuracy',hue='algorithm', data=r)

axs[0].set(ylim=(0, 1))
axs[0].set_title('Average Accuracy for each Dataset')
axs[0].set_xlabel("Dataset")
axs[0].set_ylabel("Average Accuracy")

axs[1].set_title('Average Likelihood for each Dataset')
axs[1].set_xlabel("Dataset")
axs[1].set_ylabel("Average Likelihood")

plt.tight_layout()
fig.set_figwidth(14)
plt.savefig("plots/baselineExp.pgf")
plt.savefig("plots/baselineExp.pdf")

#plt.ylim(reversed(plt.ylim()))

# %%


# %%
acc_std_pivot = pd.pivot_table(res, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.std)
lik_std_pivot = pd.pivot_table(res, 
                          values="likelihood", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.std)
acc_pivot = pd.pivot_table(res, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)
lik_pivot = pd.pivot_table(res, 
                          values="likelihood", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)

# %%
r = res.reset_index()

# %%
# Create two subplots side by side
fig, axs = plt.subplots(ncols=2, figsize=(20,8))

sns.set()  # use Seaborn styles

# First plot for accuracy_mean
axs[0].set_title('Accuracy Mean', fontsize=16)
acc_pivot.plot.bar(ax=axs[0], yerr=acc_std_pivot, fontsize=12, rot=0, ylim=(0,1), width=0.8, edgecolor='black', capsize=5, error_kw=dict(lw=1, capsize=3, capthick=1), legend="loc:lower center")
axs[0].set_xlabel("Corpus", fontsize=14)
axs[0].set_ylabel("Accuracy Mean", fontsize=14)

# Second plot for likelihood_mean
axs[1].set_title('Log Likelihood Mean', fontsize=16)
lik_pivot.plot.bar(ax=axs[1], yerr=lik_std_pivot, fontsize=12, rot=0, ylim=(-10,-50), width=0.8, edgecolor='black', capsize=5, error_kw=dict(lw=1, capsize=3, capthick=1), legend="loc:lower center")
axs[1].set_xlabel("Corpus", fontsize=14)
axs[1].set_ylabel("Log Likelihood Mean", fontsize=14)

plt.savefig("plots/baselineResults",dpi=180)


# %%
res.loc[res["accuracy"].notna()].head()

# %%
#parsed

# %%
r = {"grieg_lyric_pieces": ["op12n01","op38n07", "op47n04", "op71n03", "op54n06"]
        ,"chopin_mazurkas": ["BI168op68-4", "BI89-3op24-3"]
        ,"schumann_kinderszenen": ["n03", "n04", "n13"]
        ,"ABC": ["n14op131_06", "n14op131_03"]}

# %%
pairs= []
for corpus in r.keys():
    for piece in r[corpus]:
        pairs.append((corpus,piece))

# %%
pairs

# %%
a = parsed.reset_index()

# %%
#a.sort_values(by=["runTime"]).head(50)[["corpus","piece","accuracy","runTime"]]

# %%
a.piece.unique()

# %%
(ppr,ppg) = get_latest_experiment("exp")

# %%
ppr = ppr.loc[ppr.accuracy.notna()]

# %%
ppr

# %%
ppr.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).count()

# %%
agm = ppr.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm","piece"]).agg([np.mean])
#agm2 = agm.groupby(["corpus","algorithm"]).agg(np.mean)
#errs = agm.reset_index()["accuracy"]["std"].fillna(0)
#a = results.drop(["chordLabels", "slices"], axis=1)
#std_pivot = pd.pivot_table(agm, 
#                          values="accuracy", 
#                          index="corpus", 
#                          columns="algorithm", 
#                          aggfunc=np.std)
acc_pivot = pd.pivot_table(agm, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)
lik_pivot = pd.pivot_table(agm, 
                          values="likelihood", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)

# %% [markdown]
# ## Plot Accuracy Chart

# %%
ratio = 1
height = 5

# %%
lik_pivot.max().max()

# %%
lik_pivot


# %%
#fig, ax = plt.subplots(1, 2, figsize=((height*2)/ratio+ spacing,height),gridspec_kw={'width_ratios': [1,1]})
#lik_avgs.min().min(), lik_avgs.max().max()+ (lik_avgs.max().max()-lik_avgs.min().min()

sns.set()  # use Seaborn styles
#acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,2), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
r = acc_pivot.plot.bar( figsize=(8,6), fontsize=8, rot=0, ylim=(0,1), width=1.2, legend="loc:lower center")
l = lik_pivot.plot.bar(yerr=std_pivot,  figsize=(6,6), fontsize=8, rot=0, ylim=(lik_pivot.min().min(),lik_pivot.max().max()), width=0.3, legend="loc:lower center")

plt.ylabel('Accuracy')
plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})
          

# %% [markdown]
# # Extention: Dual Stochastic Beam Search Parameters

# %% [markdown]
# ## Run time experiments

# %% [markdown]
#  Extract parameters from algorithm name

# %%
(resultsb, labels) = get_latest_experiment("extensionBeamwidth")
resultsb = resultsb.reset_index()
resultsb['beamwidth'] = resultsb["algorithm"].apply(lambda s: int(s.split("_")[1]))
resultsb['reservoirsize'] = resultsb["algorithm"].apply(lambda s: int(s.split("_")[2]))

# %%
(resultsr, labels) = get_latest_experiment("extensionReservoir")
resultsr['beamwidth'] = resultsr["algorithm"].apply(lambda s: int(s.split("_")[1]))
resultsr['reservoirsize'] = resultsr["algorithm"].apply(lambda s: int(s.split("_")[2]))

# %%
(results, labels) = get_latest_experiment("extensionParamsFine")
results['beamwidth'] = results["algorithm"].apply(lambda s: int(s.split("_")[1]))
results['reservoirsize'] = results["algorithm"].apply(lambda s: int(s.split("_")[2]))
resultsbw= results.loc[results["beamwidth"].isin([2,8,16])]
resultsrs= results.loc[results["reservoirsize"].isin([20,80,135])]


# %%
resultsc = pd.concat([resultsr,resultsb,results])

# %%


# %%
fig, ax = plt.subplots(1, 2, figsize=(16,4))

sns.scatterplot(ax = ax[0], data=resultsbw.reset_index(), x="reservoirsize",y="runTime", hue="beamwidth")
sns.scatterplot(ax = ax[1], data=resultsrs.reset_index() x="beamwidth",y="runTime", hue="reservoirsize")
#sns.violinplot(ax = ax[2], data=results.reset_index(), x="beamwidth",y="accuracy", hue="reservoirsize")
ax[0].set_ylabel("Run-time (s)")
ax[0].get_legend().set_title("Beam Width")
ax[0].set_xlabel("Reservoir Size")
ax[1].set_xlabel("Reservoir Width")
ax[1].get_legend().set_title("Reservoir Width")
fig.suptitle("Effect of Hyperparameters on Run-time")

plt.show()

# %%
fig, ax = plt.subplots(1, 2, figsize=(16,4), sharey=True)

r = sns.lineplot(ax = ax[0], data=resultsbw.reset_index(), x="reservoirsize",y="runTime", hue="beamwidth")
b = sns.lineplot(ax = ax[1], data=resultsrs.reset_index(), x="beamwidth",y="runTime", hue="reservoirsize")
#sns.violinplot(ax = ax[2], data=results.reset_index(), x="beamwidth",y="accuracy", hue="reservoirsize")

ax[0].set_ylabel("Run-time (s)")
ax[0].set_xlabel("Reservoir Size")
ax[1].set_xlabel("Beam Width")
fig.suptitle("Effect of Reservoir Size and Beam Width on Run-time")



plt.show()

# %% [markdown]
# ## Accuracy, loglikelihood and runtime

# %%
exp_id = results.index.get_level_values(0)[0].strftime('%Y%m%d%H%M%S')

# %% [markdown]
# ## Plot hyperparemter results

# %%
def create_heatmap(xs, ax, vmin, vmax,cmap):
    lbls = xs.applymap(lambda v: "{:.2f}".format(v) if v in [np.nanmax(xs.values),np.nanmin(xs.values)]else '')
    return sns.heatmap(xs,
            #square=True,
            ax=ax,
            cmap=cmap,
            fmt='',
            annot=lbls, # Label the maximum value
            annot_kws={'fontsize':5},
            mask=xs.isna(),
            vmax=vmax,
            vmin=vmin,
            linewidth=0.000)

# %%
results.corr()

# %%
results['beamwidth'] = results["algorithm"].apply(lambda s: int(s.split("_")[1]))
results['reservoirsize'] = results["algorithm"].apply(lambda s: int(s.split("_")[2]))

def get_avgs(df, value):
    g = df.dropna(subset=value).groupby(['beamwidth','reservoirsize'])
    df_means = g[value].mean()
    return df_means.loc[:,:].reset_index().pivot(index='reservoirsize', columns='beamwidth', values=value)

acc_avgs = get_avgs(results, 'accuracy')
lik_avgs = get_avgs(results, 'likelihood')
lik_avgs = np.exp(lik_avgs)
run_avgs = get_avgs(results, 'runTime')

# %%
fig, ax = plt.subplots(1, 2, figsize=(30,6), sharey=True)
cmap = sns.color_palette("flare", as_cmap=True)
#sns.swarmplot(ax = ax[0], data=resultsbw.reset_index(),size=1, x="reservoirsize",y="runTime", hue="beamwidth")
sns.swarmplot(ax = ax[0], palette = cmap, data=resultsbw.reset_index(),linewidth=0.1,size=3.05, x="reservoirsize",y="runTime", hue="beamwidth")
sns.swarmplot(ax = ax[1], palette=cmap, data=resultsrs.reset_index(),linewidth=0.1,size=3.05, x="beamwidth",y="runTime", hue="reservoirsize")
#sns.violinplot(ax = ax[0], data=resultsbw.reset_index(),linewidth=0.1, x="reservoirsize",y="runTime", hue="beamwidth")
#sns.violinplot(ax = ax[1], data=resultsrs.reset_index(),linewidth=0.1, x="beamwidth",y="runTime", hue="reservoirsize")
#sns.violinplot(ax = ax[2], data=results.reset_index(), x="beamwidth",y="accuracy", hue="reservoirsize")
ax[0].set_ylabel("Run-time (s)")
ax[0].get_legend().set_title("Beam Width")
ax[0].set_xlabel("Reservoir Size")
ax[1].set_xlabel("Beam Width")
ax[1].get_legend().set_title("Reservoir Width")
fig.suptitle("Effect of Hyperparameters on Run-time")
plt.tight_layout()
plt.savefig('plots/extParamsRuntime.png', dpi=180)

# %%
fig, ax = plt.subplots(1, 2, figsize=(30,6), sharey=True)
cmap = sns.color_palette("flare", as_cmap=True)
#sns.swarmplot(ax = ax[0], data=resultsbw.reset_index(),size=1, x="reservoirsize",y="runTime", hue="beamwidth")
sns.boxplot(ax = ax[0],  data=resultsbw.reset_index(),linewidth=0.1, x="reservoirsize",y="accuracy", hue="beamwidth")
sns.boxplot(ax = ax[1],  data=resultsrs.reset_index(),linewidth=0.1, x="beamwidth",y="accuracy", hue="reservoirsize")
#sns.violinplot(ax = ax[0], data=resultsbw.reset_index(),linewidth=0.1, x="reservoirsize",y="runTime", hue="beamwidth")
#sns.violinplot(ax = ax[1], data=resultsrs.reset_index(),linewidth=0.1, x="beamwidth",y="runTime", hue="reservoirsize")
#sns.violinplot(ax = ax[2], data=results.reset_index(), x="beamwidth",y="accuracy", hue="reservoirsize")
ax[0].set_ylabel("Run-time (s)")
ax[0].get_legend().set_title("Beam Width")
ax[0].set_xlabel("Reservoir Size")
ax[1].set_xlabel("Beam Width")
ax[1].get_legend().set_title("Reservoir Width")
fig.suptitle("Effect of Hyperparameters on Run-time")
plt.tight_layout()

plt.show()

# %%
height=5
ratio = results['reservoirsize'].nunique() / results['beamwidth'].nunique()
spacing = 1
fig, ax = plt.subplots(1, 2,sharey=True, figsize=((height*3)/ratio+ spacing,height),gridspec_kw={'width_ratios': [1,1]})

cmap = sns.color_palette("viridis", as_cmap=True)
# Set minimum and maximum values for logarithmic colormap
#vmin = -0.6
#vmax = -0.5
cmap.set_bad(color="black")

#l = create_heatmap(lik_avgs, ax[2],  lik_avgs.min().min(),lik_avgs.max().max(),cmap.reversed())
a = create_heatmap(acc_avgs, ax[0], acc_avgs.min().min(), acc_avgs.max().max(), cmap)
r = create_heatmap(run_avgs, ax[1], run_avgs.min().min(), run_avgs.max().max(), cmap.reversed())

#ax[0].set_title('Likelihood')
#ax[0].invert_yaxis()
ax[0].set_title('Accuracy')
ax[1].set_title('Run-time (s)')
ax[0].collections[0].colorbar.set_label("Accuracy")
ax[0].set_ylabel('Reservoir Size')
ax[1].set_ylabel('')
ax[1].collections[0].colorbar.set_label("Run-time (s)")



for a in ax:
    a.set_xlabel('Beam Width')

fig.suptitle("Effect of Hyperparameters on Prediction Accuracy and Run-time")


plt.tight_layout()
plt.savefig('plots/hyperparameters/extParamsAccuracyRuntime.png', dpi=180)

# %% [markdown]
# # Heuristic Search vs baseline

# %% [markdown]
# ### Choose specific experiement
# 202304-1700-0801 is the first experiment ran.
# Includes 795 total runs. 
# With Beam width 1-20 and reservoirs 200-4000 with gaps of 200.
# Timeout was 400s, and only 72 runs completed within the time.
# 
# Next run will increase the timeout to 1000s, and narrow in the grid search region.

# %%

(results, labels) = get_latest_experiment("extension")

# %% [markdown]
#  Extract parameters from algorithm name

# %% [markdown]
# We find that the run time increase heavily with both resovoir size and beam width. Close analysis finds that its a result of slices become very large, due to unspreads combining notes. We limit the evaluator to only applow unspreads produces slices up to size $\gamma$. For brevity, we choose a fixed reservoir size, 700.

# %% [markdown]
# ### Choose specific experiement
# 

# %%
results.algorithm.unique()

# %%
results.loc[results.algorithm == "DualStochasticBeamSearch_3_100"]

# %%
results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).count()

# %%


# %%
#results = results.drop(["id"],axis=1)

# %%
#results.drop(["chordLabels", "slices"], axis=1).groupby(["algorithm"]).agg([np.mean, np.std])

# %%
agm = results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm","piece"]).agg([np.mean, np.std])

# %%
#results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).error

# %%
agm2 = agm.groupby(["corpus","algorithm"]).agg(np.mean)

# %%
errs = agm.reset_index()["accuracy"]["std"].fillna(0)

# %%


# %%
a = results.drop(["chordLabels", "slices"], axis=1)

# %%
std_pivot = pd.pivot_table(a, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.std)

# %%
acc_pivot = pd.pivot_table(a, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)

# %%
lik_pivot = pd.pivot_table(a, 
                          values="likelihood", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)

# %%
acc_pivot

# %%
lik_pivot

# %% [markdown]
# ## Plot Accuracy Chart

# %%
ratio = 1
height = 5

# %%
lik_pivot.max().max()

# %%
lik_pivot


# %%
#fig, ax = plt.subplots(1, 2, figsize=((height*2)/ratio+ spacing,height),gridspec_kw={'width_ratios': [1,1]})
#lik_avgs.min().min(), lik_avgs.max().max()+ (lik_avgs.max().max()-lik_avgs.min().min()

sns.set()  # use Seaborn styles
#acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,2), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
r = acc_pivot.plot.bar( yerr=std_pivot,  figsize=(8,6), fontsize=8, rot=0, ylim=(0,1), width=1.2, legend="loc:lower center")
l = lik_pivot.plot.bar(yerr=std_pivot,  figsize=(6,6), fontsize=8, rot=0, ylim=(lik_pivot.min().min(),lik_pivot.max().max()), width=0.3, legend="loc:lower center")

plt.ylabel('Accuracy')
plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})
          

# %%
#r = acc_pivot.plot.bar( yerr=std_pivot,  figsize=(6,6), fontsize=8, rot=0, ylim=(0,1), width=0.3, legend="loc:lower center")

l = lik_pivot.plot.bar(figsize=(6,6), fontsize=8, rot=0, ylim=(0,1), width=0.3, legend="loc:lower center")

plt.ylabel('Likelihood')
plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})

# %%
sns.set()  # use Seaborn styles
#acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,2), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,6), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")

plt.ylabel('Likelihood')
plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})

# %% [markdown]
# # Heuristic Search vs baseline with smaller beammm

# %% [markdown]
# ### Choose specific experiement
# 202304-1700-0801 is the first experiment ran.
# Includes 795 total runs. 
# With Beam width 1-20 and reservoirs 200-4000 with gaps of 200.
# Timeout was 400s, and only 72 runs completed within the time.
# 
# Next run will increase the timeout to 1000s, and narrow in the grid search region.

# %%

(results, labels) = get_latest_experiment("allexpssmaller")

# %%
path = "outputs"
experiments, experiment_types = get_all_experiments(path)

# %%
results.index.get_level_values(0).max()

# %%
experiments

# %%
exp_id= "20230420212353"

# %%
(results, groundTruth) = get_experiments(exp_id)


# %% [markdown]
#  Extract parameters from algorithm name

# %% [markdown]
# We find that the run time increase heavily with both resovoir size and beam width. Close analysis finds that its a result of slices become very large, due to unspreads combining notes. We limit the evaluator to only applow unspreads produces slices up to size $\gamma$. For brevity, we choose a fixed reservoir size, 700.

# %% [markdown]
# ### Choose specific experiement
# 

# %%
results.algorithm.unique()

# %%
results

# %%
results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).count()

# %%


# %%
#results = results.drop(["id"],axis=1)

# %%
#results.drop(["chordLabels", "slices"], axis=1).groupby(["algorithm"]).agg([np.mean, np.std])

# %%
agm = results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm","piece"]).agg([np.mean, np.std])

# %%
#results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).error

# %%
agm2 = agm.groupby(["corpus","algorithm"]).agg(np.mean)

# %%
errs = agm.reset_index()["accuracy"]["std"].fillna(0)

# %%
a = results.drop(["chordLabels", "slices"], axis=1)

# %%
std_pivot = pd.pivot_table(a, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.std)

# %%
acc_pivot = pd.pivot_table(a, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)

# %%
lik_pivot = pd.pivot_table(a, 
                          values="likelihood", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)

# %%
acc_pivot

# %%
lik_pivot

# %% [markdown]
# ## Plot Accuracy Chart

# %%
ratio = 1
height = 5

# %%
lik_pivot.max().max()

# %%
#fig, ax = plt.subplots(1, 2, figsize=((height*2)/ratio+ spacing,height),gridspec_kw={'width_ratios': [1,1]})
#lik_avgs.min().min(), lik_avgs.max().max()+ (lik_avgs.max().max()-lik_avgs.min().min()

sns.set()  # use Seaborn styles
#acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,2), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
r = acc_pivot.plot.bar( yerr=std_pivot,  figsize=(6,6), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
l = lik_pivot.plot.bar(yerr=std_pivot,  figsize=(6,6), fontsize=8, rot=0, ylim=(lik_pivot.min().min(),lik_pivot.max().max()), width=0.6, legend="loc:lower center")

plt.ylabel('Accuracy')
plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})
          

# %%
#r = acc_pivot.plot.bar( yerr=std_pivot,  figsize=(6,6), fontsize=8, rot=0, ylim=(0,1), width=0.3, legend="loc:lower center")

l = lik_pivot.plot.bar(figsize=(6,6), fontsize=8, rot=0, ylim=(0,1), width=0.3, legend="loc:lower center")

plt.ylabel('Likelihood')
plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})

# %%
sns.set()  # use Seaborn styles
#acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,2), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,6), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")

plt.ylabel('Likelihood')
plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})

# %%


# %%
exp_id = "202304-1715-2135"

# %%
exp_df = results.xs(exp_id,level=1)

# %%
stochastic_params_lim_df = exp_df.xs("stochastic-limited-fixed-res",level=0)

# %%
stochastic_params_lim_df

# %% [markdown]
#  Extract parameters from algorithm name

# %%
stochastic_params_lim_df['beamwidth'] = stochastic_params_lim_df["algorithm"].apply(lambda s: int(s.split("_")[1]))
stochastic_params_lim_df['limit'] = stochastic_params_lim_df["algorithm"].apply(lambda s: int(s.split("_")[3]))

# %% [markdown]
# ## Accuracy, loglikelihood and runtime

# %%
def get_avgs(df, value):
    g = df.dropna(subset=value).groupby(['beamwidth','limit'])
    df_means = g[value].mean()
    return df_means.loc[:,:].reset_index().pivot(index='limit', columns='beamwidth', values=value)

# %%
acc_avgs = get_avgs(stochastic_params_lim_df, 'accuracy')
lik_avgs = get_avgs(stochastic_params_lim_df, 'likelihood')
run_avgs = get_avgs(stochastic_params_lim_df, 'runTime')

# %% [markdown]
# ## Plot hyperparemter results

# %%
def create_heatmap(xs, ax, vmin, vmax,cmap):
    lbls = xs.applymap(lambda v: "{:.2f}".format(v) if v in [np.nanmax(xs.values),np.nanmin(xs.values)]else '')
    return sns.heatmap(xs,
            square=True,
            ax=ax,
            cmap=cmap,
            fmt='',
            annot=lbls, # Label the maximum value
            annot_kws={'fontsize':5},
            mask=xs.isna(),
            vmax=vmax,
            vmin=vmin,
            linewidth=0.01)

# %%
height=5
ratio = stochastic_params_lim_df['limit'].nunique() / stochastic_params_lim_df['beamwidth'].nunique()
spacing = 1
fig, ax = plt.subplots(1, 3, figsize=((height*3)/ratio+ spacing,height),gridspec_kw={'width_ratios': [1,1,1]})

cmap = sns.color_palette("viridis", as_cmap=True)
cmap.set_bad(color="black")

l = create_heatmap(lik_avgs, ax[0], lik_avgs.min().min(), lik_avgs.max().max()+ (lik_avgs.max().max()-lik_avgs.min().min()) * 0.1, cmap)
a = create_heatmap(acc_avgs, ax[1], 0, 1, cmap)
r = create_heatmap(run_avgs, ax[2], run_avgs.min().min(), run_avgs.max().max(), cmap.reversed())

ax[0].set_title('Log-likelihood')
ax[1].set_title('Accuracy')
ax[2].set_title('Run-time (s)')

for a in ax:
    a.set_xlabel('Beam Width')
    a.set_ylabel('Slice Limit')


plt.tight_layout()
plt.savefig('plots/stochbeamlimgridsearch-{}.png'.format(exp_id), dpi=180)

# %% [markdown]
# # Analysis on latest experiment

# %%


# %%
results = results.drop(["id"],axis=1)

# %%
#results.drop(["chordLabels", "slices"], axis=1).groupby(["algorithm"]).agg([np.mean, np.std])

# %%
agm = results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).agg([np.mean, np.std])

# %%
#results.drop(["chordLabels", "slices"], axis=1).groupby(["corpus", "algorithm"]).errors()

# %%
agm = results.drop(["chordLabels", "slices","runTime","likelihood"], axis=1).groupby(["corpus", "algorithm"]).agg([np.std])

# %%
errs = agm.reset_index()["accuracy"]["std"].fillna(0)

# %%
a = results.drop(["chordLabels", "slices"], axis=1)

# %%
std_pivot = pd.pivot_table(a, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.std)

# %%
acc_pivot = pd.pivot_table(a, 
                          values="accuracy", 
                          index="corpus", 
                          columns="algorithm", 
                          aggfunc=np.mean)

# %% [markdown]
# ## Plot Accuracy Chart

# %%
sns.set()  # use Seaborn styles
#acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,2), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
#acc_pivot.plot.bar(yerr=std_pivot, figsize=(6,2), fontsize=8, rot=0, ylim=(0,1), width=0.6, legend="loc:lower center")
#plt.ylabel('Accuracy')
#plt.legend(loc='upper center', title="Algorithm", bbox_to_anchor=(0.5, -0.4),
          fancybox=True, ncol=2, prop={'size': 8})
          

# %% [markdown]
# # Close Error Analysis

# %% [markdown]
# Attributes: chord type, root note, etc

# %% [markdown]
# #### Zoom in on ABC: n02op18-2_03

# %%
groundTruth.loc['ABC','n02op18-2_03']

# %%
results

# %%
ex = results.loc['schumann_kinderszenen','n04']

# %%


# %%
ex[["algorithm", "accuracy", "likelihood", "runTime"]]

# %%
experiments, experiment_types = get_all_experiments("outputs")


