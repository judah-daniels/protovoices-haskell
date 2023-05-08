import subprocess
import json
from pathlib import Path
from glob2 import glob
import os
import numpy as np
from multiprocessing import Pool
from datetime import datetime
import pytz
from uuid import uuid4
import random
from tqdm import tqdm
import sys
def get_corpus_pieces(corpus):
    return sorted (os.path.basename(d).split(".")[0] for d in glob("inputs/slices/"+corpus+"/*.csv"))

def get_corpi():
    return [os.path.basename(d) for d in glob("inputs/slices/*")]

def run_command(command):
    print(" ".join(command))    
    #res = subprocess.run(cmd, cwd="..")
    #res = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd="..")
    res = subprocess.run(command,stderr=subprocess.PIPE, cwd="..")
    #pbar.update(1)

    if res.returncode != 0:
        print("Error in subprocess:\n")
        print(res.stderr)
        return;

def ext_pieces_small():  
    pairsStrat = [('schumann_kinderszenen', 'n13'),
             ('schumann_kinderszenen', 'n04'),
             #('chopin_mazurkas', 'BI162-3op63-3'),
             #('grieg_lyric_pieces', 'op47n07'),
             #('chopin_mazurkas', 'BI89-3op24-3'),
             ('grieg_lyric_pieces', 'op43n04'),
             ('chopin_mazurkas', 'BI16-2'),
             ('chopin_mazurkas', 'BI167op67-2'),
             ('grieg_lyric_pieces', 'op12n01')]
    return pairsStrat

def ext_pieces():  
    pairsStrat = [('grieg_lyric_pieces', 'op54n06'),
             ('grieg_lyric_pieces', 'op38n06'),
             ('chopin_mazurkas', 'BI61-5op07-5'),
             ('schumann_kinderszenen', 'n13'),
             ('grieg_lyric_pieces', 'op38n07'),
             ('schumann_kinderszenen', 'n04'),
             ('chopin_mazurkas', 'BI162-3op63-3'),
             ('grieg_lyric_pieces', 'op47n07'),
             ('chopin_mazurkas', 'BI89-3op24-3'),
             ('grieg_lyric_pieces', 'op43n04'),
             ('chopin_mazurkas', 'BI162-2op63-2'),
             ('chopin_mazurkas', 'BI168op68-4'),
             ('chopin_mazurkas', 'BI16-2'),
             ('chopin_mazurkas', 'BI167op67-2'),
             ('chopin_mazurkas', 'BI73'),
             ('schumann_kinderszenen', 'n05'),
             ('grieg_lyric_pieces', 'op12n01'),
             ('chopin_mazurkas', 'BI16-1'),
             ('grieg_lyric_pieces', 'op68n05'),
             ('grieg_lyric_pieces', 'op71n02'),
             ('chopin_mazurkas', 'BI157-2op59-2'),
             ('grieg_lyric_pieces', 'op54n01'),
             ('grieg_lyric_pieces', 'op68n03'),
             ('chopin_mazurkas', 'BI61-4op07-4'),
             ('grieg_lyric_pieces', 'op71n07'),
             ('chopin_mazurkas', 'BI105-3op30-3'),
             ('chopin_mazurkas', 'BI162-1op63-1'),
             ('chopin_mazurkas', 'BI145-2op50-2'),
             ('grieg_lyric_pieces', 'op62n05'),
             ('chopin_mazurkas', 'BI153-2op56-2'),
             ('grieg_lyric_pieces', 'op68n04'),
             ('grieg_lyric_pieces', 'op47n06')]
    return pairsStrat

def baseline_pieces():  
    pairsStrat = [('grieg_lyric_pieces', 'op54n06'),
             ('grieg_lyric_pieces', 'op38n06'),
             ('chopin_mazurkas', 'BI61-5op07-5'),
             ('schumann_kinderszenen', 'n13'),
             ('grieg_lyric_pieces', 'op38n07'),
             ('schumann_kinderszenen', 'n04'),
             ('chopin_mazurkas', 'BI162-3op63-3'),
             ('grieg_lyric_pieces', 'op47n07'),
             ('chopin_mazurkas', 'BI89-3op24-3'),
             ('grieg_lyric_pieces', 'op43n04'),
             ('ABC', 'n14op131_03'),
             ('chopin_mazurkas', 'BI162-2op63-2'),
             ('chopin_mazurkas', 'BI168op68-4'),
             ('ABC', 'n14op131_06'),
             ('chopin_mazurkas', 'BI16-2'),
             ('chopin_mazurkas', 'BI167op67-2'),
             ('ABC', 'n04op18-4_03'),
             ('ABC', 'n13op130_02'),
             ('chopin_mazurkas', 'BI73'),
             ('schumann_kinderszenen', 'n05'),
             ('grieg_lyric_pieces', 'op12n01'),
             ('chopin_mazurkas', 'BI16-1'),
             ('grieg_lyric_pieces', 'op68n05'),
             ('grieg_lyric_pieces', 'op71n02'),
             ('chopin_mazurkas', 'BI157-2op59-2'),
             ('grieg_lyric_pieces', 'op54n01'),
             ('ABC', 'n16op135_03'),
             ('grieg_lyric_pieces', 'op68n03'),
             ('chopin_mazurkas', 'BI61-4op07-4'),
             ('grieg_lyric_pieces', 'op71n07'),
             ('chopin_mazurkas', 'BI105-3op30-3'),
             ('chopin_mazurkas', 'BI162-1op63-1'),
             ('ABC', 'n01op18-1_03'),
             ('ABC', 'n09op59-3_03'),
             ('chopin_mazurkas', 'BI145-2op50-2'),
             ('grieg_lyric_pieces', 'op62n05'),
             ('chopin_mazurkas', 'BI153-2op56-2'),
             ('grieg_lyric_pieces', 'op68n04'),
             ('grieg_lyric_pieces', 'op47n06')]
    return pairsStrat
    
    

def parseable_pieces(): 
    pairs = []
    r = {"grieg_lyric_pieces": ["op12n01","op38n07", "op47n04", "op71n03", "op54n06"]
        ,"chopin_mazurkas": ["BI168op68-4", "BI153-2op56-2", "BI89-3op24-3"]
        ,"schumann_kinderszenen": ["n03", "n04", "n13"]
        ,"ABC": ["n14op131_06", "n16op135_03", "n14op131_03", "n14op131_06"]}
    
    pairs = [('schumann_kinderszenen', 'n03'),
             ('grieg_lyric_pieces', 'op54n06'),
             ('grieg_lyric_pieces', 'op47n04'),
             ('grieg_lyric_pieces', 'op68n02'),
             ('grieg_lyric_pieces', 'op38n06'),
             ('chopin_mazurkas', 'BI61-5op07-5'),
             ('schumann_kinderszenen', 'n13'),
             ('grieg_lyric_pieces', 'op38n07'),
             ('schumann_kinderszenen', 'n04'),
             ('chopin_mazurkas', 'BI162-3op63-3'),
             ('grieg_lyric_pieces', 'op47n07'),
             ('chopin_mazurkas', 'BI89-3op24-3'),
             ('grieg_lyric_pieces', 'op71n03'),
             ('grieg_lyric_pieces', 'op43n04'),
             ('ABC', 'n14op131_03'),
             ('chopin_mazurkas', 'BI162-2op63-2'),
             ('chopin_mazurkas', 'BI168op68-4'),
             ('ABC', 'n14op131_06'),
             ('chopin_mazurkas', 'BI16-2'),
             ('chopin_mazurkas', 'BI89-2op24-2'),
             ('grieg_lyric_pieces', 'op71n05'),
             ('grieg_lyric_pieces', 'op12n08'),
             ('chopin_mazurkas', 'BI167op67-2'),
             ('ABC', 'n04op18-4_03'),
             ('ABC', 'n13op130_02'),
             ('chopin_mazurkas', 'BI73'),
             ('schumann_kinderszenen', 'n05'),
             ('grieg_lyric_pieces', 'op12n01'),
             ('chopin_mazurkas', 'BI16-1'),
             ('grieg_lyric_pieces', 'op68n05'),
             ('grieg_lyric_pieces', 'op71n02'),
             ('chopin_mazurkas', 'BI157-2op59-2'),
             ('grieg_lyric_pieces', 'op38n02'),
             ('chopin_mazurkas', 'BI145-1op50-1'),
             ('grieg_lyric_pieces', 'op54n01'),
             ('ABC', 'n16op135_03'),
             ('grieg_lyric_pieces', 'op68n03'),
             ('chopin_mazurkas', 'BI61-4op07-4'),
             ('grieg_lyric_pieces', 'op71n07'),
             ('chopin_mazurkas', 'BI105-3op30-3'),
             ('chopin_mazurkas', 'BI162-1op63-1'),
             ('ABC', 'n01op18-1_03'),
             ('grieg_lyric_pieces', 'op65n04'),
             ('grieg_lyric_pieces', 'op57n03'),
             ('ABC', 'n09op59-3_03'),
             ('chopin_mazurkas', 'BI145-2op50-2'),
             ('grieg_lyric_pieces', 'op62n05'),
             ('grieg_lyric_pieces', 'op12n02'),
             ('grieg_lyric_pieces', 'op47n03'),
             ('chopin_mazurkas', 'BI153-2op56-2'),
             ('ABC', 'n14op131_05'),
             ('grieg_lyric_pieces', 'op68n04'),
             ('schumann_kinderszenen', 'n09'),
             ('ABC', 'n15op132_04'),
             ('grieg_lyric_pieces', 'op47n06'),
             ('ABC', 'n05op18-5_02'),
             ('grieg_lyric_pieces', 'op43n06'),
             ('ABC', 'n08op59-2_02')]
    
    pairs2 = [('grieg_lyric_pieces', 'op54n06'),
             ('grieg_lyric_pieces', 'op47n04'),
             ('grieg_lyric_pieces', 'op38n06'),
             ('chopin_mazurkas', 'BI61-5op07-5'),
             ('schumann_kinderszenen', 'n13'),
             ('grieg_lyric_pieces', 'op38n07'),
             ('schumann_kinderszenen', 'n04'),
             ('chopin_mazurkas', 'BI162-3op63-3'),
             ('grieg_lyric_pieces', 'op47n07'),
             ('chopin_mazurkas', 'BI89-3op24-3'),
             ('grieg_lyric_pieces', 'op43n04'),
             ('ABC', 'n14op131_03'),
             ('chopin_mazurkas', 'BI162-2op63-2'),
             ('chopin_mazurkas', 'BI168op68-4'),
             ('ABC', 'n14op131_06'),
             ('chopin_mazurkas', 'BI16-2'),
             ('chopin_mazurkas', 'BI89-2op24-2'),
             ('grieg_lyric_pieces', 'op12n08'),
             ('chopin_mazurkas', 'BI167op67-2'),
             ('ABC', 'n04op18-4_03'),
             ('ABC', 'n13op130_02'),
             ('chopin_mazurkas', 'BI73'),
             ('schumann_kinderszenen', 'n05'),
             ('grieg_lyric_pieces', 'op12n01'),
             ('chopin_mazurkas', 'BI16-1'),
             ('grieg_lyric_pieces', 'op68n05'),
             ('grieg_lyric_pieces', 'op71n02'),
             ('chopin_mazurkas', 'BI157-2op59-2'),
             ('grieg_lyric_pieces', 'op38n02'),
             ('chopin_mazurkas', 'BI145-1op50-1'),
             ('grieg_lyric_pieces', 'op54n01'),
             ('ABC', 'n16op135_03'),
             ('grieg_lyric_pieces', 'op68n03'),
             ('chopin_mazurkas', 'BI61-4op07-4'),
             ('grieg_lyric_pieces', 'op71n07'),
             ('chopin_mazurkas', 'BI105-3op30-3'),
             ('chopin_mazurkas', 'BI162-1op63-1'),
             ('ABC', 'n01op18-1_03'),
             ('grieg_lyric_pieces', 'op65n04'),
             ('grieg_lyric_pieces', 'op57n03'),
             ('ABC', 'n09op59-3_03'),
             ('chopin_mazurkas', 'BI145-2op50-2'),
             ('grieg_lyric_pieces', 'op62n05'),
             ('grieg_lyric_pieces', 'op12n02'),
             ('grieg_lyric_pieces', 'op47n03'),
             ('chopin_mazurkas', 'BI153-2op56-2'),
             ('ABC', 'n14op131_05'),
             ('grieg_lyric_pieces', 'op68n04'),
             ('schumann_kinderszenen', 'n09'),
             ('ABC', 'n15op132_04'),
             ('grieg_lyric_pieces', 'op47n06'),
             ('ABC', 'n05op18-5_02'),
             ('grieg_lyric_pieces', 'op43n06'),
             ('ABC', 'n08op59-2_02')]
    
    pairsStrat = [('grieg_lyric_pieces', 'op54n06'),
             ('grieg_lyric_pieces', 'op38n06'),
             ('chopin_mazurkas', 'BI61-5op07-5'),
             ('schumann_kinderszenen', 'n13'),
             ('grieg_lyric_pieces', 'op38n07'),
             ('schumann_kinderszenen', 'n04'),
             ('chopin_mazurkas', 'BI162-3op63-3'),
             ('grieg_lyric_pieces', 'op47n07'),
             ('chopin_mazurkas', 'BI89-3op24-3'),
             ('grieg_lyric_pieces', 'op43n04'),
             ('ABC', 'n14op131_03'),
             ('chopin_mazurkas', 'BI162-2op63-2'),
             ('chopin_mazurkas', 'BI168op68-4'),
             ('ABC', 'n14op131_06'),
             ('chopin_mazurkas', 'BI16-2'),
             ('chopin_mazurkas', 'BI167op67-2'),
             ('ABC', 'n04op18-4_03'),
             ('ABC', 'n13op130_02'),
             ('chopin_mazurkas', 'BI73'),
             ('schumann_kinderszenen', 'n05'),
             ('grieg_lyric_pieces', 'op12n01'),
             ('chopin_mazurkas', 'BI16-1'),
             ('grieg_lyric_pieces', 'op68n05'),
             ('grieg_lyric_pieces', 'op71n02'),
             ('chopin_mazurkas', 'BI157-2op59-2'),
             ('grieg_lyric_pieces', 'op54n01'),
             ('ABC', 'n16op135_03'),
             ('grieg_lyric_pieces', 'op68n03'),
             ('chopin_mazurkas', 'BI61-4op07-4'),
             ('grieg_lyric_pieces', 'op71n07'),
             ('chopin_mazurkas', 'BI105-3op30-3'),
             ('chopin_mazurkas', 'BI162-1op63-1'),
             ('ABC', 'n01op18-1_03'),
             ('grieg_lyric_pieces', 'op65n04'),
             ('ABC', 'n09op59-3_03'),
             ('chopin_mazurkas', 'BI145-2op50-2'),
             ('grieg_lyric_pieces', 'op62n05'),
             ('grieg_lyric_pieces', 'op47n03'),
             ('chopin_mazurkas', 'BI153-2op56-2'),
             ('grieg_lyric_pieces', 'op68n04'),
             ('ABC', 'n15op132_04'),
             ('grieg_lyric_pieces', 'op47n06'),
             ('ABC', 'n05op18-5_02')]
    
    for corpus in r.keys():
        for piece in r[corpus]:
            pairs.append((corpus,piece))
    return pairsStrat
            
def run_Experiment(commands,name,iterations, threads):
    timeout=1300
    
    uk_tz = pytz.timezone('Europe/London')
    id = datetime.now(uk_tz).strftime('%Y%m%d%H%M%S_')+ name
    #random.shuffle(commands)

    commands = [["stack","run","fullParse","--","-t",str(timeout),"-n",str(iterations), "-id", str(id)] + command for command in commands]
    
    
    with Pool(threads) as p:
        for _ in tqdm(p.imap_unordered(run_command, commands), total=len(commands)):
            pass
    

        
def stochastic_res_experiment(threads=28):
    commands = []
    corpi = get_corpi()
    beamWidthRange = range (1,40,2)
    reservoirSizeRange = range(100,1000,50)
    for corpus in corpi:
        pieces = get_corpus_pieces(corpus)
        for piece in pieces:
            for w in beamWidthRange:
                for r in reservoirSizeRange:
                    algo = "StochasticBeamSearch {} {}".format(w,r)
                    commands.append([ corpus, piece, algo])
    
    print("Running {} Stochastic Beam Search experiments, with beamwidth {} and reservoirSize {}".format(len(commands), beamWidthRange, reservoirSizeRange))

    run_Experiment(commands, "stochastic-all-params", threads)

def dual_stochastic_res_experiment(inputs, beamWidthRange, reservoirSizeRange, iterations=1, threads=28):
    commands = []

    for (corpus, piece) in inputs:
        for w in beamWidthRange:
            for r in reservoirSizeRange:
                algo = "DualStochasticBeamSearch {} {}".format(w,r)
                commands.append([ corpus, piece, algo])
    
    print("Running {} Stochastic Beam Search experiments, with beamwidth {} and reservoirSize {}".format(len(commands)*iterations, beamWidthRange, reservoirSizeRange))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "dual-stochastic-res-params-grieg128", iterations,threads)

def all_searches_experiment(inputs, iterations=10, threads=28):
    beamWidths = range (6,36,3)
    commands = []
    for (corpus, piece) in inputs:
        algo1 = "RandomWalk"
        algo2 = "DualStochasticBeamSearch {} {}".format(20,50)
        algo3 = "DualStochasticBeamSearch {} {}".format(40,50)
        algo4 = "DualStochasticBeamSearch {} {}".format(60,50)
        algo41 = "DualStochasticBeamSearch {} {}".format(20,100)
        algo42 = "DualStochasticBeamSearch {} {}".format(20,300)
        algo43 = "DualStochasticBeamSearch {} {}".format(20,600)
        algo44 = "DualStochasticBeamSearch {} {}".format(40,600)
        algo45 = "DualStochasticBeamSearch {} {}".format(40,300)
        algo46 = "DualStochasticBeamSearch {} {}".format(100,300)
        algo5 = "RandomWalkPerSegment"
        algo6 = "RandomReduction"
        algo7 = "RandomSample"
        for a in [algo1,algo2,algo3,algo4,algo41,algo42,algo43,algo44,algo45,algo46,algo5, algo6, algo7]: 
            commands.append([ corpus, piece,a ])
    print("Running {} experiments with all different algos".format(len(commands)*iterations, beamWidthRange, reservoirSizeRange))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "allexps", iterations,threads)


def smaller_beam(inputs, iterations=10, threads=28):
    beamWidths = range (6,36,3)
    commands = []
    for (corpus, piece) in inputs:
        algo1 = "RandomWalk"
        algo2 = "DualStochasticBeamSearch {} {}".format(1,1)
        algo3 = "DualStochasticBeamSearch {} {}".format(2,2)
        algo4 = "DualStochasticBeamSearch {} {}".format(3,10)
        algo41 = "DualStochasticBeamSearch {} {}".format(3,30)
        algo42 = "DualStochasticBeamSearch {} {}".format(3,100)
        algo43 = "DualStochasticBeamSearch {} {}".format(5,5)
        algo44 = "DualStochasticBeamSearch {} {}".format(5,40)
        algo45 = "DualStochasticBeamSearch {} {}".format(12,50)
        algo46 = "DualStochasticBeamSearch {} {}".format(12,300)
        algo5 = "RandomWalkPerSegment"
        algo6 = "RandomReduction"
        algo7 = "RandomSample"
        for a in [algo1,algo2,algo3,algo4,algo41,algo42,algo43,algo44,algo45,algo46,algo5, algo6, algo7]: 
            commands.append([ corpus, piece,a ])
    print("Running {} experiments with all different with smaller beam algos".format(len(commands)*iterations, beamWidthRange, reservoirSizeRange))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "allexpssmaller", iterations,threads)
    
def stochastic_res_lim_experiment(threads=28):
    commands = []
    corpi = get_corpi()
    beamWidthRange = range (7,14,1)
    limitRange = range(1,20,1)
    reservoirSize = 700
    for corpus in corpi:
        pieces = get_corpus_pieces(corpus)
        for piece in pieces:
            for w in beamWidthRange:
                for l in limitRange:
                    algo = "StochasticBeamSearchLimited {} {} {}".format(w,reservoirSize,l)
                    commands.append([ corpus, piece, algo])
    
    print("Running {} Stochastic Beam Search Limited experiments, with beamwidth {}, reservoirSize {} and spread note limit {}".format(len(commands), beamWidthRange, reservoirSize, limitRange))
    print("Experiment will take approx {}-{} hours\n".format(commands/ 28 * 0.1),commands/ 28 * 0.27)

    run_Experiment(commands, "stochastic-limited-fixed-res", threads)
    
    
def new_heuristics(threads=28):
    commands = []
    for (corpus, piece) in inputs:
        algo1 = "RandomWalk"
        algo2 = "DualStochasticBeamSearch {} {}".format(1,5)
        algo3 = "DualStochasticBeamSearch {} {}".format(5,10)
        algo4 = "DualStochasticBeamSearch {} {}".format(70,300)
        algo41 = "DualStochasticBeamSearch {} {}".format(3,30)
        algo42 = "DualStochasticBeamSearch {} {}".format(3,100)
        algo43 = "DualStochasticBeamSearch {} {}".format(5,5)
        algo44 = "DualStochasticBeamSearch {} {}".format(5,40)
        algo45 = "DualStochasticBeamSearch {} {}".format(12,50)
        algo46 = "DualStochasticBeamSearch {} {}".format(12,300)
        
        algo5 = "RandomWalkPerSegment"
        algo6 = "RandomReduction"
        algo7 = "RandomSample"
        for a in [algo1,algo2,algo3,algo4,algo41,algo42,algo43,algo44,algo45,algo46,algo5, algo6, algo7]: 
            commands.append([ corpus, piece,a ])
    print("Running {} experiments with all different with smaller beam algos".format(len(commands)*iterations, beamWidthRange, reservoirSizeRange))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "newheuristics", iterations,threads)
    
def exp_parseable(iterations=2, threads=28):
    beamWidths = range (6,36,3)
    inputs = parseable_pieces()
    commands = []
    for (corpus, piece) in inputs:
        algo1 = "RandomWalk"
        algo2 = "DualStochasticBeamSearch {} {}".format(3,100)
        algo42 = "DualStochasticBeamSearch {} {}".format(5,50)
        algo45 = "DualStochasticBeamSearch {} {}".format(10,50)
        algo5 = "RandomWalkPerSegment"
        algo6 = "RandomReduction"
        algo7 = "RandomSample"
        algo8 = "PerfectReduction"

        for a in [algo1,algo2,algo42, algo45, algo5, algo6, algo8, algo7]: 
            commands.append([ corpus, piece,a ])
    print("Running {} experiments on just parseable ones ".format(len(commands)*iterations))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "exppareablee", iterations,threads)
    
    
def baseline_exp(iterations=8,threads=28):
    commands = []
    for (corpus, piece) in baseline_pieces():
        algos = ["RandomWalk", "PerfectReduction", "RandomReduction", "Templating"]
        for a in algos: 
            commands.append([ corpus, piece,a ])
    print("Running {} experiments on all scores with random walk".format(len(commands)))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)/ 28 * 0.08, len(commands)/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "baselineExp", iterations,threads)
    
    
def extension_exp(iterations=8,threads=28):
    commands = []
    for (corpus, piece) in ext_pieces_small():
        algos = ["RandomWalk", "RandomReduction", "DualStochasticBeamSearch 18 65"]
        for a in algos: 
            commands.append([ corpus, piece,a ])
    print("Running {} experiments on all scores with walk and extn".format(len(commands)))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)/ 28 * 0.08, len(commands)/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "extensionExp", iterations,threads)

def extension_params(iterations=4,threads=28):
    commands = []
    beamWidthRange = range(2, 15,1)
    reservoirSizeRange = range(10,140,10)
    for (corpus, piece) in [("chopin_mazurkas", "BI16-2")]:
        for w in beamWidthRange:
            for r in reservoirSizeRange:
                commands.append([ corpus, piece,"DualStochasticBeamSearch {} {}".format(w,r) ])
    print("Running {} experiments with different params".format(len(commands)))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)/ 28 * 0.08, len(commands)/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "extensionParams", iterations,threads)

def extension_params_fine(iterations=8,threads=28):
    commands = []
    beamWidthRange = range(2, 20,1)
    reservoirSizeRange = range(5,150,5)
    for (corpus, piece) in [("chopin_mazurkas", "BI16-2")]:
        for w in beamWidthRange:
            for r in reservoirSizeRange:
                commands.append([ corpus, piece,"DualStochasticBeamSearch {} {}".format(w,r) ])
    print("Running {} experiments with different params".format(len(commands)))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)/ 28 * 0.08, len(commands)/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "extensionParamsFine", iterations,threads)
    


def extension_beamwidth(iterations=5,threads=28):
    commands = []
    beamWidthRange = range(2, 15,1)
    reservoirSizeRange = [10, 40, 70, 100]
    for (corpus, piece) in [("chopin_mazurkas", "BI16-2")]:
        for w in beamWidthRange:
            for r in reservoirSizeRange:
                commands.append([ corpus, piece,"DualStochasticBeamSearch {} {}".format(w,r) ])
    print("Running {} experiments with different params".format(len(commands)))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)/ 28 * 0.08, len(commands)/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "extensionBeamwidth", iterations,threads)

def extension_reservoir(iterations=5,threads=28):
    commands = []
    beamWidthRange = [2,5,9,13]
    reservoirSizeRange = range(2, 100,5)
    for (corpus, piece) in [("chopin_mazurkas", "BI16-2")]:
        for w in beamWidthRange:
            for r in reservoirSizeRange:
                commands.append([ corpus, piece,"DualStochasticBeamSearch {} {}".format(w,r) ])
    print("Running {} experiments with different params".format(len(commands)))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)/ 28 * 0.08, len(commands)/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "extensionReservoir", iterations,threads)


    
def parseablepiecesall(iterations=2,threads=28):
    commands = []
    for corpus in get_corpi():
        pieces = get_corpus_pieces(corpus)
        for piece in pieces:
            a = "RandomWalk"
            commands.append([ corpus, piece,a ])
    print("Running {} experiments on all scores with random walk".format(len(commands)*iterations))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "parseablepiecesrandomwalk", iterations,threads)
    
    
    
def perfectredutions(iterations=1,threads=28):
    commands = []
    for corpus in get_corpi():
        pieces = get_corpus_pieces(corpus)
        for piece in pieces:
            commands.append([ corpus, piece, "PerfectReduction"])
    print("Running {} experiments on all scores with random walk".format(len(commands)*iterations))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))
    
def perfectvstemplating(iterations=8,threads=28):
    commands = []
    for (corpus,piece) in parseable_pieces():
        commands.append([ corpus, piece, "PerfectReduction"])
        commands.append([ corpus, piece, "Templating"])
    print("Running {} experiments on all scores with templating and perfect".format(len(commands)*iterations))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))
    random.shuffle(commands)
    run_Experiment(commands, "perfectvstemplating", iterations,threads)
    
def extLengthPiece(iterations=5,threads=28):
    commands = []
    for (corpus,piece) in parseable_pieces():
        commands.append([ corpus, piece, "DualStochasticBeamSearch 5 25"])
    print("Running {} experiments to mesuare legnth of pice, beam 5 res 25".format(len(commands)*iterations))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))
    random.shuffle(commands)
    run_Experiment(commands, "extlengthpiece", iterations,threads)
    
#if __name__ == '__main__':
#    beamWidthRange = range (10,510,50)
#    reservoirSizeRange = range(50,750,50)
#    pairs = [("grieg_lyric_pieces", x) for x in get_corpus_pieces("grieg_lyric_pieces")]
#    all_searches_experiment(pairs, iterations=5,threads=28)

if __name__ == '__main__':
    #parseable_pieces_randomwalk(iterations=3,threads=28) 
    #exp_parseable(iterations=3,threads=28) 
    #baseline_exp()
    #perfectredutions()  
    #extension_params_fine()
    #perfectvstemplating()
    #extension_exp()
    extLengthPiece()
    #extension_reservoir()
    #extension_beamwidth()
    
#if __name__ == '__main__':
#    perfect_redutions()  
#if __name__ == '__main__':
#    exp_parseable(iterations=2,threads=28) 
# if __name__ == '__main__':
#     beamWidthRange = range (10,510,50)
#     reservoirSizeRange = range(50,750,50)
#     pairs = []

#     for corpus in get_corpi():
#         pieces = get_corpus_pieces(corpus)
#         for piece in pieces:
#             pairs.append((corpus, piece))
#     #pairs = [("grieg_lyric_pieces", x) for x in get_corpus_pieces("grieg_lyric_pieces")]
#     random.shuffle(pairs)

#     smaller_beam(pairs, iterations=2,threads=28)
    
