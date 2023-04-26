import subprocess
import json
from pathlib import Path
from glob2 import glob
import os
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
    
def parseable_pieces(pairs): 
    pairs = []
    r = {"grieg_lyric_pieces": ["op12n01","op38n07", "op47n04", "op71n03", "op54n06"]
        ,"chopin_mazurkas": ["BI168op68-4", "BI89-3op24-3"]
        ,"schumann_kinderszenen": ["n03", "n04", "n13"]
        ,"ABC": ["n14op131_06", "n14op131_03", "n14op131_06"]}
    
    for corpus in r.keys():
        for piece in r[corpus]:
            pairs.append((corpus,piece))
    return pairs
            
def run_Experiment(commands,name,iterations, threads):
    timeout=60
    
    uk_tz = pytz.timezone('Europe/London')
    id = datetime.now(uk_tz).strftime('%Y%m%d%H%M%S_')+ name
    #random.shuffle(commands)

    commands = [["stack","run","fullParse","--","-t",str(timeout),"-n",str(iterations), "-id", str(id)] + command for command in commands]
    
    
    with Pool(threads) as p:
        for _ in tqdm(p.imap_unordered(run_command, commands), total=len(commands)*iterations):
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
    
    
def parseable_pieces_randomwalk(iterations=2,threads=28):
    commands = []
    for corpus in get_corpi():
        pieces = get_corpus_pieces(corpus)
        for piece in pieces:
            a = "RandomWalk"
            commands.append([ corpus, piece,a ])
    print("Running {} experiments on all scores with random walk".format(len(commands)*iterations))
    print("Experiment will take approx {}-{} hours\n".format(len(commands)*iterations/ 28 * 0.08, len(commands)*iterations/ 28 * 0.2))

    random.shuffle(commands)
    run_Experiment(commands, "parseable_pieces_randomwalk", iterations,threads)

#if __name__ == '__main__':
#    beamWidthRange = range (10,510,50)
#    reservoirSizeRange = range(50,750,50)
#    pairs = [("grieg_lyric_pieces", x) for x in get_corpus_pieces("grieg_lyric_pieces")]
#    all_searches_experiment(pairs, iterations=5,threads=28)
if __name__ == '__main__':
    parseable_pieces_randomwalk(iterations=2,threads=28) 
    
    
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
    
