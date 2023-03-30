r"""°°°
# Imports
°°°"""
# |%%--%%| <2m02J4WyK4|YcC0bncuM6>

import subprocess
import json
from pathlib import Path
from glob2 import glob
import os
from multiprocessing import Pool

# |%%--%%| <YcC0bncuM6|KKwSV9K5Nd>
r"""°°°
# Functions
°°°"""
# |%%--%%| <KKwSV9K5Nd|Jaacso6eM3>

def runFullParse (inputPiece, iterations=5):
    (corpus, piece) = inputPiece
    jsonPath = "outputs/"+corpus+"/"+piece+".json"
    cmd = ["stack","run","fullParse","--","-n",str(iterations), corpus, piece, "All"] 
    print("Running command: " + (" ".join(cmd)))
    print("Expecting results in " + jsonPath)
    
    #res = subprocess.run(cmd, cwd="..")
    #res = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd="..")
    res = subprocess.run(cmd,stderr=subprocess.PIPE, cwd="..")

    if res.returncode != 0:
        print("Error in subprocess:")
        print(res.stderr)
        return;
    else:
        f = open(jsonPath)
        results = json.load(f)
        f.close()
        return results
     
def get_corpus_pieces(corpus):
    return sorted (os.path.basename(d).split(".")[0] for d in glob("inputs/slices/"+corpus+"/*.csv"))

def get_corpi():
    return [os.path.basename(d) for d in glob("inputs/slices/*")]

def run_experiment():
    corpi = get_corpi()
    for corpus in corpi:
        pieces = get_corpus_pieces(corpus)
        for piece in pieces:
            runFullParse((corpus, piece))

# |%%--%%| <Jaacso6eM3|d0ifs7vsg1>
r"""°°°
# Run Experiments
°°°"""
# |%%--%%| <d0ifs7vsg1|eHdRDnhify>

if __name__ == '__main__':
    inputs = []
    corpi = get_corpi()
    for corpus in corpi:
        pieces = get_corpus_pieces(corpus)
        for piece in pieces:
            inputs.append((corpus, piece))
    
    with Pool(10) as p:
        p.map(runFullParse, inputs) 
