# proto-voice-model

implementation sketches of the proto-voice model (WIP)


Full piece and segment 

Cant do it accross segment boundaries.
You can unsplit but cannot unspread

State is a partially reduced full piece.
initial state is surface of score and empty reduciton
Goal state is any partial reduction with one slice per segment.

Only for single segment at a time.

The paradigm for parsing would be the search.
Parsing is always search.

Similar to graph search. Looking for shortest path between all nodes.
all paths between two vertices, then pick the shortest maybe.

Idea is not to enumerate all, but just find one. 

Starting config
Intermediate config - a step is one reduction step
State is always the current reduction and all the steps that has been done so far.

A search is agenda.
Put stuff in agenda, which order do we explore the space?
If we organise it as a queue. then you get bfs.
Agenda becomes like a priority.

Estimate how good the partial reduction is.
- Use inferred probabilties to estimate how plausible a reduciton
- Nothing to do with harmony
- Very crude but works suprisingly well.
- We are looking into account the harmonic configuration 
- Chapter 5 tone profiles
  - which notes are plausible chord tones, which notes are plausible ornamentation.
  - look at the ornamentation profile of the typical chord
  - eg for major chord the profile for chord tone is 1 3 5 maybe 6 and 9
  - but for a dominant 7th chord then a 4 is often an ornament for example

So i use these profiles from chapter 5 

figure out a way of seeing how 
harmonic probabilities
elaboration probabilties
ornamentation probabilities

This is something to play around with.


