# Overview

A simple Sudoku solver that I've done to experiment with SAT/SMT solvers.
It currently only runs for boards up to 36x36 (k = 6).
For anything bigger, MiniSAT chokes anyway.

# Build

THIS WAS ONLY TESTED ON CYGWIN!!!

Install Haskell Platform and do
```
  $ make clean all -j4
  $ make check
```
To profile, export PROFILE=1 and rebuild.

# Investigation TODO

* plot timing graphs (time vs. #constraints, #constraints vs. #vars)
* see how rule order influences MiniSAT timing:
  * randomize
  * spatial (rules for ij go together)
* find all solutions (plot graph number-of-sols vs. percentage)
* experiment with different encodings:
  * Sudoku as a SAT Problem (http://anytime.cs.umass.edu/aimath06/proceedings/P34.pdf)
  * A SAT-based Sudoku Solver (https://www.lri.fr/~conchon/mpri/weber.pdf)
  * Optimized CNF Encoding for Sudoku Puzzles (http://www.cs.cmu.edu/~hjain/papers/sudoku-as-SAT.pdf)
* see how Haskell people do interfaces to MiniSAT or other external solvers (and how they cope with big CNFs)
* experiment with SMT solvers
* find all solutions

# Code TODO

* pass several lists to CNF instead of one (this should speed things up because we won't copy data when concatting rules)
* store temp files to separate folder
* Haskell coding style
* write Haddocks
* run for inputs up to 100x100 (k=10); ideas:
  * understand where is all the memory going to
  * generate all rules for each cell in one function
  * use streams

