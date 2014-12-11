open Core.Std

(* Adapted from http://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf *)


type ('vertex, 'edge) =
  | Empty
  | Graph of 
