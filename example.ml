(*
                         Custom example graph
                          CS51 Problem Set 8
                         -.-. ... ..... .----
 *)

open List ;;

open Points ;;
open Masses ;;
open Controls ;;
open Graphobj ;;
open Graphdraw ;;

let example () =
  (* masses *)
  let m1 = new mass 75. 375. 1. in
  let m2 = new mass 225. 375. 1. in
  let m3 = new mass 75. 175. 1. in
  let m4 = new mass 225. 175. 1. in
  let m5 = new mass 150. 150. 1. in
  let m6 = new mass 75. 50. 1. in
  let m7 = new mass 150. 50. 1. in
  let m8 = new mass 225. 50. 1. in
  (* constraints *)

  (* the scene: nodes *)

  (* ... edges *)
  let s1 = new edge ~col:Graphics.red (m1 :> point) (m3 :> point) in
  let s2 = new edge ~col:Graphics.red (m3 :> point) (m5 :> point) in
  let s3 = new edge ~col:Graphics.red (m2 :> point) (m4 :> point) in
  let s4 = new edge ~col:Graphics.red (m4 :> point) (m5 :> point) in
  let s5 = new edge ~col:Graphics.red (m5 :> point) (m7 :> point) in
  let s6 = new edge ~col:Graphics.red (m6 :> point) (m8 :> point) in

  (* solve it *)
  x11_solve [] [] [s1; s2; s3; s4; s5; s6] ;;

let _ = example () ;;
