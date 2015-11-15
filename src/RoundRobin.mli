(* $Id: RoundRobin.mli 3251 2015-10-29 15:56:15Z sutre $ *)


(**
 * Least fixpoint computation through round-robin iteration.
 *
 * This module provides a conservative least fixpoint engine (see {! Fixpoint})
 * that implements the basic round-robin iteration.  Nodes of the input graph
 * are processed in the order of the list returned by the function [nodes] (see
 * {! DiGraph}).
 *)


module Make : Fixpoint.S
