(* $Id: DomConstant.mli 3370 2015-11-13 16:50:50Z sutre $ *)


(**
 * Numerical domain for constant propagation analysis.
 *
 * The underlying abstract lattice contains the following elements: ⊥, all
 * integers, and ⊤.
 *
 * The functions [Op.equality] and [Op.inequality] are optimal.
 *)


include NumericalDomain.S
