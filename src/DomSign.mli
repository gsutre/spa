(* $Id: DomSign.mli 3370 2015-11-13 16:50:50Z sutre $ *)


(**
 * Numerical domain for sign analysis.
 *
 * The underlying abstract lattice contains the following elements: ⊥, negative,
 * zero, positive, nonpositive, nonzero, nonnegative, and ⊤.
 *
 * The functions [Op.equality] and [Op.inequality] are optimal.
 *)


include NumericalDomain.S
