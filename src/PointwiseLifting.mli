(* $Id: PointwiseLifting.mli 3353 2015-11-12 09:45:14Z sutre $ *)


(**
 * Pointwise lifting of a numerical non-relational abstract domain.
 *
 * This module provides a functor that takes a module [N] of type
 * {! NumericalDomain.S} and returns a module [A] of type {! AbstractDomain.S}.
 * The lattice [A] is obtained by pointwise lifting of the lattice [N].  More
 * precisely, an abstract element in [A] is a mapping from variables (i.e.,
 * values of type [Variable.t]) to abstract numerical values (i.e., values of
 * type [N.t]).
 *
 * In the implementation, abstract tests x ⋈ y, where ⋈ ∈ \{=, ≤\} and x, y are
 * abstract numerical values, are reduced to (α(\{0\}))*v + (x-y) ⋈ 0 and solved
 * using [N.Op.equality] or [N.Op.inequality].  In general, this approach is
 * less precise than dedicated abstract tests x ⋈ y.  There are two reasons for
 * choosing this approach despite the potential loss of precision.
 *
 * - Dedicated abstract tests would have to be performed and provided by the
 *   numerical abstract domain, which would complexify the {! NumericalDomain.S}
 *   interface.
 *
 * - There is no loss of precision when the numerical abstract domain has both
 *   an {e exact} abstract zero (i.e., γ(α(\{0\})) = \{0\}) and an {e exact}
 *   subtraction (i.e., γ(x-y) = γ(x) - γ(y)).
 *
 * For the best precision, [N.Op.equality] and [N.Op.inequality] must be optimal
 * in the special case where the first argument is α(\{0\}).
 *)


module Make : functor (N : NumericalDomain.S) -> AbstractDomain.S
