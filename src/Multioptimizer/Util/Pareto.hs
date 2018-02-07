module Multioptimizer.Util.Pareto (
  dominates,
  domOrdering,
  Frontier,
  emptyFrontier,
  getFrontier,
  insert,
  insertQuery,
  shrinkToSize
) where

import Multioptimizer.Util.Pareto.Internal