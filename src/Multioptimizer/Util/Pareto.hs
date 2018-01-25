module Multioptimizer.Util.Pareto (
  dominates,
  Frontier,
  emptyFrontier,
  getFrontier,
  insert,
  insertQuery,
  shrinkToSize
) where

import Multioptimizer.Util.Pareto.Internal