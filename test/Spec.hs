import Spec.Multioptimizer.Util.Pareto.Internal as PI
import Spec.Multioptimizer.Backend.MCTS as MCTS

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All tests" [PI.units, MCTS.units]
