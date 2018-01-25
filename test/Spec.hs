import Spec.Multioptimizer.Util.Pareto.Internal as PI

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All tests" [PI.units]
