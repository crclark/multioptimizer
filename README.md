# multioptimizer

Multiobjective search over arbitrary data structures, with a monadic interface and multiple search strategies including Monte Carlo Tree Search.



### TODO

* Distributed workers.

* Instead of specifying our objectives when we run the generator, it might be interesting to interleave `maximize` commands *inside* the do notation. For example:
  ```haskell
  foo = do
    x <- choose xs
    maximize "obj1" (f x)
    y <- choose ys
    maximize "obj2" (g x y)
  ```

  The advantage here would be that we don't need to think about `y` to optimize `obj1`, which could hasten convergence. This seems powerful, but has a number of open questions I haven't thought through. What if we reach a different set of `maximize` commands on each run? What if we place a `maximize` command inside a loop, giving us `n` extra objectives? These seem like problems that can be solved with straight-forward extensions of the Pareto dominance idea, plus added bookkeeping, but I'm not sure if there are any applications that could benefit from this added complexity.

