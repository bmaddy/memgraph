# Reference implementation of micro and mini Adapton in Clojure

This is a Clojure implementation of microAdapton and miniAdapton as described on [adapton.org](adapton.org).

## Not finished...
* Don't use this in production: performance improvements would need to be done 
* I haven't come up with a good example that shows a performance increase over normal memoization yet, but I haven't tried terribly hard either.

## Resources
* The paper this is based on: [miniAdapton: A Minimal Implementation of Incremental Computation in Scheme](https://arxiv.org/abs/1609.05337)
* Comparing Adapton to Reactive Programming: [Programming Language Techniques for Incremental and
Reactive Computing](http://www.informatik.uni-marburg.de/~seba/publications/IC-dagstuhl.pdf)
* Another Clojure implementation: https://github.com/aibrahim/micro_adapton

## Possible future improvements
* use a profiler to see why it's slow
* maybe use proteus for the mutable state? - https://github.com/ztellman/proteus
* make it work with core.cache and core.memoize
* maybe implement shadow dom and compare against vue and react?
** Preact.js has a small api, might be a good way to do a POC
** build your own react guides:
*** https://hackernoon.com/build-your-own-react-48edb8ed350d
*** https://engineering.hexacta.com/didact-learning-how-react-works-by-building-it-from-scratch-51007984e5c5

