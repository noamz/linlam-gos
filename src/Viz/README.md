# Visualization

Some visualization routines using the Haskell [diagrams](http://projects.haskell.org/diagrams/) DSL.  Associated diagrams are typically sent [here](/diagrams).


* Catalan.hs: visualizing [Catalan](https://en.wikipedia.org/wiki/Catalan_number) objects as binary trees or as planar arc diagrams.
* List.hs: basic utilities for lists.
* Test*.hs: various tests.

To run the tests, for example you could try the following from ghci:

```
> :set args -o output.svg -w 4096
> :load Viz/TestChapo.hs
> main
> n: 3
> generating svg...
```

This will generate a file called "output.svg" which you can then open for example in your web browser.
