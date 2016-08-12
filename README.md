# Linear lambda calculus and graphs on surfaces

This repository gathers some code and data for exploring the
combinatorics of linear lambda calculus and its connections to the
theory of graphs on surfaces.  Besides browsing the
[source code](src/), you can also:

* Look at [related OEIS entries](doc/oeis.md)
* See [bibliographic references](doc/refs.md)

Some quick explanations of terminology:

* A lambda term is **linear** if every variable is used exactly once.
* A lambda term is **normal** if it contains no $\beta$-redices. It is **neutral** if it consists of a head variable applied to a sequence of normal terms.
* A linear term is **planar** if the list of free variables as the term is traversed grows like a stack, with lambda abstraction as "push" and variables as "pop". This actually gives rise to two different notions of planarity, depending on whether applications are traversed left-to-right (e.g., both `\x -> x(\y -> y)` and \x y -> y x` are LR-planar) or right-to-left (e.g., both `\x -> x(\y -> y)` and `\x y -> x y` are RL-planar).
* A linear term is **indecomposable** if it contains no closed proper subterms.
* Two normal linear terms are said to be equivalent modulo **free exchange** if one can be obtained from the other by a sequence of permutations of adjacent lambda abstractions.

Some other resources:

* Jason Reed's [lambda-maps repository](https://github.com/jcreedcmu/lambda-maps).
