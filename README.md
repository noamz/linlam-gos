# Linear lambda calculus and graphs on surfaces

This repository gathers some code and data for exploring the
combinatorics of linear lambda calculus and its connections to the
theory of graphs on surfaces.  Besides browsing the
[source code](src/), you can also:

* Look at [related OEIS entries](doc/oeis.md)
* See [bibliographic references](doc/refs.md)

Some quick explanations of _terminology_:

* A lambda term is __linear__ if every variable is used exactly once.
* A lambda term is __normal__ if it contains no $\beta$-redices. It is __neutral__ if it consists of a head variable applied to a sequence of normal terms.
* A linear term is __planar__ if the list of free variables as the term is traversed grows like a stack, with lambda abstraction as "push" and variables as "pop". This actually gives rise to two different notions of planarity, depending on whether applications are traversed left-to-right (e.g., $\lambda x.\lambda y.y(x)$ is LR-planar) or right-to-left (e.g., $\lambda x.\lambda y.x(y)$ is RL-planar).
* A linear term is __indecomposable__ if it contains no closed proper subterms.
* Two normal linear terms are said to be equivalent modulo __free exchange__ if one can be obtained from the other by a sequence of permutations of adjacent lambda abstractions.

Some other resources:

* Jason Reed's [lambda-maps repository](https://github.com/jcreedcmu/lambda-maps).
