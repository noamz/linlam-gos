# Linear lambda calculus and graphs on surfaces

This repository gathers some code and data for exploring the
combinatorics of linear lambda calculus and its connections to the
theory of graphs on surfaces.  Besides browsing the
[source code](src/), you can also:

* Look at [related OEIS entries](doc/oeis.md)
* Examine [tables of lambda terms](tables/)
* Reflect upon various [diagrams](diagrams/)
* Find [sage worksheets](sage/) for computing generating functions
* Read some [bibliographic references](doc/refs.md)

Some quick explanations of terminology:

* A lambda term is **linear** if every variable is used exactly once (e.g., `\x y -> x y` and `(\x -> x)(\y -> y)` but not `\x y -> x (x y)` or `\x y -> y`).
* A lambda term is **normal** if it contains no β-redices (e.g., `\x y -> x y` and `\x -> x(\y -> y)` but not `(\x -> x)(\y -> y)`). It is **neutral** if it consists of a head variable applied to a sequence of normal terms (e.g., `x y` and `x(\y -> y)` but not `\x -> x(\y -> y)`).
* A linear term is **planar** if in traversing its tree of subterms, the context of free variables grows like a stack (reading lambda abstraction as "push" and variable occurrences as "pop"). This actually gives rise to two different notions of planarity, depending on whether binary application nodes are traversed left-to-right (e.g., both `\x -> x(\y -> y)` and `\x y -> y x` are LR-planar) or right-to-left (e.g., both `\x -> x(\y -> y)` and `\x y -> x y` are RL-planar).
* A linear term is **indecomposable** if it contains no closed proper subterms (e.g., `\x y -> x y` but not `\x -> x(\y -> y)`).
* Two normal linear terms are said to be equivalent modulo **free exchange** if one can be obtained from the other (up to α-conversion) by a sequence of permutations of adjacent lambda abstractions (e.g., `\x y -> x y` and `\y x -> x y` ==α `\x y -> y x` are equivalent modulo free exchange, but `\x y -> y (\z -> z x)` and `\x y -> y (\z -> x z)` are not equivalent modulo free exchange).

Some other resources:

* Jason Reed's [lambda-maps repository](https://github.com/jcreedcmu/lambda-maps).
