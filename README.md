# purescript-functor-vector
Vector operations heavily based in Functor composition


Representable Functors are container types that are basically the same as functions from some index type to the contained type. They are often big product types. The index type is in a sense the "logarithm" of the Functor type. Functor Composition leads to products of indices, and functor product leads to sum types of indices.

In the sense that vectors are objects that you can give an index and they return the value, this is a good match for vector-like types. This supplies a useful interface to fill out vectors given a function from the index type to the value you want to hold.

Scalar multiplication of vectors can be achieved via the Functor typeclass. Addition can be often achieved via the Applicative typeclass.

The Semiring typeclass is abused a little bit. Matrix multiplication of heterogenous types is achieved by specifying an instance of the Dottable typeclass. This typeclass can also be seen as a method to convert data types holding linear operators into their functional form.

The binary indexed vector V2 and the corresponding 2x2 matrix M2 give simple primitives for building up larger vectors. This corresponds to the useful concept of block matrices https://en.wikipedia.org/wiki/Block_matrix and allows matrix multiplication to be defined very naturally in purescript via simple recursion. Matrix inversion can be achieved recursively via the Schur Complement formula. For performance reasons, the base case of these recursion should involve more standard array based vector implementations of sufficient size, although it may be acceptable to recurse down to the Number base case.

Functor composition of these primitives allows one to build statically sized vectors and matrices with dimensionality that is powers of 2. 

Functor Composition of vector-like functors corresponds to the notion of taking a dense Kronecker product. Data.Functor.Compose is replicated somewhat in the module DenseKron, which is renamed so that it is more apparent that it is indeed the Kronecker product, and to add the necessary typeclasses. There are convenience data types defined for the iterated composition of functors. This is how we can easily build larger data types.

The HFunctor typeclass is a higher ordered functor, allowing one to replace one functor type with another in a data type. This is useful for swapping in and out FreeSemiRing layers for example.

Similarly Data.Functor.Product is equivalent to taking the direct sum of the spaces. This is in the module DirectSum.

The Dual of a vector space is the space of linear functor from vectors to their underlying field. This can be easily (minus the linear part) represented in Purescript.

The FreeSemiRing is one which freezes out every application of add, one, mul, or zero into a symbolic data structure, to be manipulated and consumed later. It is reminiscent of other Free encodings, such as the Free Monad. This allows for possible refactoring, simplification, and avoidance of huge unneccessary zero blocks in matrices. This recursive zero matrix labelling gives a kind of sparsity encoding.

The Free Kronecker product is an unexpanded dense kronecker product. This can be very useful. The size of storage increases exponentially with every densification of a new kronecker producted space. The Free Kronecker product expands very little at the cost of losing matrix invertibility and having space increase possibly exponentially (depending on the matrix in question) with every matrix multiplication. It is this Free Kronecker product that gives one of the very few acceptable approaches to very very large vector spaces, such as seen in perturbative many body quantum physics.


Adjacency Matrices using the MaxPlus semiring can be used to solve minimum path problems.








