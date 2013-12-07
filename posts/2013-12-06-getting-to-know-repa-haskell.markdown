---
title: Getting to know Repa in Haskell
mathjax: true
---

[Repa][], or **RE**gular **P**arallel **A**rrays,
is a fast unboxed multi-dimensional array
implementation in Haskell that
can help you achieve speeds near what you would
expect from C libraries.

It also has combinators that make it easier to run
parallel computation, though I was not able to use
them effectively in my initial use.

## My reason to learn it
For my Thanksgiving break, I was working on a significant
project for my introduction to probability for machine
learning course. 

I realized that many of my computations throughout this project
will be re-using many of the computations from before, so why
not break this down into a pipeline with matrices?

It did turn out to be much faster than lazily computed
list-monad implementations, which was necessary to
process a more data for better results.

## Equations
As part of the project, I rendered equations like 
$P(c|\underline{x}_i) =
\frac{\lambda_c \Pi_{j=1}^V \beta_{c,j}^{x_{i,j}}}
{\sum_{k=1}^{K}\lambda_k \Pi_{j=1}^V \beta_{k,j}^{x_{i,j}}}$
As you can see, the $\beta_{c,j}^{x_{i,j}}$
can be considered a result of a function that takes the
$\beta$ matrix, $x$ matrix, and indices $c$, $i$, and $j$.
Since this particular part happens both in the top and bottom,
it would be senseless to recompute it every time.
But, also considering the $\Pi_{j=1}^V$ portion,
we can actually do a matrix operation here and collapse
the results down to just two dimensions, $c$, and $i$.

Most operations were put into the log domain due
to the precision issue. I did try to only use ratios
as an experiment, but one EM-Step took 55 minutes.


I've heard of [accelerate][], but it seems tailored
mostly to [CUDA][], which I do not have support for on
my laptop. So, I opted for repa, hoping for a short
learning curve.

## The basics

For the rest of this document, refer to the following
type synonyms:

```Haskell
type DoubleMatrix = Array U (Z :. Int :. Int) Double
type IntMatrix = Array U (Z :. Int :. Int) Int
type DynMatrix = Array D (Z :. Int :. Int) Double
type DoubleVector = Array U (Z :. Int) Double
type BetaMap = DoubleMatrix
type Lambda = DoubleVector
```

So, given the top half of the fun equation, namely
$\Pi_{j=1}^V \beta_{c,j}^{x_{i,j}}$ (without the lambda),
we see that a loop is involved, and $c$ and $i$
should be given. However, instead of making it
a one-off function, let's just return a matrix
where for any coordinate `c,i`, we have stored
the result for this.

However, since this will be computed in the log
domain, the respective equation will be
$\sum_{j=1}^V x_{i,j} \cdot log \beta_{c,j}$

In Haskell, using the list monad, we can create
a 2D repa array, which acts like a matrix:

```Haskell
-- | Calculate the matrix, where entry c,j is as if
-- the answer to the single function call using c,j
logBetaXi2  :: BetaMap -- ^ The beta matrix
            -> DoubleMatrix -- ^ Our X_i,j count vector,
                            -- but as doubles.
            -> DoubleMatrix -- ^ Response matrix
logBetaXi2 b x = fromListUnboxed (Z :. cs :. is) $ do
    c <- [0..cs-1] -- Start with the first dimension (cs)
    i <- [0..is-1] -- Then the next dimension (is)
    let bs = do
        -- We loop over js times to get each piece
        -- which we later sum.
        j <- [0..js-1]
        -- do some lookups
        let xij = Repa.index x (Z :. i :. j)
            bcj = Repa.index b (Z :. c :. j)
        -- return the entry for this part of (c,i,j)
        return $ xij * log bcj
    -- Now fold the result with a sum to make the singular
    -- entry for (c,i)
    -- and return it.
    return $ sum bs
    where
        (Z :. cs :. js) = Repa.extent b
        -- Use the extents to get the current dimensions
        (Z :. is :. _) = Repa.extent x
```

Now that we have that kind of naive representation out of
the way, we can focus on making an equivalent version
just using repa operations!

```Haskell
logBetaXi :: BetaMap -> DoubleMatrix -> DoubleMatrix
logBetaXi bs xs = mmultS lbs xst
    where
        lbs = Repa.computeS $ Repa.map log bs
        xst = Repa.computeS $ Repa.transpose xs
```

W-wait, that was it!? Also, where did the `mmultS`
matrix multiply come from?
Well, there's a [repa-algortihms][mmult]
library which has this available. Though I really
dislike the restriction that the only things that
can go in are already-computed matrices.
It also has no warnings if the extents don't match
up for proper multiplication.
*To this end, I stole the implementation out of the
source and added my own check to stop me from making
stupid logical mistakes.*

So, let's see, what magic allowed me to do this?
First of all, we were doing a summation, and
a multiplication over an intermediary dimension $j$.
**This is a sure sign that what you're doing can be
a matrix multiply!**

But we can't just multiply $x_{i,j}$ as is. You cannot
multiply a $[i,j]$ matrix to a $[c,j]$ matrix. However
you can multiply a $[c,j]$ matrix to a $[j,i]$.
How do we go from a $[i,j]$ to a $[j,i]$?
It is really just a matter of a matrix transpose.
This results in a $[c,i]$ matrix, which is exactly
what we want!

Let's refer to the result of this operation as $BX$.

## The not as intuitive basics

Now back to that $\lambda_c$ from earlier.
We did not simply add it to the equation
because it was only a single dimensional
vector, and not a matrix. How then, do we
add it to $BX$?

In the naive list-based generation version,
it is trivial to add this to the looping.
But as a matrix operation,
how to we ensure that for each $c$, the correct
corresponding $\lambda_c$ is added to each entry
on $c,i$?

It turns out that this is really not difficult at
all. We take our vector $\lambda$, which is of size
$c$ (in the sense of $BD$ being $c,i$ in it's size),
and create a new matrix which has $\lambda$ replicated
$i$ times. We now have a matrix of size $c,i$ that we
can add to $BD$.

For the entire equation that computes
$P(c|\underline{x}_i)$,
we have the following:

```Haskell
logP_C_Given_Xi :: Lambda -> BetaMap -> DoubleMatrix -> DynMatrix
logP_C_Given_Xi l b x = s2
    where
        -- Get our dimensions, the last parameter
        -- being the js, but we don't care.
        (Z :. cs :. _) = Repa.extent b
        (Z :. is :. _) = Repa.extent x
        -- Operation of adding the extended lambda matrix
        -- mentioned to BX.
        s1 = repaZipAssure (+) lcs bxs
        -- Because we are in the log domain, we do a
        -- subtraction of the denominator.
        s2 = repaZipAssure (-) s1 lsbxse
        -- perform the log on lambda
        ll = Repa.map log l
        -- Now extend it to be i wide.
        lcs = Repa.extend (Any :. is) ll
        -- Now get our BX matrix
        bxs = logBetaXi b x
        -- The bottom iterates over the c's and
        -- not the i's. So we need to transpose before
        -- we sum into a vector (and not a matrix).
        -- Folds take place on the "innermost" dimension.
        s1t = Repa.transpose s1
        -- 'logadd' is a special function that
        -- assists with adding in the log domain
        -- don't worry about it's implementation
        -- only the fact that e^-infinity is 0.
        lsbxs = Repa.foldS logadd (-1/0) s1t
        -- Now extend it back so we can properly
        -- subtract it.
        lsbxse = Repa.extend (Any :. cs :. All) lsbxs
```
Note that I am using some yet-to-be defined `repaZipAssure`
function. This is merely a zip that requires the dimensions
to be exactly equal, unlike the available one which goes
with the minimum. Otherwise, it is literally just
`Repa.zipWith f a b`, where `f` is a function, and `a` and
`b` are equivalently sized arrays.


## Conclusion

Previous attempts used maps and sets in attempt to
solve this problem, and folding over maps and sets,
while building a map or set using an accumulator
quickly became

+ Less easy to understand
+ Painful error messages when typing became an issue
+ Performance was terrible

Repa was magnitudes faster than a naive implementation
in my class, where we used **our language of choice**.

I used Haskell because it seemed (and was) easier
to express problems like these equations, and test
them than what you might expect in something like
C++ or Java. *Python has it much easier thanks to
[numPy][] and the various containers available.*

Repa is wonderful for when you know lazy evaluation
will only get in the way. In the case shown above,
each and every entry in the $\beta$ matrix was used
to construct the values in the resultant matrix for
the vector of probability distributions (which is
can be thought of as a matrix).

In case you plan to do numerical processing, and
your data can be represented as vectors of
vectors--or a matrix--then repa is a good starting
place for you. 

[repa]: http://hackage.haskell.org/package/repa
[accelerate]: http://hackage.haskell.org/package/accelerate
[cuda]: http://www.nvidia.com/object/cuda_home_new.html
[mmult]: http://hackage.haskell.org/package/repa-algorithms-3.2.4.1/docs/Data-Array-Repa-Algorithms-Matrix.html
[numpy]:http://www.numpy.org/
