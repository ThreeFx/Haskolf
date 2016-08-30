## Haskolf

Haskell, "optimized" for code golf (of course, written in Haskell). At its core it's a rename of Prelude functions
with automatic imports and shortcuts.

## How do I use it?

## Features

### Datatypes

Haskolf shortens the known datatypes to just a few letters:

Haskell ADT | Shortened
----------+----------
`Bool` | `B`
`True` | `T`
`False` | `F`
`Maybe` | `M`
`Nothing` | `N`
`Just` | `J`

(Well that's all for now)



### Shortened names

All functions commonly used in code golf have a shortened name / corresponding operator.
Normal Haskell doesn't support operator overloading

Haskell function | Shortened name | Operator
-----------------+----------------+----------
`(:)` | `c` | `(:)`
`(!!)` | `i` | `(!)`
`(++)` | `d` | `(@)`
`filter` | `` | `(?)`
`replicate` | `r` | `()`
`length` | `l` | -
---------+------------+----------
`(+)` | `a` | `(+)`
`(-)` | `s` | `(-)`
`(*)` | `m` | `(*)`
`(/)` | `d` | `(/)`
`(^)` | `e` | `(^)`
`(>)` | `g` | `(>)`
`(<)` | `l` | `(<)`
--------+----------+------------
`(&&)` | `a` | `(&)`
`(||)` | `o` | `(|)`
--------+----------+------------
`fmap` / `(<$>)` | `f` | `(#)`
`pure` / `return` | `p` | -
`ap` / `(<*>)` | `a` | `(~)`
`bind` | `b` | `(})` resp.: `({)`
`join` | `j` | -
`sequenceA` | `s` | -
`replicateM` | `` | `()`
