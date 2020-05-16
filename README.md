# simsim
[![Build Status](https://travis-ci.org/atcol/simsim.svg?branch=master)](https://travis-ci.org/atcol/simsim)
[![Simple Haskell](http://simplehaskell.org/badges/badge.svg)](http://simplehaskell.org)

Simplified simulation API for the layman.

## Philosophy

Simsim aims to provide a comprehensive API for simulation that is:

 * accessible  - no over-baked APIs or excessive abstractions, adhering to [SimpleHaskell](http://simplehaskell.org/)'s philosophy
 * generalised - there is no constraint on the domain of simulation
 * productive  - an easy programming model light on boilerplate that enables productive creation of simulations
 * concurrent  - the framework is concurrent in all its abstractions, where necessary

## Getting Started

### Core Types

The basic `Actor m a` type provides the interface for a simple unit of behaviour that mutates state to participate 
in a simulation.

That is, an `Actor` is to take its last (or initial) state, mutate it according to its behavioural
traits, and then return the new state along with a status of `Continue` or `Terminate`.

A very simple example is as follows:

```
incrementor :: Actor IO Int
incrementor = do
  (g, s@(ActorSimState i sim)) <- ask
  return (Continue, g, s { astValue = i + 1 })

main = do
  results <- runSim [(incrementor, 1)]
```

where `Actor` is a `ReaderT` in the `IO` monad that operates on `Int`s. The 
actor merely increments the last integer by 1 and returns. This would continue 
indefinitely until the `Actor` returns `Terminate`, the first element in the return
triple.

More comprehensive examples can be found in the [test](./test) directory.

## Build

`stack install`


## Run Tests

`stack test`
