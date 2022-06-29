# Aff

[![CI](https://github.com/purescript-contrib/purescript-aff/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-aff/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-aff.svg)](https://github.com/purescript-contrib/purescript-aff/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-aff/badge)](https://pursuit.purescript.org/packages/purescript-aff)
[![Maintainer: natefaubion](https://img.shields.io/badge/maintainer-natefaubion-teal.svg)](https://github.com/natefaubion)

An asynchronous effect monad and threading model for PureScript.

## Installation

Install `aff` with [Spago](https://github.com/purescript/spago):

```sh
spago install aff
```

## Quick start

This quick start covers common, minimal use cases for the library. Longer examples and tutorials can be found in the [docs directory](./docs).

```purescript
main :: Effect Unit
main = launchAff_ do
  response <- Ajax.get "http://foo.bar"
  log response.body
```

## Effect and Aff

`Effect` is a synchronous effect monad. It is used to sequence effectful foreign (JavaScript) code — things like random number generation, reading and writing mutable values, writing to the console and throwing and catching exceptions.[<sup>1</sup>](https://stackoverflow.com/questions/37661391/what-are-eff-and-aff)

`Aff` is an asynchronous effect monad. It can handle and sequence effectful asynchronous code, like AJAX requests, timeouts, and network and file IO. 
It also provides a nice mechanism for handling errors.

Furthermore, `Aff` can perform synchronous effects by using `liftEffect`. In other words, `Effect` is a special case of `Aff`, for the special case that we expect the effect to complete immediately. `Effect` is in the core libraries instead of `Aff` for two reasons.

1. The implementation of `Effect` is much simpler than `Aff`, which makes a big difference for non-JavaScript backends.
2. `Effect` is much faster than `Aff`, and we expect most effects to complete synchronously, so usually `Effect` will suffice.

Many I/O-related packages in the PureScript ecosystem provide both an asynchronous callback-based `Effect` API and an `Aff` API for the same feature. When you encounter this, you should almost always prefer the `Aff` API. You will find the `Aff` API is much simpler to use correctly. 

## Documentation

`aff` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-aff).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-aff/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `aff` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-aff/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
