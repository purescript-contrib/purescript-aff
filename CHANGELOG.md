# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v6.0.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#196)

New features:
- Added roles declarations to allow safe coercions (#194) 

Bugfixes:
- Fixed typo in `forkAff` documentation (#184)

Other improvements:
- Added disclaimer about non-working examples in README (#188)
- Changed default branch to `main`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#195)

## [v5.1.2](https://github.com/purescript-contrib/purescript-aff/releases/tag/v5.1.2) - 2019-09-11

- Try to recover when exceptions are thrown in pure code (@ford-prefect)
- Fixed memory leak in supervisors where child fibers are retained even though they have completed (@eric-corumdigital)

## [v5.1.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v5.1.1) - 2019-03-29

- Fixed supervision when no child fibers are active (#164)
- Fixed various bugs around resumption within a bracket mask (#171)

## [v5.1.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v5.1.0) - 2018-12-14

- Added `fiberCanceler` (@safareli)

## [v5.0.2](https://github.com/purescript-contrib/purescript-aff/releases/tag/v5.0.2) - 2018-08-24

- No longer resumes from an enqueued task if interrupted (#162)
- Fixed finalization after failure (#161)

## [v5.0.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v5.0.1) - 2018-07-12

- Fixed runtime error when running an async canceler in a `ParAff` `apply` operation (#153)

## [v5.0.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v5.0.0) - 2018-05-25

- Updated for PureScript 0.12

## [v4.1.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.1.1) - 2018-04-18

- Fixes `bhead is not a function` FFI errors when yielding a fork at the tail of a fresh attempt context (possibly through `bracket` acquisition or `catchError`).

## [v4.1.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.1.0) - 2018-04-06

- Added `Lazy` instance for `Aff` (@safareli)

## [v4.0.2](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.2) - 2018-01-14

- Fixed regression in ParAff Applicative behavior when an exception occurs.

## [v4.0.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.1) - 2017-11-19

- Fixed JavaScript runtime error in `ParAff` cancellation.

## [v4.0.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.0) - 2017-09-14

This release (v4.0.0) features a revamped API for writing more expressive asynchronous programs with stronger guarantees.

- `Fiber` cooperative multi-tasking primitive for fork/join workflows.
- Stronger cleanup guarantees with `bracket` and `supervise`.
- Reformulated `AVar` semantics
- Rewritten core with an emphasis on performance and consistency.

**New Features and Enhancements**

**`Fiber`**

Previously, `Aff`s supported `forkAff`, but it was very difficult to get values back when forked computations completed. Libraries like `purescript-aff-future` were written to overcome this limitation (though with limitations of their own). The semantics of `purescript-aff-future` have been assimilated into `Aff` through the `Fiber` type without any of the previous limitaions (like lack of cancellation). `Fiber`s make it easy to not only fork computations, but also share their results among many consumers with `joinFiber`, which will wait until the `Fiber` completes, or yield immediately if it has already resolved. If a `Fiber` threw an exception, then the exception will be rethrown in the observer. `Fiber`s additionally support cancellation (`killFiber`) and finalizers for cleanup (`bracket`).

**`bracket`**

When we kill/cancel a `Fiber`, often times we need to make sure some resource gets released. `bracket` lets you take control of the acquire/use/release resource cycle.

```purescript
example =
  bracket
    (openFile myFile) -- Acquire a resource
    (\file -> closeFile file) -- Release the resource
    (\file -> appendFile "hello" file) -- Use the resource
```

In the example above, the runtime will always ensure the "release" effect will run even in the presence of cancellation. There is also `generalBracket`, which lets you observe whether the primary action completed successfully, threw an exception, or was killed asynchronously and run different cleanup effects accordingly.

**`supervise`**

Sometimes we need to fork many `Fiber`s for a task, but it's possible (often through cancellation) for these sub-tasks to leak. We've introduced a `supervise` combinator which will automatically keep track of forked `Fiber`s and clean them up and run their finalizers once the computation completes or is cancelled.

```purescript
example = supervise do
  _ <- forkAff requestA
  _ <- forkAff requestB
  requestC
```

In the above example, if `requestA` or `requestB` are still running when `requestC` completes, they will be killed by the runtime and have their finalizers run.

**`suspendAff`**

As an alternative to `forkAff`, which eagerly forks and runs a computations, we've introduced `suspendAff`. This forks a computation but does not initiate it until a result is demanded via `joinFiber`. Results are still memoized (as all `Fiber` results are), but are just computed lazily.

**Stack-safety**

With the old callback approach, each bind resulted in more and more stack, and it was trivial to blow the stack unless you explicitly used `tailRecM`. The `Aff` interpreter now uses a constant amount of stack space, making `tailRecM` unnecesary. This extends to `ParAff` as well.

**Uncaught exceptions**

Previously, exceptions thrown in forked computations were completely swallowed. This made it extremely difficult to diagnose bugs. Now if a `Fiber` has no observers and it throws an exception, the exception will always be rethrown in a fresh stack. This can be observed by things like `window.onerror` or just by watching the console.

**Breaking Changes**

- The low-level callback representation is no longer relevant. If you've defined `Aff` effects via the FFI, you should transition to using `Control.Monad.Aff.Compat`, which provides an `EffFn` adapter. This makes it easy to use idiomatic JavaScript callbacks when building `Aff` actions.
- The `AVar` API methods have changed to match Haskell's `MVar` API. `putVar` now _blocks_ until the `AVar` actually assimilates the value. Previously, `putVar` would queue the value, but yield immediately. It's possible to recover similar behavior as the old API with `forkAff (try (putVar value avar))` (though this should be considered mildly unsafe), or you can use `tryPutVar` which will attempt a synchronous put.
- Argument order for `AVar` and `Fiber` operations consistently put the subject last.
- Several unlawful instances where removed for `Aff` (`Alternative`, `MonadPlus`, and `MonadZero`).
- `forkAff` now returns a `Fiber` rather than a `Canceler`.
- `forkAll` was removed. Just use `Traversable` and `forkAff` directly.
- `cancel` was removed. Use `killFiber`.
- The signature of `makeAff` has changed to provide a _single_ callback which takes an `Either Error a` argument. `Canceler`s are also _required_. If you are sure you have no means of cancelling an action, you can use `nonCanceler` or `mempty`.
- `Canceler`s no longer yield `Boolean`. This was meaningless and not useful, so all cancellation effects now yield `Unit`.
- `ParAff` is no longer a newtype. Parallel computations should be constructed via `Control.Parallel` with `parallel` and `sequential`.

## [v4.0.0-rc.6](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.0-rc.6) - 2017-09-12

- Rename `atomically` to `invincible`.
- Killing a suspended fiber should be synchronous.

## [v4.0.0-rc.5](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.0-rc.5) - 2017-08-31

- Changed the argument order of `AVar` operations to have the AVar last.

## [v4.0.0-rc.4](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.0-rc.4) - 2017-08-26

- Reexport things in `Control.Monad.Eff.Exception` relevant to the `Aff` API.

## [v4.0.0-rc.3](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.0-rc.3) - 2017-08-24

- `kill` always succeeds. If a finalizer throws, it will rethrow in a fresh stack.
- Fixes the behavior of `throwError` and `generalBracket` within a finalizer.

## [v4.0.0-rc.2](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.0-rc.2) - 2017-08-20

- Fixes `ParAff` `Alt` behavior when propagating exceptions.

## [v4.0.0-rc.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v4.0.0-rc.1) - 2017-08-19

This pre-release for version v4.0.0 features a revamped API for writing more expressive asynchronous programs with stronger guarantees.

- `Fiber` cooperative multi-tasking primitive for fork/join workflows
- Stronger cleanup guarantees with `bracket` and `supervise`.
- Reformulated `AVar` semantics

**Migration Notes**

- The low-level callback representation is no longer relevant. If you've defined `Aff` effects via the FFI, you should transition to using `Control.Monad.Eff.Compat`, which provides an `EffFn` adapter.
- The `AVar` API methods have changed to match Haskell's `MVar` API. `putVar` effects now block until matched with a `takeVar`. It's possible to recover similar behavior as the old API with `forkAff (try (putVar avar value))`.
- Several unlawful instances where removed for `Aff` (`Alternative`, `MonadPlus`, and `MonadZero`).

## [v3.1.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v3.1.0) - 2017-05-08

- Added `tryTakeVar` and `tryPeekVar` (@syaiful6)

## [v3.0.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v3.0.0) - 2017-04-02

- Updated to work with PureScript 0.11
- Removed `later` and `later'` in favor of `delay`.

## [v2.0.3](https://github.com/purescript-contrib/purescript-aff/releases/tag/v2.0.3) - 2017-02-18

- Avoid `Discard` constraint arising in upcoming PureScript version (@mlang)

## [v2.0.2](https://github.com/purescript-contrib/purescript-aff/releases/tag/v2.0.2) - 2016-12-07

- Fixed broken `peekVar` (@natefaubion)
- Fixed `Alt` instance for parallel to cancel the "losing" side (@garyb)

## [v2.0.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v2.0.1) - 2016-10-23

- Fixed `Applicative` instance for `ParAff` #76

## [v2.0.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v2.0.0) - 2016-10-21

- Updated dependencies, now compatible with PureScript 0.10
- Added `peekVar` that reads from an `AVar` without consuming

## [v1.1.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v1.1.0) - 2016-07-24

- Added functions to `Control.Monad.Aff.Console` to match `Control.Monad.Eff.Console`, re-export `CONSOLE` effect (@texastoland)

## [v1.0.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v1.0.0) - 2016-06-09

- Initial 1.0.0 release

## [v0.17.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.17.0) - 2016-06-05

- Requires PureScript 0.9.1 or later.

## [v0.16.2](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.16.2) - 2016-06-01

- The `MonadRec` instance now preserves synchronous semantics instead of periodically bouncing asynchronously.

## [v0.16.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.16.0) - 2016-03-11

- Removed `Affable` class again
- `MonadAff` is now a subclass of `MonadEff`

## [v0.15.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.15.0) - 2016-03-09

- Added `Affable` class
- `MonadAff` now is a subclass of `Affable` and `Monad`

## [v0.14.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.14.1) - 2016-02-22

- Added `forkAll` combinator, for forking many asynchronous computations in a synchronous manner (@natefaubion)

## [v0.13.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.13.1) - 2015-11-19

- Fixed warnings

## [v0.13.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.13.0) - 2015-09-22

- Bumped transformers dependency

## [v0.11.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.11.0) - 2015-07-02

- This release is intended for 0.7 of the PureScript compiler.

## [v0.10.1](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.10.1) - 2015-04-18

- This release fixes a number of bugs related to cancelation, and greatly improves the semantics of cancelation. Several tests have been added or made more sophisticated.

## [v0.10.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.10.0) - 2015-04-14

This release adds a newtype for `Canceler` so that `Semigroup` and `Monoid` instances may be defined, allowing easy composition of multiple cancelers into a single canceler.

In addition, `Par` has been rewritten to use a new generic, exported `cancelWith` combinator, so that canceling a parallel computation will now cancel the individual components of the computation.

This is mostly a drop-in upgrade, but if you created your own cancelers before, you'll now need to use the newtype.

## [v0.7.0](https://github.com/purescript-contrib/purescript-aff/releases/tag/v0.7.0) - 2015-03-24

This release includes the following enhancements:
- Native representation of `Aff` which minimizes the amount of wrapping that is performed. This should be much faster than the previous version.
- Automatically catch exceptions and propagate along the error channel, so that, for example, ill-typed `Eff` code won't halt an `Aff` application.
- Forked computations can be killed if the computation supports it.
- The effect type of asynchronous code has been simplified; in particular, the mere act of being asynchronous is not considered an effect.
- Documentation has been updated to account for the changes.
