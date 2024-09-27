# Revision history for reflex

## Unreleased

* Add support for GHC 9.8 and 9.10

## 0.9.3.0

* Headless Host: Generalize to allow returning arbitrary types

## 0.9.2.0

* Add MonadMask, MonadCatch, MonadThrow instances

## 0.9.1.0

* Headless Host: Add some MonadSample, MonadHold, and MonadFix instances

## 0.9.0.1

* Add support for ghc-9.6

## 0.9.0.0

* Breaking Change: Filter updates to `listWithKey` child widgets so that changes to the input Map don't cause spurious updates to unaffected children. This imposes an `Eq` constraint on the child values.
* Expose all Requester internals in Reflex.Requester.Base.Internal
* [Add EventWriter instance for RequesterT #469](https://github.com/reflex-frp/reflex/pull/469)

## 0.8.2.2

* Require witherable >= 0.4 and, hence, a newer monoidal-containers
* Support newer constraints-extras (0.4)

## 0.8.2.1

* Fix build for GHC 9.2
* Require patch >= 0.0.7.0

## 0.8.2.0

* Add `matchResponseMapWithRequests`, which it similar to `matchResponsesWithRequests` but allows processing of multiple responses at once.

## 0.8.1.1

* Allow newer hlint for older GHCs, and add upper bound for newer GHCs

## 0.8.1.0

* Add support for GHC 8.10
* Drop support for GHC < 8.4

## 0.8.0.0

* Replace 0.7.2.0 with 0.8.0.0 to reflect the `MonadHold` interface change. Deprecates 0.7.2.0.

## 0.7.2.0 -- *Deprecated*

* ([#416](https://github.com/reflex-frp/reflex/pull/416)) Add `now :: m (Event t ())` to `MonadHold`.
* Extend some dependency version bounds
* Fix HLint 3 test

## 0.7.1.1

*Backport release*.
Changes do not carry forward to 0.7.2.0.

* Add support for GHC 8.10
* Drop support for GHC < 8.4
* Extend some dependency version bounds
* Fix HLint 3 test

## 0.7.1.0

* ([#413](https://github.com/reflex-frp/reflex/pull/413), [#417](https://github.com/reflex-frp/reflex/pull/417)) Add `Reflex.Host.Headless` module which provides `runHeadlessApp` as an easy way to run a Reflex network in a "headless" environment.
* ([#420](https://github.com/reflex-frp/reflex/pull/420)) Add a [`Data.Zip.Unzip`](https://hackage.haskell.org/package/semialign-1.1/docs/Data-Zip.html#t:Unzip) instance for `Event`.
* ([#419](https://github.com/reflex-frp/reflex/pull/419)) Add `distributeIntMapOverDynPure` and `joinDynThroughIntMap` as convenience functions for working with `Dynamic` `IntMap`s.


## 0.7.0.0

* Add lifting instances for most classes to `Reflex.Profiled.Profiled`. ([#398](https://github.com/reflex-frp/reflex/pull/398))
* Class `MonadQuery t q m` now has a `Monad m` superclass constraint. ([#400](https://github.com/reflex-frp/reflex/pull/400))
* **(Breaking change)** Rename class `MonadBehaviorWriter` -> `BehaviorWriter` for consistency with `EventWriter`/`DynamicWriter`. ([#401](https://github.com/reflex-frp/reflex/pull/401))
* Introduce deprecated alias `MonadBehaviorWriter = BehaviorWriter`. ([#401](https://github.com/reflex-frp/reflex/pull/401))
* Fix bug in spider where event subscriptions would be prematurely finalized due to over-aggressive inlining. ([#409](https://github.com/reflex-frp/reflex/pull/409))
* Add instances of `PerformEvent` and `TriggerEvent` for `MaybeT`. ([#395](https://github.com/reflex-frp/reflex/pull/395))

## 0.6.4.1

* Fix a bug in the Reflex Profiled transformer where
  `Reflex.Class.mergeIncrementalG` and
  `Reflex.Class.mergeIncrementalWithMoveG` implementations referenced
  itself instead of the inner transformed timeline, causing an
  infinite loop.

## 0.6.4

* Support GHC 8.8

* Add `Reflex.Query.Base.mapQueryT`. See that module for documentation

* The `Reflex.Patch.*` modules were moved to the `patch` library.
  They are `Data.Patch.*` there, but reexported under their old names for backwards compatability here.

* Additional instances for `Query` classes for basic types.

* Add cabal flags `debug-propagation` and `debug-event-cycles` to build in debugging
  code for performance and for cyclic dependencies between events

## 0.6.3

* `Data.WeakBag.traverse` and `Data.FastWeakBag.traverse` have been deprecated.
  They are replaced with `Data.WeakBag.traverse_` and `Data.FastWeakBag.traverse_`, respectively.

* Fixes a bug in `Reflex.Patch.MapWithMove.patchThatSortsMapWith` that was producing invalid `PatchMapWithMove`.

* Add missing `NotReady` instances:
   - `instance NotReady (SpiderTimeline x) (SpiderHost x)`
   - `instance HasSpiderTimeline x => NotReady (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x))`

## 0.6.2.4

* Update to monoidal-containers 0.6

## 0.6.2.3

* Add an upper-bound to witherable

## 0.6.2.2

* Support these >= 1. Add `split-these` flag to control whether to use new these/semialign combination or not.

* Update version bounds to fix some CI failures

* Add travis CI configuration

## 0.6.2.1

* Generalize `fan` to `fanG` to take a `DMap` with non-`Identity`
  functor:
    * `fan` to `fanG`
    * `EventSelectorG` for `fanG` result selector.

* Reduce the amount of unsafeCoerce in coercing newtypes under Event/Dynamic/Behavior.
    * Add fused ReaderIO for the purpose of coercion (ReaderT's third argument has nominal role preventing automated coerce)
    * Add incrementalCoercion/coerceIncremental to go with dynamicCoercion/coerceDynamic

* Generalize merging functions:
  `merge` to `mergeG`,
  `mergeIncremental` to `mergeIncrementalG`,
  `mergeIncrementalWithMove` to `mergeIncrementalWithMoveG`.

* Generalize distribute function:
    `distributeDMapOverDynPure` to `distributeDMapOverDynPureG`,

## 0.6.2.0

* Fix `holdDyn` so that it is lazy in its event argument
  These produce `DMap`s  whose values needn't be `Identity`.

* Stop using the now-deprecated `*Tag` classes (e.g., `ShowTag`).

* Fix `holdDyn` so that it is lazy in its event argument.

## 0.6.1.0

* Re-export all of `Data.Map.Monoidal`

* Fix `QueryT` and `RequesterT` tests

## 0.6.0.0 -- 2019-03-20

* Deprecate `FunctorMaybe` in favor of `Data.Witherable.Filterable`. We still export `fmapMaybe`, `ffilter`, etc., but they all rely on `Filterable` now.

* Rename `MonadDynamicWriter` to `DynamicWriter` and add a deprecation for the old name.

* Remove many deprecated functions.

* Add a `Num` instance for `Dynamic`.

* Add `matchRequestsWithResponses` to make it easier to use `Requester` with protocols that don't do this matching for you.

* Add `withRequesterT` to map functions over the request and response of a `RequesterT`.

* Suppress nil patches in `QueryT` as an optimization. The `Query` type must now have an `Eq` instance.

* Add `throttleBatchWithLag` to `Reflex.Time`. See that module for details.
