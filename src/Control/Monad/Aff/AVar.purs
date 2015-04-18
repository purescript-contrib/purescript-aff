-- | A low-level primitive for building asynchronous code.
module Control.Monad.Aff.AVar
  ( AffAVar()
  , AVar()
  , AVAR()
  , killVar
  , makeVar
  , makeVar'
  , modifyVar
  , putVar
  , takeVar
  ) where

  import Data.Function(Fn2(), Fn3(), runFn2, runFn3)
  import Control.Monad.Aff
  import Control.Monad.Eff.Exception(Error())

  foreign import data AVAR :: !

  foreign import data AVar :: * -> *

  type AffAVar e a = Aff (avar :: AVAR | e) a

  -- | Makes a new asynchronous avar.
  makeVar :: forall e a. AffAVar e (AVar a)
  makeVar = _makeVar nonCanceler

  -- | Makes a avar and sets it to some value.
  makeVar' :: forall e a. a -> AffAVar e (AVar a)
  makeVar' a = do
    v <- makeVar 
    putVar v a
    return v

  -- | Takes the next value from the asynchronous avar.
  takeVar :: forall e a. AVar a -> AffAVar e a
  takeVar q = runFn2 _takeVar nonCanceler q

  -- | Puts a new value into the asynchronous avar. If the avar has
  -- | been killed, this will result in an error.
  putVar :: forall e a. AVar a -> a -> AffAVar e Unit
  putVar q a = runFn3 _putVar nonCanceler q a

  -- | Modifies the value at the head of the avar (will suspend until one is available).
  modifyVar :: forall e a. (a -> a) -> AVar a -> AffAVar e Unit
  modifyVar f v = takeVar v >>= (f >>> putVar v)

  -- | Kills an asynchronous avar.
  killVar :: forall e a. AVar a -> Error -> AffAVar e Unit
  killVar q e = runFn3 _killVar nonCanceler q e

  foreign import _makeVar """
    function _makeVar(nonCanceler) {
      return function(success, error) {
        try {
          success({
            consumers: [],
            producers: [],
            error: undefined 
          });
        } catch (e) {
          error(e);
        }

        return nonCanceler;
      }
    }
  """ :: forall e a. Canceler e -> AffAVar e (AVar a)

  foreign import _takeVar """
    function _takeVar(nonCanceler, avar) {
      return function(success, error) {
        if (avar.error !== undefined) {
          error(avar.error);
        } else if (avar.producers.length > 0) {
          var producer = avar.producers.shift();

          producer(success, error);
        } else {
          avar.consumers.push({success: success, error: error});
        }

        return nonCanceler;
      } 
    }
  """ :: forall e a. Fn2 (Canceler e) (AVar a) (AffAVar e a)
  
  foreign import _putVar """
    function _putVar(nonCanceler, avar, a) {
      return function(success, error) {
        if (avar.error !== undefined) {
          error(avar.error);
        } else if (avar.consumers.length === 0) {
          avar.producers.push(function(success, error) {
            try {
              success(a);
            } catch (e) {
              error(e);
            }
          });

          success({});
        } else {
          var consumer = avar.consumers.shift();

          try {
            consumer.success(a);
          } catch (e) {
            error(e);

            return;                  
          }

          success({});
        }

        return nonCanceler;
      }
    }
  """ :: forall e a. Fn3 (Canceler e) (AVar a) a (AffAVar e Unit)

  foreign import _killVar """
    function _killVar(nonCanceler, avar, e) {
      return function(success, error) {
        if (avar.error !== undefined) {
          error(avar.error);
        } else {
          var errors = [];

          avar.error = e;

          while (avar.consumers.length > 0) {
            var consumer = avar.consumers.shift();

            try {
              consumer.error(e);
            } catch (e) {
              errors.push(e);              
            }
          }

          if (errors.length > 0) error(errors[0]);
          else success({});
        }

        return nonCanceler;
      }
    }
  """ :: forall e a. Fn3 (Canceler e) (AVar a) Error (AffAVar e Unit)

