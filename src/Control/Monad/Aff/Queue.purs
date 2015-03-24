-- | A low-level primitive for building asynchronous code.
module Control.Monad.Aff.Queue
  ( AffQueue()
  , Queue()
  , QueueFx()
  , killQueue
  , makeQueue
  , makeQueue'
  , modifyQueue
  , putQueue
  , takeQueue
  ) where

  import Data.Function(Fn2(), Fn3(), runFn2, runFn3)
  import Control.Monad.Aff
  import Control.Monad.Eff.Exception(Error())

  foreign import data QueueFx :: !

  foreign import data Queue :: * -> *

  type AffQueue e a = Aff (queue :: QueueFx | e) a

  -- | Makes a new asynchronous queue.
  makeQueue :: forall e a. AffQueue e (Queue a)
  makeQueue = _makeQueue nonCanceler

  -- | Makes a queue and sets it to some value.
  makeQueue' :: forall e a. a -> AffQueue e (Queue a)
  makeQueue' a = do
    v <- makeQueue 
    putQueue v a
    return v

  -- | Takes the next value from the asynchronous queue.
  takeQueue :: forall e a. Queue a -> AffQueue e a
  takeQueue q = runFn2 _takeQueue nonCanceler q

  -- | Puts a new value into the asynchronous queue. If the queue has
  -- | been killed, this will result in an error.
  putQueue :: forall e a. Queue a -> a -> AffQueue e Unit
  putQueue q a = runFn3 _putQueue nonCanceler q a

  -- | Modifies the value at the head of the queue (will suspend until one is available).
  modifyQueue :: forall e a. (a -> a) -> Queue a -> AffQueue e Unit
  modifyQueue f v = takeQueue v >>= (f >>> putQueue v)

  -- | Kills an asynchronous queue.
  killQueue :: forall e a. Queue a -> Error -> AffQueue e Unit
  killQueue q e = runFn3 _killQueue nonCanceler q e

  foreign import _makeQueue """
    function _makeQueue(canceler) {
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

        return canceler;
      }
    }
  """ :: forall e a. Canceler e -> AffQueue e (Queue a)

  foreign import _takeQueue """
    function _takeQueue(canceler, queue) {
      return function(success, error) {
        if (queue.error !== undefined) {
          error(queue.error);
        } else if (queue.producers.length > 0) {
          var producer = queue.producers.shift();

          producer(success, error);
        } else {
          queue.consumers.push({success: success, error: error});
        }

        return canceler;
      } 
    }
  """ :: forall e a. Fn2 (Canceler e) (Queue a) (AffQueue e a)
  
  foreign import _putQueue """
    function _putQueue(canceler, queue, a) {
      return function(success, error) {
        if (queue.error !== undefined) {
          error(queue.error);
        } else if (queue.consumers.length === 0) {
          queue.producers.push(function(success, error) {
            try {
              success(a);
            } catch (e) {
              error(e);
            }
          });

          success({});
        } else {
          var consumer = queue.consumers.shift();

          try {
            consumer.success(a);
          } catch (e) {
            error(e);

            return;                  
          }

          success({});
        }

        return canceler;
      }
    }
  """ :: forall e a. Fn3 (Canceler e) (Queue a) a (AffQueue e Unit)

  foreign import _killQueue """
    function _killQueue(canceler, queue, e) {
      return function(success, error) {
        if (queue.error !== undefined) {
          error(queue.error);
        } else {
          var errors = [];

          queue.error = e;

          while (queue.consumers.length > 0) {
            var consumer = queue.consumers.shift();

            try {
              consumer.error(e);
            } catch (e) {
              errors.push(e);              
            }
          }

          if (errors.length > 0) error(errors[0]);
          else success({});
        }

        return canceler;
      }
    }
  """ :: forall e a. Fn3 (Canceler e) (Queue a) Error (AffQueue e Unit)

