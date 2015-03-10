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

  import Control.Monad.Aff
  import Control.Monad.Eff.Exception(Error())

  foreign import data QueueFx :: !

  foreign import data Queue :: * -> *

  type AffQueue e a = Aff (queue :: QueueFx | e) a

  -- | Makes a new asynchronous queue.
  foreign import makeQueue """
    function makeQueue(error) {
      return function(success) {
        return function() {
          success({
            consumers: [],
            producers: [],
            error: undefined 
          })();
        }
      }
    }
  """ :: forall e a. AffQueue e (Queue a)

  -- | Makes a queue and sets it to some value.
  makeQueue' :: forall e a. a -> AffQueue e (Queue a)
  makeQueue' a = do
    v <- makeQueue 
    putQueue v a
    return v

  -- | Takes the next value from the asynchronous queue.
  foreign import takeQueue """
    function takeQueue(queue) {
      return function(error) {
        return function(success) {
          return function() {
            if (queue.error !== undefined) {
              error(queue.error)();
            } else if (queue.producers.length > 0) {
              var producer = queue.producers.shift();

              producer(error, success);
            } else {
              queue.consumers.push({error: error, success: success});
            }
          }
        }
      } 
    }
  """ :: forall e a. Queue a -> AffQueue e a

  -- | Puts a new value into the asynchronous queue. If the queue has
  -- | been killed, this will result in an error.
  foreign import putQueue """
    function putQueue(queue) {
      return function(a) {
        return function(error) {
          return function(success) {
            return function() {
              if (queue.error !== undefined) {
                error(queue.error)();
              } else if (queue.consumers.length === 0) {
                queue.producers.push(function(error, success) {
                  success(a)();
                });

                success({})();
              } else {
                var consumer = queue.consumers.shift();

                try {
                  consumer.success(a)();
                } catch (e) {
                  error(e)();

                  return;                  
                }

                success({})();
              }
            }
          }
        }
      }
    }
  """ :: forall e a. Queue a -> a -> AffQueue e Unit

  -- | Modifies the value at the head of the queue (will suspend until one is available).
  modifyQueue :: forall e a. (a -> a) -> Queue a -> AffQueue e Unit
  modifyQueue f v = takeQueue v >>= (f >>> putQueue v)

  -- | Kills an asynchronous queue.
  foreign import killQueue """
    function killQueue(queue) {
      return function(e) {
        return function(error) {
          return function(success) {
            return function() {
              if (queue.error !== undefined) {
                error(queue.error);
              } else {
                var errors = [];

                queue.error = e;

                while (queue.consumers.length > 0) {
                  var consumer = queue.consumers.shift();

                  try {
                    consumer.error(e)();
                  } catch (e) {
                    errors.push(e);              
                  }
                }

                if (errors.length > 0) error(errors[0])();
                else success({})();
              }
            }
          }
        }
      }
    }
  """ :: forall e a. Queue a -> Error -> AffQueue e Unit

