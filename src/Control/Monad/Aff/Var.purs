-- | A low-level primitive for building asynchronous code.
module Control.Monad.Aff.Var
  ( AffVar()
  , Var()
  , VarFx()
  , killVar
  , makeVar
  , makeVar'
  , modifyVar
  , putVar
  , takeVar
  ) where

  import Control.Monad.Aff
  import Control.Monad.Eff.Exception(Error())

  foreign import data VarFx :: !

  foreign import data Var :: * -> *

  type AffVar e a = Aff (var :: VarFx | e) a

  -- | Makes a new asynchronous variable.
  foreign import makeVar """
    function makeVar(error) {
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
  """ :: forall e a. AffVar e (Var a)

  -- | Makes a variable and sets it to some value.
  makeVar' :: forall e a. a -> AffVar e (Var a)
  makeVar' a = do
    v <- makeVar 
    putVar v a
    return $ v

  -- | Takes the next value from the asynchronous variable.
  foreign import takeVar """
    function takeVar(avar) {
      return function(error) {
        return function(success) {
          return function() {
            if (avar.error !== undefined) {
              error(avar.error)();
            } else if (avar.producers.length > 0) {
              var producer = avar.producers.shift();

              producer(error, success);
            } else {
              avar.consumers.push({error: error, success: success});
            }
          }
        }
      } 
    }
  """ :: forall e a. Var a -> AffVar e a

  -- | Puts a new value into the asynchronous variable. If the variable has
  -- | been killed, this will result in an error.
  foreign import putVar """
    function putVar(avar) {
      return function(a) {
        return function(error) {
          return function(success) {
            return function() {
              if (avar.error !== undefined) {
                error(avar.error)();
              } else if (avar.consumers.length == 0) {
                avar.producers.push(function(error, success) {
                  success(a)();
                });
              } else {
                var consumer = avar.consumers.shift();

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
  """ :: forall e a. Var a -> a -> AffVar e Unit

  -- | Modifies an asynchronous variable.
  modifyVar :: forall e a. (a -> a) -> Var a -> AffVar e Unit
  modifyVar f v = takeVar v >>= (f >>> putVar v)

  -- | Kills an asynchronous variable.
  foreign import killVar """
    function killVar(avar) {
      return function(e) {
        return function(error) {
          return function(success) {
            return function() {
              if (avar.error !== undefined) {
                error(avar.error);
              } else {
                var errors = [];

                avar.error = e;

                while (avar.consumers.length > 0) {
                  var consumer = avar.consumers.shift();

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
  """ :: forall e a. Var a -> Error -> AffVar e Unit

