{-|
Description: Main module, wires up the whole app

This module calls basic initializations functions - loads the Config, builds
the 'Env', and starts up the Warp HTTP server.

One interesting thing here is the use of Async to start the server in a
separate thread. We could spawn other threads here too, if we had other things
which needed to run concurrently to our HTTP API, like a message queue
consumer. If we needed any mechanism to synchronize these sub-systems, we could
put that in the 'Env', which is shared.

This follows the pattern suggested
<https://www.fpcomplete.com/blog/2017/06/readert-design-pattern here>, although
we don't use 'ReaderT' at the top level, instead just passing the 'Env' as an
argument.

A possible pitfall here is that we might have a deeper hierarchy of
dependencies. For example, suppose we have some type that allows us to produce
messages to a message queue (call it @Producer@). This type may need some
configuration, which we would put in 'Env'. However, we want this @Producer@
value to be available to out HTTP API (so we can produce messages when we
receive some request). Therefore, we would want the @Producer@ to be in 'Env',
which would be a circular dependency.

We could solve this by creating a hierarchy of Env types, matching the
hierarchies of dependencies in our code. This is the approach mentioned in
<https://www.fpcomplete.com/blog/2017/07/the-rio-monad this blog post>.

We could also create an Env with some mutable fields (using 'Data.IORef.IORef',
for example). In this approach, Env would initially hold an empty
@'Data.IORef.IORef' ('Maybe' Producer)@. We could use this to build our
@Producer@, fill the 'Data.IORef.IORef', then build our HTTP API. In this case,
the HTTP API has no type-level guarantee that a @Producer@ will actually be
available.

Instead of 'Data.IORef.IORef', we could use
@<https://hackage.haskell.org/package/ivar-simple-0.3.2 IVar>@s,
single-assignment variables which block on read until a value is provided.
This protects us against the @Producer@ value changing or disappearing,
but still allows for cyclic dependencies leading to deadlocks.

This is the kind of problem that libraries like
<https://github.com/stuartsierra/component component> (for Clojure) solve.
It allows us to easily define a graph of dependencies and initialize them in
the correct order, detecting circular dependencies.

I'm not aware if something like that exists for Haskell. If you do, please let
me know! :)
-}
module HaskellTodo (
  startApp
  ) where

import Control.Concurrent.Async ( withAsync, wait )
import HaskellTodo.Config
import HaskellTodo.Env
import qualified HaskellTodo.WarpServer as WarpServer

startApp :: IO ()
startApp = do
  config <- loadConfig
  env <- buildEnv config
  withAsync (WarpServer.run env) $ \warp ->
    wait warp
