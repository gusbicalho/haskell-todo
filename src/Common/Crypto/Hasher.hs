{-|
Description: Defines the Hasher class

This module defines an abstraction for taking and validating hashes of values.
I guess this isn't really general enough to support many encryption libraries
besides BCrypt, but it works as an exercise in abstraction.

This is a Multi-Param Type Class. Basically, an instance of this class for some
type @hasher@ tells us that given __a value of type @hasher@__, we can __create
actions in monad @hashingM@__, either __taking a value of type @input@ and
outputting a value of type @hash@__, or __validating a @hash@ value against
some @input@ value__.

The functional dependencies here tell us that, given a @hasher@ type and an
@input@ type, we unambiguously know the output type @hash@. This means you
cannot define a @Hasher@ that takes @ByteString@s and outputs some polymorphic
thing. In other words, a specific @hasher@ may be able to turn @ByteString@s
into @ByteString@ hasher /and/ to turn @Text@s into @Text@ hashes, but this
means that this same @hasher@ cannot turn a @Text@s into a @ByteString@ hash.

This isn't a necessary restriction, but it is useful, because it lets us avoid
writing some type signatures. In normal usage (as far as I explored), the
@hasher@, @hashingM@ and @input@ types are usually unambiguous - they are
either concrete types or defined polymorphically in a function's type
signature.

The @hash@ type, however, is commonly ambiguous. For example, if the only use
of a hash in the body of a function is to call @show@ on it, GHC cannot infer
a specific type, only a @Showable@ constraint. This would require us to add
@:: String@ or @:: Text@ somewhere, which would in fact end up reducing our
polymorphism. With the functional dependency, this ambiguity goes away.

This class is similar, but a little different to the normal monad transformer
approach. Instead of adding extra information to the monadic context itself
(like @MonadReader@ does), we expect the extra information to be held in the
@hasher@ type. If it works on IO, that's fine; if it requires some custom
monad, that's fine too. Code that uses a @hasher@ will have to run on a monad
supported by it.

However, I feel like this makes implementation and use of hashers a little
simpler than the monad transformer approach. For example, a typical
implementation will usually run on @IO@, or require @MonadIO@ (for randomness).
We could create a monad transformer that encapsulates a @MonadIO@ and add that
to the application mtl stack (which runs into the famous n^2 problem).

Instead, we just make a @hasher@ that can run in any @MonadIO@ context.
Code that uses it is just as generic - it can just require that a Hasher exist
for some monadic context and and input type.
-}
module Common.Crypto.Hasher where

class Monad hashingM => Hasher hasher hashingM input hash | hasher input -> hash where
  maybeHash :: hasher -> input -> hashingM (Maybe hash)
  validate :: hasher -> hash -> input -> hashingM Bool
