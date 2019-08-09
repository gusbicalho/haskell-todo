{-|
Description: Defines the Transactor class

This module defines an abstraction for running actions in a transactional
context. This would typically be a database, but we could use this same
abstraction when dealing with STM.

This is a Multi-Param Type Class. Basically, an instance of this class for some
type @transactor@ tells us that given __a value of type @transactor@__, we can
__create actions in monad @transactionM@__, that will transact the
__contents of some value of type @action@__.

The functional dependencies here tell us that each @transactor@ can only transact
@action@s of a single type. This means you cannot define a 'Transactor' that
accepts both 'ByteString' and 'Text' as actions, for example.

This isn't a necessary restriction, but it is useful, because it lets us avoid
writing some type signatures. In normal usage (as far as I explored), the
@transactor@ and @transactionM@ types are usually unambiguous - they are either
concrete types or defined polymorphically in a function's type signature.

The @action@ type, however, is commonly ambiguous, because in general the
definition of the actions themselves will come from a typeclass. For example,
one can have a typeclass @DbUser action@, with a function
@getUser :: Integer -> action User@ that returns an @action@ to get a @User@
from a database. We will need to have different instances of this DbUser class
for each underlying database interface, with a different @action@ type. In this
case, type @action@ in the code below would be ambiguous:

@
get42 :: Transactor t m action => t -> m User
get42 transactor = transact transactor (getUser 42)
@

If we did not have a functional dependency, the call above would be ambiguous
because GHC could not infer the @action@ type parameter to find an
implementation for both @transact@ and @getUser@.

By adding a functional dependency, we ensure that the @action@ type is known,
as long as we know the type @transactor@.

This class is similar, but a little different to the normal monad transformer
approach. Instead of adding extra information to the monadic context itself
(like 'MonadReader' does), we expect the extra information to be held in the
@transactor@ type. If it works on IO, that's fine; if it requires some custom
monad, that's fine too. Code that uses a @transactor@ will have to run on a
monad supported by it.

However, I feel like this makes implementation and use of transactors a little
simpler than the monad transformer approach. For example, a typical
implementation will usually run on 'IO', or require 'MonadIO'. We could create
a monad transformer that encapsulates a 'MonadIO' and add that to the
application mtl stack (which runs into the famous n^2 problem).

Instead, we just make a @transactor@ that can run in any 'MonadIO' context.
Code that uses it is just as generic - it can just require that a Transactor
exist for some monadic context.

Note that this typeclass does not put any restrictions on the @action@ type.
It may or may not be a MonadIO, or even a Monad. Typical SQL databases allow
one to take the results of one query, run arbitrary code, and make another
query, all in a single transaction. Therefore, @action@s of 'Transactor's for
these databases may have instances for 'Monad' and 'MonadIO'.

On the other hand, databases like Datomic require transactions to be sent all
at once, as a single request. This means the @action@s for a 'Transactor' for
Datomic cannot be 'Monad's. However, they could support other typeclasses, like
'Monoid', for example.
-}
module Common.Db.Transactor where

class Monad transactionM => Transactor transactor transactionM action | transactor -> action where
  transact :: transactor -> action a -> transactionM a
