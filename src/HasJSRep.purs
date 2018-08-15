module HasJSRep where

import Prelude

import Control.Promise (Promise)
import Data.Function.Uncurried as FU
import Data.Nullable (Nullable)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Uncurried as EU
import Foreign.Object as FO
import Prim.RowList as RL
import Prim.TypeError as TE

class MembersHaveJSRep (rl :: RL.RowList)
instance nilMHJSR :: MembersHaveJSRep RL.Nil
instance consMHJSR :: (MembersHaveJSRep tail, HasJSRep ty) => MembersHaveJSRep (RL.Cons name ty tail)

hasJSRep :: forall a. HasJSRep a => a -> a
hasJSRep x = x

-- | A type class for types that have a JavaScript representation, with instances constrained to this module.
class HasJSRep a

-- simple types
instance numberHJSR :: HasJSRep Number
else instance stringHJSR :: HasJSRep String
else instance intHJSR :: HasJSRep Int
else instance booleanHJSR :: HasJSRep Boolean
else instance unitHJSR :: HasJSRep Unit

-- more complex
else instance arrayHJSR :: HasJSRep a => HasJSRep (Array a)
else instance foObjectHJSR :: HasJSRep a => HasJSRep (FO.Object a)
else instance promiseHJSR :: HasJSRep a => HasJSRep (Promise a)
else instance recordHJSR :: (RL.RowToList r rl, MembersHaveJSRep rl) => HasJSRep (Record r)
else instance variantHJSR :: (RL.RowToList r rl, MembersHaveJSRep rl) => HasJSRep (Variant r)

else instance fnHJSR :: (HasJSRep a, HasJSRep b) => HasJSRep (a -> b)
else instance effectHJSR :: HasJSRep a => HasJSRep (Effect a)

-- no nested nullables
else instance nestedNullableHJSR ::
  ( TE.Fail
      (TE.Above
        (TE.Text "Nested nullable types do not have a runtime representation. You should fix your type:")
        (TE.Quote (Nullable (Nullable a)))
      )
  ) => HasJSRep (Nullable (Nullable a))
else instance nullableHJSR :: (HasJSRep a) => HasJSRep (Nullable a)

-- generated function instances
instance iFUFn2HJSR ::
  ( HasJSRep a
  , HasJSRep b
  ) => HasJSRep (FU.Fn2 a b c)

instance iFUFn3HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  ) => HasJSRep (FU.Fn3 a b c d)

instance iFUFn4HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  ) => HasJSRep (FU.Fn4 a b c d e)

instance iFUFn5HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  ) => HasJSRep (FU.Fn5 a b c d e f)

instance iFUFn6HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  , HasJSRep f
  ) => HasJSRep (FU.Fn6 a b c d e f g)

instance iFUFn7HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  , HasJSRep f
  , HasJSRep g
  ) => HasJSRep (FU.Fn7 a b c d e f g h)

instance iFUFn8HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  , HasJSRep f
  , HasJSRep g
  , HasJSRep h
  ) => HasJSRep (FU.Fn8 a b c d e f g h i)

instance iFUFn9HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  , HasJSRep f
  , HasJSRep g
  , HasJSRep h
  , HasJSRep i
  ) => HasJSRep (FU.Fn9 a b c d e f g h i j)

instance iEUEffectFn2HJSR ::
  ( HasJSRep a
  , HasJSRep b
  ) => HasJSRep (EU.EffectFn2 a b c)

instance iEUEffectFn3HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  ) => HasJSRep (EU.EffectFn3 a b c d)

instance iEUEffectFn4HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  ) => HasJSRep (EU.EffectFn4 a b c d e)

instance iEUEffectFn5HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  ) => HasJSRep (EU.EffectFn5 a b c d e f)

instance iEUEffectFn6HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  , HasJSRep f
  ) => HasJSRep (EU.EffectFn6 a b c d e f g)

instance iEUEffectFn7HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  , HasJSRep f
  , HasJSRep g
  ) => HasJSRep (EU.EffectFn7 a b c d e f g h)

instance iEUEffectFn8HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  , HasJSRep f
  , HasJSRep g
  , HasJSRep h
  ) => HasJSRep (EU.EffectFn8 a b c d e f g h i)

instance iEUEffectFn9HJSR ::
  ( HasJSRep a
  , HasJSRep b
  , HasJSRep c
  , HasJSRep d
  , HasJSRep e
  , HasJSRep f
  , HasJSRep g
  , HasJSRep h
  , HasJSRep i
  ) => HasJSRep (EU.EffectFn9 a b c d e f g h i j)
