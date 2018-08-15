module Test.Main where

import Prelude

import Data.Array (range)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.String as String
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (log)
import Record.Format as RF
import Type.Prelude (SProxy(..))

main :: Effect Unit
main = do
  traverse_ (go "FU.Fn") (range 2 9)
  traverse_ (go "EU.EffectFn") (range 2 9)

  where
    templateFn = SProxy :: SProxy """
instance i{name}{count}HJSR ::
  ( {constraints}
  ) => HasJSRep ({head})"""

    go name i = do
      log $ RF.format templateFn
        { count: i
        , constraints: intercalate "\n  , " (Array.take i constraints)
        , name: String.replace (String.Pattern ".") (String.Replacement "") name
        , head: name <> show i <> " " <> intercalate " " (Array.take (i + 1) vars)
        }

    constraints =
      [ "HasJSRep a"
      , "HasJSRep b"
      , "HasJSRep c"
      , "HasJSRep d"
      , "HasJSRep e"
      , "HasJSRep f"
      , "HasJSRep g"
      , "HasJSRep h"
      , "HasJSRep i"
      , "HasJSRep j"
      ]

    vars =
      [ "a"
      , "b"
      , "c"
      , "d"
      , "e"
      , "f"
      , "g"
      , "h"
      , "i"
      , "j"
      ]
