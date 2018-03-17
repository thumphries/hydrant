{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hydrant.Markup.Monadic (
    MarkupM
  , runMarkupM
  , liftMarkup
  ) where


import           Control.Monad.Trans.Writer (WriterT (..), Writer, runWriter, tell)
import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..))

import           Data.Function (($), (.))
import           Data.Functor (Functor (..))
import           Data.Tuple (snd)

import qualified Hydrant.Markup as Hydrant


-- FIXME just provide an identical API - interface should be the same
--       clear warnings via documentation to ensure no confusion
newtype MarkupM a = MarkupM {
    unMarkup :: Writer Hydrant.Markup a
  } deriving (Functor, Applicative, Monad)

runMarkupM :: MarkupM a -> Hydrant.Markup
runMarkupM =
  snd . runWriter . unMarkup

liftMarkup :: Hydrant.Markup -> MarkupM ()
liftMarkup =
  Markup . tell

element :: Hydrant.Tag -> [Hydrant.Attribute] -> MarkupM () -> MarkupM ()
element t as m =
  liftMarkup $
    Hydrant.element t as m

voidElement :: Hydrant.Tag -> [Hydrant.Attribute] -> MarkupM ()
voidElement t as =
  liftMarkup $
    Hydrant.voidElement t as

content :: Text -> MarkupM ()
content =
  liftMarkup . Hydrant.content

unsafeContent :: Text -> MarkupM ()
unsafeContent =
  liftMarkup . Hydrant.unsafeContent
