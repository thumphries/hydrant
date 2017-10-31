{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hydrant.Markup (
  -- * Markup
    Markup
  , toText
  , Tag (..)
  , Attribute (..)
  , AttributeKey (..)
  , AttributeValue (..)
  -- ** Combinators
  , element
  , voidElement
  , content
  , unsafeContent
  -- * Utility
  , Internal.escapeEntities
  ) where


import           Data.Functor (Functor (..))
import           Data.Function ((.))
import           Data.Text (Text)

import qualified Hydrant.Markup.Internal as Internal

import           Text.Builder (Builder)
import qualified Text.Builder as B


newtype Markup = Markup {
    unMarkup :: Builder
  }

toText :: Markup -> Text
toText =
  B.run . unMarkup

newtype Tag = Tag {
    unTag :: Text
  }

data Attribute =
  Attribute !AttributeKey !AttributeValue

unAttribute :: Attribute -> (Text, Text)
unAttribute attr =
  case attr of
    (Attribute (AttributeKey k) (AttributeValue v)) ->
      (k, v)
{-# INLINE unAttribute #-}

newtype AttributeKey = AttributeKey {
    unAttributeKey :: Text
  }

newtype AttributeValue = AttributeValue {
    unAttributeValue :: Text
  }

element :: Tag -> [Attribute] -> Markup -> Markup
element t attrs =
  Markup . Internal.element (unTag t) (fmap unAttribute attrs) . unMarkup

voidElement :: Tag -> [Attribute] -> Markup
voidElement t =
  Markup . Internal.voidElement (unTag t) . fmap unAttribute

content :: Text -> Markup
content =
  Markup . Internal.content

unsafeContent :: Text -> Markup
unsafeContent =
  Markup . Internal.unsafeContent
