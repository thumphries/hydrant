{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hydrant.Markup.Internal (
    element
  , voidElement
  , content
  , unsafeContent
  , escapeEntities
  ) where


import           Data.Functor (Functor (..))
import           Data.Function ((.), ($))
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tuple (uncurry)

import           Text.Builder (Builder)
import qualified Text.Builder as Builder


element :: Text -> [(Text, Text)] -> Builder -> Builder
element tag attrs body =
  tagOpen tag attrs <> body <> tagClose tag

voidElement :: Text -> [(Text, Text)] -> Builder
voidElement tag attrs =
  Builder.text $
       "<"
    <> T.intercalate " " (escapeEntities tag : fmap (uncurry attribute) attrs)
    <> "/>"

tagOpen :: Text -> [(Text, Text)] -> Builder
tagOpen tag attrs =
  Builder.text $
       "<"
    <> T.intercalate " " (escapeEntities tag : fmap (uncurry attribute) attrs)
    <> ">"

tagClose :: Text -> Builder
tagClose t =
  Builder.text $
    "</" <> t <> ">"

attribute :: Text -> Text -> Text
attribute key val =
  key <> "=\"" <> escapeEntities val <> "\""

content :: Text -> Builder
content =
  Builder.text . escapeEntities

unsafeContent :: Text -> Builder
unsafeContent =
  Builder.text

-- | Performs minimal predefined entity escaping as follows:
--
-- > case c of
-- >   '<'  -> "&lt;"
-- >   '>'  -> "&gt;"
-- >   '&'  -> "&amp;"
-- >   '"'  -> "&quot;"
-- >   '\'' -> "&#39;"
-- >   x    -> fromString [x]
escapeEntities :: Text -> Text
escapeEntities =
    T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
  . T.replace "\"" "&quot;"
  . T.replace "'" "&#39;"
  . T.replace "&" "&amp;"
{-# INLINEABLE escapeEntities #-}
