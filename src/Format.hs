-- Hoff -- A gatekeeper for your commits
-- Copyright 2020 The Hoff authors
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Format (
  format,
  Text.Only (..),
)
where

import Data.Text (Text)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)

import Data.Text.Format qualified as Text

-- Like `Text.format`, but returning a strict `Text` instead of a lazy one.
format :: Params ps => Text.Format -> ps -> Text
format formatString params = toStrict $ Text.format formatString params
