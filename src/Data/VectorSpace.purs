-- Copyright 2016 Morgan Thomas
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--    http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Data.VectorSpace where

import Data.Group (class Group)
import Data.Field (class Field)

-- | A `VectorSpace` v over a field f of "scalars" is a type with an
-- | addition operation <> which makes v a `Group`, and a scalar multiplication
-- | operation `scalarMult` for multiplying a vector by a scalar.
-- |
-- | In addition to the relevant `Group` and `Field` laws, a `VectorSpace` must
-- | satisfy the following laws:
-- |
-- | ```text
-- | x `scalarMult` (y `scalarMult` v) = (x * y) `scalarMult` v
-- | one `scalarMult` v = v
-- | x `scalarMult` (u <> v) = (x `scalarMult` u) <> (x `scalarMult` v)
-- | (x + y) `scalarMult` u = (x `scalarMult` u) <> (y `scalarMult` v)
-- | ```
class (Group v, Field f) <= VectorSpace v f | v -> f where
  scalarMult :: f -> v -> v

infixr 6 scalarMult as *<
