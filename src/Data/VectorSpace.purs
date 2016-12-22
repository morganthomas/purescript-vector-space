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

import Data.Functor (map)
import Control.Applicative (class Applicative)
import Data.Group (class CommutativeGroup)
import Data.Field (class Field)
import Data.Monoid.Additive (Additive(..))
import Data.Ring (mul)
import Data.ApplyAlgebra (ApplyAlgebra, applyAlgebraLift)

-- | A `VectorSpace` v over a field f of "scalars" is a type with an
-- | addition operation <> which makes v a `Group`, and a scalar multiplication
-- | operation `scalarMul` or *< for multiplying a vector by a scalar.
-- |
-- | In addition to the relevant `Group` and `Field` laws, a `VectorSpace` must
-- | satisfy the following laws:
-- |
-- | ```text
-- | x *< (y *< v) = (x * y) *< v
-- | one *< v = v
-- | x *< (u <> v) = (x *< u) <> (x *< v)
-- | (x + y) *< u = (x *< u) <> (y *< v)
-- | ```
class (CommutativeGroup v, Field f) <= VectorSpace v f | v -> f where
  scalarMul :: f -> v -> v

infixr 6 scalarMul as *<

-- | Numbers are a vector space over themselves.
instance numberVectorSpace :: VectorSpace (Additive Number) Number where
  scalarMul x (Additive y) = Additive (x `mul` y)

-- | An Applicative applied to a VectorSpace *may* give you a VectorSpace. You need
-- | to check whether the axioms hold, or at least whether they hold enough for
-- | your use case.
instance applyAlgebraVectorSpace :: (Applicative f, VectorSpace v a) => VectorSpace (ApplyAlgebra f v) a where
  scalarMul a = applyAlgebraLift (map (scalarMul a))
