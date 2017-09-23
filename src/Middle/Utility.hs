
module Middle.Utility where

type Byte = Char
type Size = Integer
data Ptr  = DirectPtr String | OffsetPtr Ptr Integer

