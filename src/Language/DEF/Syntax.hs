
module Language.DEF.Syntax where

import Data.Text
import Data.Vector

import Language.LEFDEF.Syntax


data DEF = DEF
  [Option] [History] DieArea
  [Row] [Tracks] [Gcellgrid]
  (Vector Via)
  (Vector Component)
  (Vector Pin)
  (Vector Net)
  (Vector Specialnet)
  deriving (Eq, Show)

data Option
  = Version Decimal
  | Cases Bool
  | Design Name
  | BitChars Name
  | DivideChar Name
  | Units DistanceList
  | UseMinSpacing Bool
  | ClearanceMeasure Name
  | ManufacturingGrid Decimal
  deriving (Eq, Show)

newtype DistanceList = DistanceList Integer
  deriving (Eq, Show)

newtype History = History Text
  deriving (Eq, Show)

data DieArea = DieArea (Decimal, Decimal) (Decimal, Decimal)
  deriving (Eq, Show)


data Row = Row Name Name Integer Integer Orientation Integer Integer Integer Integer
  deriving (Eq, Show)

data Tracks = Tracks XY Decimal Integer Decimal [LayerName]
  deriving (Eq, Show)

data Gcellgrid = Gcellgrid XY Decimal Integer Integer
  deriving (Eq, Show)

type XY = Name


data Via = Via Name [Rect]
  deriving (Eq, Show)

data Rect = Rect Name (Decimal, Decimal) (Decimal, Decimal)
  deriving (Eq, Show)


data Component = Component Name Name (Maybe Placed)
  deriving (Eq, Show)


data Placed
  = Unplaced
  | Placed (Decimal, Decimal) Orientation
  | Fixed (Decimal, Decimal) Orientation
  deriving (Eq, Show)

type Orientation = Name


data Pin = Pin Name (Maybe Name) (Maybe Direction) (Maybe Layer) (Maybe Placed)
  deriving (Eq, Show)


data Layer = Layer LayerName (Integer, Integer) (Integer, Integer)
  deriving (Eq, Show)


data Net = Net Name [Contact] (Maybe Routed)
  deriving (Eq, Show)


data Specialnet = Specialnet Name (Maybe Routed)
  deriving (Eq, Show)


type Contact = Either Name (Name, Name)


data Routed = Routed [Segment Integer]
  deriving (Eq, Show)


data Segment a = Seg LayerName (Maybe a) (a, a) [(Maybe a, Maybe a)] (Maybe Name)
  deriving (Eq, Show)


type LayerName = Name


data Direction = Input | Output | InputOutput
  deriving (Eq, Show)

