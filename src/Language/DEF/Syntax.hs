
module Language.DEF.Syntax where

import Data.Text
import Data.Vector

type Ident = Text

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
  = Version Double
  | Cases Bool
  | Design Ident
  | BitChars Ident
  | DivideChar Ident
  | Units DistanceList
  | UseMinSpacing Bool
  | ClearanceMeasure Ident
  | ManufacturingGrid Double
  deriving (Eq, Show)

newtype DistanceList = DistanceList Integer
  deriving (Eq, Show)

newtype History = History Text
  deriving (Eq, Show)

data DieArea = DieArea (Double, Double) (Double, Double)
  deriving (Eq, Show)


data Row = Row Ident Ident Integer Integer Orient Integer Integer Integer Integer
  deriving (Eq, Show)

data Tracks = Tracks XY Double Integer Double [LayerName]
  deriving (Eq, Show)

data Gcellgrid = Gcellgrid XY Double Integer Integer
  deriving (Eq, Show)

type XY = Ident


data Via = Via Ident [Rect]
  deriving (Eq, Show)

data Rect = Rect Ident (Double, Double) (Double, Double)
  deriving (Eq, Show)


data Component = Component Ident Ident (Maybe Placed)
  deriving (Eq, Show)


data Placed
  = Unplaced
  | Placed (Double, Double) Orient
  | Fixed (Double, Double) Orient
  deriving (Eq, Show)

type Orient = Ident


data Pin = Pin Ident (Maybe Ident) (Maybe Direction) (Maybe Layer) (Maybe Placed)
  deriving (Eq, Show)


data Layer = Layer LayerName (Integer, Integer) (Integer, Integer)
  deriving (Eq, Show)


data Net = Net Ident [Contact] (Maybe Routed)
  deriving (Eq, Show)


data Specialnet = Specialnet Ident (Maybe Routed)
  deriving (Eq, Show)


type Contact = Either Ident (Ident, Ident)


data Routed = Routed [Segment Integer]
  deriving (Eq, Show)


data Segment a = Seg LayerName (Maybe a) (a, a) [(Maybe a, Maybe a)] (Maybe Ident)
  deriving (Eq, Show)


type LayerName = Ident


data Direction = Input | Output | InputOutput
  deriving (Eq, Show)

