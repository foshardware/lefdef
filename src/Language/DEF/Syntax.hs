
module Language.DEF.Syntax where

import Data.Text (Text)

type Ident = Text

data DEF = DEF [Option] DieArea [Row] [Track] [Component] [Pin] [Net] [Specialnet]
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


data DieArea = DieArea (Double, Double) (Double, Double)
  deriving (Eq, Show)


data Row = Row Ident Ident Integer Integer Orient Integer Integer Integer Integer
  deriving (Eq, Show)

data Track = Track XY Double Integer Double LayerName
  deriving (Eq, Show)

type XY = Ident


data Component = Component Ident Ident (Maybe Placed)
  deriving (Eq, Show)


data Placed
  = Unplaced
  | Placed (Double, Double) Orient
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

