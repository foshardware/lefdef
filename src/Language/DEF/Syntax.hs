
module Language.DEF.Syntax where

import Data.Text (Text)

type Ident = Text

data DEF = DEF [Option] DieArea [Track] [Component] [Pin] [Net]
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


data Track = Track XY Double Integer Double LayerName
  deriving (Eq, Show)

type XY = Ident


data Component = Component Ident Ident (Maybe Placed)
  deriving (Eq, Show)


data Placed = Placed (Double, Double) Ori
  deriving (Eq, Show)

type Ori = Ident


data Pin = Pin Ident (Maybe Ident) (Maybe Layer) (Maybe Placed)
  deriving (Eq, Show)


data Layer = Layer LayerName (Integer, Integer) (Integer, Integer)
  deriving (Eq, Show)


data Net = Net Ident [Contact]
  deriving (Eq, Show)


type Contact = Either Ident (Ident, Ident)


type LayerName = Ident


data PortDirection = Input | Output | InputOutput
  deriving (Eq, Show)

