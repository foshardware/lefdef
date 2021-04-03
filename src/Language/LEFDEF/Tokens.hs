
module Language.LEFDEF.Tokens
    ( Lexer (..)
    , Pos
    , Token (..)
    ) where

import Data.Text (Text)

data Lexer a = L Pos a
  deriving (Show, Eq)

type Pos = (Int, Int)

data Token
    -- Keywords
    = Tok_End
    | Tok_Specialnets
    | Tok_Routed
    | Tok_Star
    | Tok_New
    | Tok_Net
    | Tok_Nets
    | Tok_Row
    | Tok_Pins
    | Tok_Plus
    | Tok_Fixed
    | Tok_Placed
    | Tok_Source
    | Tok_Signal
    | Tok_Dist
    | Tok_Components
    | Tok_Minus
    | Tok_Tracks
    | Tok_Do
    | Tok_Step
    | Tok_Diearea
    | Tok_Library
    | Tok_Lparen
    | Tok_Rparen
    | Tok_Version
    | Tok_Namescasesensitive
    | Tok_BusBitChars
    | Tok_DividerChar
    | Tok_Units
    | Tok_Design
    | Tok_Distance
    | Tok_Microns
    | Tok_Obs
    | Tok_Pin
    | Tok_ClearanceMeasure
    | Tok_ManufacturingGrid
    | Tok_Layer
    | Tok_Type
    | Tok_Spacing
    | Tok_Direction
    | Tok_Pitch
    | Tok_Offset
    | Tok_Thickness
    | Tok_Height
    | Tok_Width
    | Tok_Resistance
    | Tok_Spacingtable
    | Tok_Parallelrunlength
    | Tok_EdgeCapacitance
    | Tok_Capacitance
    | Tok_Via
    | Tok_Vias
    | Tok_Rect
    | Tok_ViaRule
    | Tok_To
    | Tok_By
    | Tok_Overhang
    | Tok_Enclosure
    | Tok_MetalOverhang
    | Tok_Samenet
    | Tok_Site
    | Tok_Symmetry
    | Tok_Stack
    | Tok_Class
    | Tok_Size
    | Tok_Macro
    | Tok_Foreign
    | Tok_Origin
    | Tok_Use
    | Tok_UseMinSpacing
    | Tok_Antennapartialmetalarea
    | Tok_Antennapartialmetalsidearea
    | Tok_Antennagatearea
    | Tok_Antennadiffarea
    | Tok_Shape
    | Tok_Port
    | Tok_Path
    | Tok_On
    | Tok_Off
    | Tok_Input
    | Tok_Output
    | Tok_Inout
    | Tok_Horizontal
    | Tok_Vertical
    | Tok_Power
    | Tok_Ground
    | Tok_Polygon
    | Tok_Range
    | Tok_Analog
    | Tok_Clock
    | Tok_Database
    | Tok_Gcellgrid

    | Tok_History Text

    -- Identifiers
    | Tok_Identifier Text
    | Token Text

    -- Literals
    | Tok_Number Text
    | Tok_String Text

  deriving (Eq, Show)
