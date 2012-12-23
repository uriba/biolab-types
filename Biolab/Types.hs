module Biolab.Types (
    Well(..),
    SampleId(..),
    LabeledData,
    MesType(..),
    RawMeasurement(..),
    ExpData,
    wellStr,
    ColonySample,
    )
where
import Data.DateTime (DateTime)
import qualified Data.Map as M
import Data.Vector (Vector)
import Data.Map (Map)

newtype RawMeasurement = RawMeasurement {mVal :: Double} deriving (Eq, Ord)
data SampleId = SampleId { sidExpId :: String, sidPlate :: Int, sidWell :: Well} deriving (Eq, Ord, Show, Read)
data MesType = Absorbance Int | Fluorescence Int Int | Luminesense Int deriving (Eq, Ord, Show, Read)
type ColonySample = Vector(DateTime, RawMeasurement)

data Well = Well {
        wRow :: Char,
        wColumn :: Int
    } deriving (Eq, Show, Read, Ord)

wellStr :: Well -> String
wellStr w = concat ["(", [wRow w],",",show . wColumn $ w,")"]

type LabeledData a = Map String (Map SampleId a)
    
ldMap :: (a -> b) -> LabeledData a -> LabeledData b
ldMap f = M.map (M.map f)

ldZip :: (a -> b -> c) -> LabeledData a -> LabeledData b -> LabeledData c
ldZip f = M.intersectionWith (M.intersectionWith f)

type ExpData = LabeledData [(MesType,ColonySample)]
