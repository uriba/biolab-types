module Biolab.Types (
    Well(..),
    Measurement(..),
    SampleId(..),
    LabeledData,
    MesType(..),
    ExpData,
    )
where
import Data.DateTime (DateTime)
import qualified Data.Map as M
import Data.Map (Map)

data SampleId = SampleId { sidExpId :: String, sidPlate :: Int, sidWell :: Well} deriving (Eq, Ord, Show, Read)

data Well = Well {
        wRow :: Char,
        wColumn :: Int
    } deriving (Eq, Show, Read, Ord)

wellStr :: Well -> String
wellStr w = concat ["(", [wRow w],",",show . wColumn $ w,")"]

data MesType = Absorbance Int | Fluorescence Int Int | Luminesense Int

data Measurement = Measurement {
        mExpDesc :: String,
        mPlate :: Int,
        mTime :: DateTime,
        mType :: String,
        mWell :: Well,
        mVal :: Double
    } deriving (Eq, Show)

type LabeledData a = Map String (Map SampleId a)
    
ldMap :: (a -> b) -> LabeledData a -> LabeledData b
ldMap f = M.map (M.map f)

ldZip :: (a -> b -> c) -> LabeledData a -> LabeledData b -> LabeledData c
ldZip f = M.intersectionWith (M.intersectionWith f)

type ExpData = LabeledData [Measurement]
