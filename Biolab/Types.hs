module Biolab.Types (
    Well(..),
    SampleId(..),
    LabeledData,
    MesType(..),
    RawMeasurement(..),
    NormalizedMeasurement(..),
    ExpData,
    wellStr,
    ColonySample(..),
    AbsorbanceSample(..),
    FluorescenseSample(..),
    LuminescenseSample(..),
    RawColonyMeasurements,
    RawAbsorbance(..),
    RawFluorescence(..),
    RawLuminescense(..),
    NormalizedAbsorbance(..),
    NormalizedFluorescence(..),
    NormalizedLuminescense(..),
    )
where
import Data.DateTime (DateTime)
import qualified Data.Map as M
import Data.Vector (Vector)
import Data.Map (Map)

newtype RawMeasurement = RawMeasurement {mVal :: Double} deriving (Eq, Ord, Show)
newtype NormalizedMeasurement = NormalizedMeasurement {nmVal :: Double}

data SampleId = SampleId { sidExpId :: String, sidPlate :: Int, sidWell :: Well} deriving (Eq, Ord, Show, Read)
data MesType = Absorbance Int | Fluorescence Int Int | Luminesense Int deriving (Eq, Ord, Show, Read)

type ColonyMeasurements a = Vector (DateTime, a)
type RawColonyMeasurements = ColonyMeasurements RawMeasurement
type NormalizedColonyMeasurements = ColonyMeasurements NormalizedMeasurement

class ColonySample a where
    measurements :: a b -> ColonyMeasurements b

data AbsorbanceSample a = AbsorbanceSample { asWaveLength :: Int, asMes :: ColonyMeasurements a }
type RawAbsorbance = AbsorbanceSample RawMeasurement
type NormalizedAbsorbance = AbsorbanceSample NormalizedMeasurement
instance ColonySample AbsorbanceSample where
    measurements = asMes

data FluorescenseSample a = FluorescenseSample { flExcitation :: Int, flEmission :: Int, flMes :: ColonyMeasurements a }
type RawFluorescence = FluorescenseSample RawMeasurement
type NormalizedFluorescence = FluorescenseSample NormalizedMeasurement
instance ColonySample FluorescenseSample where
    measurements = flMes

data LuminescenseSample a = LuminescenseSample { lsWaveLength :: Int, lsMes :: ColonyMeasurements a }
type RawLuminescense = LuminescenseSample RawMeasurement
type NormalizedLuminescense = LuminescenseSample NormalizedMeasurement
instance ColonySample LuminescenseSample where
    measurements = lsMes

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

type ExpData = LabeledData ([RawAbsorbance],[RawFluorescence],[RawLuminescense])
