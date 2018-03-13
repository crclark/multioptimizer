{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Multioptimizer
import Multioptimizer.Executor.Local

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.ByteString.Lazy as BS
import Data.Char (toLower)
import Data.Data (toConstr, Data(..))
import qualified Data.HashMap.Strict as HM
import Data.Scientific (fromFloatDigits)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V

-- All types below correspond to the Keras model JSON format.
-- Higher-level types amenable to a Keras DSL will be introduced later.

type Shape = [Maybe Int]

data Backend = TensorFlow
  deriving (Show, Eq, Typeable, Data, Generic)

instance ToJSON Backend where
  toJSON TensorFlow = String "tensorflow"

newtype Version = Version {getVersion :: Text}
  deriving (Show, Eq, Typeable, Data, Generic, IsString)

instance ToJSON Version where
  toJSON = String . getVersion

data DType = Float32
  deriving (Show, Eq, Typeable, Data, Generic)

instance ToJSON DType where
  toJSON =
    genericToJSON
      Data.Aeson.defaultOptions {tagSingleConstructors = True,
                                 constructorTagModifier = map toLower}
instance FromJSON DType where
  parseJSON =
    genericParseJSON
      Data.Aeson.defaultOptions {tagSingleConstructors = True,
                                 constructorTagModifier = map toLower}

data Activation =
  Softmax | Elu | Selu | Softplus | Softsign | Relu | Tanh | Sigmoid
  | HardSigmoid | Linear
  deriving (Show, Eq, Typeable, Generic, Data)

snakeOptions :: Data.Aeson.Options
snakeOptions =
  Data.Aeson.defaultOptions {constructorTagModifier = snakeCase}

instance ToJSON Activation where
  toJSON = genericToJSON snakeOptions

instance FromJSON Activation where
  parseJSON = genericParseJSON snakeOptions


data NodeRef = NodeRef {
  name :: Text
  -- TODO: figure out the weird inbound_nodes structure.
} deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON NodeRef where
  toJSON (NodeRef n) =
    Array [String n, Number 0, Number 0, Object mempty]

instance FromJSON NodeRef where
  parseJSON x = do
    parsedOuter <- parseJSONList x
    case parsedOuter of
      [inner] -> do
        parsedInner <- parseJSONList inner
        case parsedInner of
          (String y:_) -> return $ NodeRef y
          bad -> fail $ "inner list was " ++ show bad
      _ -> fail "failed to parse outer list of NodeRef"

data VarianceScalingMode = FanIn | FanOut | FanAvg
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON VarianceScalingMode where
  toEncoding = genericToEncoding snakeOptions

instance FromJSON VarianceScalingMode where
  parseJSON = genericParseJSON snakeOptions

data VarianceScalingDistribution = Uniform | Normal
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON VarianceScalingDistribution where
  toEncoding = genericToEncoding snakeOptions

instance FromJSON VarianceScalingDistribution where
  parseJSON = genericParseJSON snakeOptions

data Initializer =
  Zeros {dtype :: DType}
  | Ones
  | Constant {value :: Maybe Double}
  | RandomNormal {mean :: Maybe Double, stddev :: Maybe Double, seed :: Maybe Int}
  | RandomUniform {minval :: Maybe Double, maxval :: Maybe Double, seed :: Maybe Int}
  | TruncatedNormal {mean :: Maybe Double, stddev :: Maybe Double, seed :: Maybe Int}
  | VarianceScaling {
      scale :: Maybe Double,
      mode :: VarianceScalingMode,
      distribution :: VarianceScalingDistribution,
      seed :: Maybe Int,
      dtype :: DType }
  | Orthogonal { gain :: Maybe Double, seed :: Maybe Int}
  | Identity {gain :: Maybe Double}
  | LecunUniform {seed :: Maybe Int}
  | GlorotNormal {seed :: Maybe Int}
  | GlorotUniform {seed :: Maybe Int}
  | HeNormal {seed :: Maybe Int}
  | LecunNormal {seed :: Maybe Int}
  | HeUniform {seed :: Maybe Int}
  deriving (Show, Eq, Generic, Typeable, Data)

classNamedConfig :: Value -> Value
classNamedConfig (Object m) =
  object ["class_name" .= m HM.! "tag",
          "config" .= HM.delete "tag" m]
classNamedConfig _ = error "didn't get a map"

instance ToJSON Initializer where
  toJSON VarianceScaling{..} =
    object ["class_name" .= String "VarianceScaling",
            "config" .= object (scaleRep ++
                                ["mode" .= modeRep,
                                 "distribution" .= distRep])]
    where scaleRep = case scale of
            Nothing -> []
            Just s -> ["scale" .= Number (fromFloatDigits s)]
          modeRep = case mode of
            FanIn -> String "fan_in"
            FanOut -> String "fan_out"
            FanAvg -> String "fan_avg"
          distRep = case distribution of
            Uniform -> String "uniform"
            Normal -> String "normal"

  toJSON x = classNamedConfig . genericToJSON Data.Aeson.defaultOptions $ x

data Regularizer = L1L2 {l1 :: Double, l2 :: Double}
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Regularizer where
  toJSON L1L2{..} =
    object ["class_name" .= String "L1L2",
            "config" .= object ["l1" .= fromFloatDigits l1,
                                "l2" .=  fromFloatDigits l2]]

data Constraint = MaxNorm {maxValue :: Double, axis :: Word}
  | NonNeg | UnitNorm {axis :: Word}
  | MinMaxNorm {minValue :: Double, maxValue :: Double, axis :: Word}
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Constraint where
  toJSON = classNamedConfig . genericToJSON Data.Aeson.defaultOptions

-- | Config of a layer. Constructor name must be name of Keras class.
data LayerConfig = Dense {
  name :: Text,
  trainable :: Bool,
  dtype :: DType,
  units :: Word,
  activation :: Activation,
  useBias :: Bool,
  kernelInitializer :: Initializer,
  biasInitializer :: Maybe Initializer,
  kernelRegularizer :: Maybe Regularizer,
  biasRegularizer :: Maybe Regularizer,
  activityRegularizer :: Maybe Regularizer,
  kernelConstraint :: Maybe Constraint,
  biasConstraint :: Maybe Constraint
} | InputLayer {
  name :: Text,
  dtype :: DType,
  batchInputShape :: Shape,
  sparse :: Bool
} deriving (Show, Eq, Typeable, Generic, Data)

instance ToJSON LayerConfig where
  toJSON =
    genericToJSON
      Data.Aeson.defaultOptions {sumEncoding = UntaggedValue,
                                 fieldLabelModifier = snakeCase}

data Layer = Layer {
  -- TODO: class_name should be added using show . typeOf when
  -- creating JSON output
  config :: LayerConfig,
  inboundNodes :: [[NodeRef]]
} deriving (Show, Eq, Typeable, Generic, Data)

instance ToJSON Layer where
  toJSON Layer{..} =
    object
    ["name" .= (name :: LayerConfig -> Text) config,
     "class_name" .= T.pack (show (toConstr config)),
     "config" .= toJSON config,
     "inbound_nodes" .= toJSON inboundNodes]

layerRef :: Layer -> NodeRef
layerRef Layer{..} = NodeRef ((name :: LayerConfig -> Text) config)

data Model = Model {
  backend :: Backend,
  kerasVersion :: Version,
  layers :: [Layer],
  inputLayers :: [NodeRef],
  outputLayers :: [NodeRef]
} deriving (Show, Eq, Typeable, Generic, Data)

toEdgeNodeRef :: Value -> Value
toEdgeNodeRef (Array xs) = Array (V.map prefix xs)
  where prefix :: Value -> Value
        prefix (Array ys) = Array $ V.take 3 ys
        prefix _ = error $ "unexpected NodeRef encoding: " ++ show xs
toEdgeNodeRef x = error $ "unexpected NodeRef encoding: " ++ show x

instance ToJSON Model where
  toJSON Model{..} = object
    ["class_name" .= String "Model",
     "config" .= object ["name" .= String "model_1",
                         "layers" .= toJSON layers,
                         "input_layers" .= toEdgeNodeRef (toJSON inputLayers),
                         "output_layers" .= toEdgeNodeRef (toJSON outputLayers)],
     "backend" .= toJSON backend,
     "keras_version" .= toJSON kerasVersion]

input_1 :: Layer
input_1 = Layer conf []
  where conf = InputLayer {
    name = "input_1",
    dtype = Float32,
    batchInputShape = [Nothing, Just 784],
    sparse = False
  }

defaultVarianceScaling :: Initializer
defaultVarianceScaling =
  VarianceScaling {
    scale = Just 1.0,
    mode = FanAvg,
    distribution = Uniform,
    seed = Nothing,
    dtype = Float32
  }

dense_1 :: Layer
dense_1 = Layer conf [[layerRef input_1]]
  where conf = Dense {
    name = "dense_1",
    trainable = True,
    dtype = Float32,
    units = 64,
    activation = Relu,
    useBias = True,
    kernelInitializer = defaultVarianceScaling,
    biasInitializer = Just $ Zeros Float32,
    kernelRegularizer = Nothing,
    biasRegularizer = Nothing,
    activityRegularizer = Nothing,
    kernelConstraint = Nothing,
    biasConstraint = Nothing
  }

dense_2 :: Layer
dense_2 = Layer conf [[layerRef dense_1]]
  where conf = Dense {
    name = "dense_2",
    trainable = True,
    dtype = Float32,
    units = 64,
    activation = Relu,
    useBias = True,
    kernelInitializer = defaultVarianceScaling,
    biasInitializer = Just $ Zeros Float32,
    kernelRegularizer = Just (L1L2 0 0.1),
    biasRegularizer = Nothing,
    activityRegularizer = Nothing,
    kernelConstraint = Nothing,
    biasConstraint = Nothing
  }

dense_3 :: Layer
dense_3 = Layer conf [[layerRef dense_2]]
  where conf = Dense {
    name = "dense_3",
    trainable = True,
    dtype = Float32,
    units = 10,
    activation = Softmax,
    useBias = True,
    kernelInitializer = defaultVarianceScaling,
    biasInitializer = Just $ Zeros Float32,
    kernelRegularizer = Just (L1L2 0 0.1),
    biasRegularizer = Nothing,
    activityRegularizer = Nothing,
    kernelConstraint = Nothing,
    biasConstraint = Nothing
  }

testNetwork :: Model
testNetwork = Model {
  backend = TensorFlow,
  kerasVersion = "2.0.8-tf" :: Version,
  layers = [input_1, dense_1, dense_2, dense_3],
  inputLayers = [layerRef input_1],
  outputLayers = [layerRef dense_3]
}

main :: IO ()
main = BS.writeFile "test_output.json" $ encode testNetwork
