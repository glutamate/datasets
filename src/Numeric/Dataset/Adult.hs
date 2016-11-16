{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

Adult (AKA Census Income) dataset.

UCI ML Repository link <http://archive.ics.uci.edu/ml/datasets/Adult>

-}

module Numeric.Dataset.Adult where

import Numeric.Dataset.UCI

import Data.Csv
import GHC.Generics
import Control.Applicative
import Data.Text (Text, strip)

data WorkClass = Private | SelfEmpNotInc | SelfEmpInc | FederalGov
               | LocalGov | StateGov | WithoutPay | NeverWorked
  deriving (Show, Read, Eq, Generic)

instance FromField WorkClass where
  parseField = parseDashToCamelField


data MaritalStatus = MarriedCivSpouse | Divorced | NeverMarried
                   | Separated | Widowed | MarriedSpouseAbsent | MarriedAFSpouse
  deriving (Show, Read, Eq, Generic)

instance FromField MaritalStatus where
  parseField "Married-AF-spouse" = pure MarriedAFSpouse
  parseField s = parseDashToCamelField s

data Occupation = TechSupport | CraftRepair | OtherService | Sales | ExecManagerial | ProfSpecialty
                | HandlersCleaners | MachineOpInspct | AdmClerical | FarmingFishing | TransportMoving
                | PrivHouseServ | ProtectiveServ | ArmedForces
  deriving (Show, Read, Eq, Generic)

instance FromField Occupation where
  parseField "ArmedForces" = pure ArmedForces
  parseField s = parseDashToCamelField s

data Relationship = Wife | OwnChild | Husband | NotInFamily | OtherRelative | Unmarried
  deriving (Show, Read, Eq, Generic)

instance FromField Relationship where
  parseField s = parseDashToCamelField s

data Adult = Adult
  { age :: Int
  , workClass :: Maybe WorkClass
  , finalWeight :: Int
  , education :: Text
  , educationNum :: Int
  , maritalStatus :: MaritalStatus
  , occupation :: Maybe Occupation
  , relationship :: Relationship
  } deriving (Show, Read, Generic)

instance FromRecord Adult where
  parseRecord v = Adult <$> v .! 0 <*> (v.! 1 <|> return Nothing) <*> v.!2 <*> (strip <$> v.!3)
                        <*> v.!4 <*> v.!5<*> (v.!6 <|> return Nothing) <*> v.!7

adult :: Dataset Adult
adult = csvDataset "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
