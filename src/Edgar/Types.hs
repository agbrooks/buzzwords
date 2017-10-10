{-# LANGUAGE TemplateHaskell #-}

module Edgar.Types where

import Data.Time
import Control.Lens

-- |Company index. Will need zeros to be accepted by EDGAR.
type CIK = Integer

-- |A company, which has a name and a CIK.
data Company = Company {
  _name :: String,
  _cik  :: CIK
} deriving (Show, Eq, Ord)

-- |A String indicating the type of form filed.
type FilingCategory = String

-- |Information about the filing of a form, but not the contents themselves.
data FilingMetadata = FilingMetadata {
  _company  :: Company,
  _category :: FilingCategory,
  _date     :: Day
} deriving (Show, Eq, Ord)

-- |The complete form, with publishing information and text. 
data Filing = Filing {
  _handle     :: FilingMetadata,
  _filingText :: String
} deriving (Show, Eq, Ord)

makeLenses ''Company
makeLenses ''FilingMetadata
makeLenses ''Filing
