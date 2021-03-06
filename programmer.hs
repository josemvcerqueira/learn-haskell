module Programmer where

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgramingLanguage =
    Haskell
    | Agda
    | Idris
    | Purescript
    deriving (Eq, Show)

data Programmer =
    Programmer {
        os :: OperatingSystem,
        lang :: ProgramingLanguage }
        deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgramingLanguage]
allLanguages = [Haskell, Agda, Idris, Purescript]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer { lang = x, os = y }
  | x <- allLanguages
  , y <- allOperatingSystems
  ]

newtype Name    = Name String deriving Show
newtype Acres   = Acres Int deriving Show

