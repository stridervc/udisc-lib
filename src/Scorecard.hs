{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- | ... Module UDisc-Scorecard
 -
 - Haskell library to parse UDisc CSV files
 -}

module Scorecard
  ( Scorecard (..)
  , Scorecards
  , PlayerScore (..)
  , HoleStats (..)
  , CourseLayout
  , rowsToCard
  , courseLayout
  , courseLayouts
  , courses
  , layouts
  , allPlayers
  , courseLayoutStats
  , applyAliases
  , timesPlayed
  , cardsForLayout
  ) where

import qualified Data.Text as T
import Data.Csv hiding (lookup)
import Data.Time
import Data.List (nub, sort, sortOn, lookup, intersect)

type Course       = T.Text
type Layout       = T.Text
type Player       = T.Text
type CourseLayout = (Course, Layout)
type Score        = Int
type Par          = Int
type Count        = Int
type Scorecards   = [Scorecard]
type PlayerScore  = (Player, [Score])
type Average      = Float

-- | A complete score card
data Scorecard = Scorecard
  { course        :: Course
  , layout        :: Layout
  , datetime      :: UTCTime
  , pars          :: [Par]          -- ^ List of pars for each hole
  , playerscores  :: [PlayerScore]  -- ^ List of player names with list of their scores
  } deriving (Eq, Show)

data HoleStats = HoleStats
  { holepar :: Par
  , average :: Average
  , eagles  :: Count
  , birdies :: Count
  , numpars :: Count
  , bogeys  :: Count
  , doubles :: Count
  , triples :: Count
  } deriving (Eq, Show)

-- | Get 'CourseLayout' from 'ScoreCard'
courseLayout :: Scorecard -> CourseLayout
courseLayout sc = (course sc, layout sc)

-- List of courses we know about
courses :: Scorecards -> [Course]
courses = nub . map course

-- List of layouts for course we know about
layouts :: Scorecards -> Course -> [Layout]
layouts scs c = nub $ map layout scs'
  where scs'  = cardsForCourse scs c

-- List of course layout combos we know about
courseLayouts :: Scorecards -> [CourseLayout]
courseLayouts = nub . map courseLayout

-- List of players on scorecard
playersOnCard :: Scorecard -> [Player]
playersOnCard sc = map fst $ playerscores sc

-- List of all players we know about
allPlayers :: Scorecards -> [Player]
allPlayers scs = sort $ nub $ concatMap playersOnCard scs

-- Number of times a player has played a course layout
timesPlayed :: Scorecards -> Player -> CourseLayout -> Count
timesPlayed scs p cl = length [x | x <- scs', p `elem` playersOnCard x]
  where scs'  = [x | x <- scs, courseLayout x == cl]

par :: Scorecard -> Par
par = sum . pars

-- score for player on scorecard
playerScore :: Scorecard -> Player -> Score
playerScore sc p = sum scores'
  where Just scores'  = lookup p $ playerscores sc

-- get only scorecards for a course (all layouts)
cardsForCourse :: Scorecards -> Course -> Scorecards
cardsForCourse scs c = filter (\x -> course x == c) scs

-- get only scorecards for this layout
cardsForLayout :: Scorecards -> CourseLayout -> Scorecards
cardsForLayout scs cl = filter (\c -> courseLayout c == cl) scs

-- get the latest scorecard for the layout
latestScoreCard :: Scorecards -> CourseLayout -> Scorecard
latestScoreCard scs cl = head scs'
  where scs'  = reverse $ cardsForLayout scs cl

-- get par for course layout from most recent scorecard
-- of that layout
courseLayoutPar :: Scorecards -> CourseLayout -> Par
courseLayoutPar scs cl = par $ latestScoreCard scs cl

-- get scores for player on course layout
playerScores :: Scorecards -> CourseLayout -> Player -> [Score]
playerScores scs cl p = map (`playerScore` p) scs'
  where scs'  = [c | c <- scs, courseLayout c == cl, p `elem` playersOnCard c]

-- get just a single holes' stats from given scorecards
holeStats :: Scorecards -> Int -> HoleStats
holeStats scs x = HoleStats
  { holepar   = pars (last scs) !! x
  , average   = fromIntegral(sum scores)/fromIntegral (length scores)
  , eagles    = 0
  , birdies   = 0
  , numpars   = 0
  , bogeys    = 0
  , doubles   = 0
  , triples   = 0
  }
  where scores  = map ((!!x).snd) $ concatMap playerscores scs

courseLayoutStats :: Scorecards -> CourseLayout -> (Count, [HoleStats])
courseLayoutStats scs cl = (count, map (holeStats scs') [0..17])
  where scs'  = [sc | sc <- scs, courseLayout sc == cl]
        count = length scs'

data ScorecardRow = ScorecardRow
  { _player   :: Player
  , _course   :: Course
  , _layout   :: Layout
  , datetime  :: String
  , totalS    :: Score
  , score     :: Maybe Score
  , hole1     :: Score
  , hole2     :: Score
  , hole3     :: Score
  , hole4     :: Score
  , hole5     :: Score
  , hole6     :: Score
  , hole7     :: Score
  , hole8     :: Score
  , hole9     :: Score
  , hole10    :: Score
  , hole11    :: Score
  , hole12    :: Score
  , hole13    :: Score
  , hole14    :: Score
  , hole15    :: Score
  , hole16    :: Score
  , hole17    :: Score
  , hole18    :: Score
  } deriving (Eq, Show)

instance FromNamedRecord ScorecardRow where
  parseNamedRecord r = ScorecardRow
    <$> r .: "PlayerName"
    <*> r .: "CourseName"
    <*> r .: "LayoutName"
    <*> r .: "Date"
    <*> r .: "Total"
    <*> r .: "+/-"
    <*> r .: "Hole1"
    <*> r .: "Hole2"
    <*> r .: "Hole3"
    <*> r .: "Hole4"
    <*> r .: "Hole5"
    <*> r .: "Hole6"
    <*> r .: "Hole7"
    <*> r .: "Hole8"
    <*> r .: "Hole9"
    <*> r .: "Hole10"
    <*> r .: "Hole11"
    <*> r .: "Hole12"
    <*> r .: "Hole13"
    <*> r .: "Hole14"
    <*> r .: "Hole15"
    <*> r .: "Hole16"
    <*> r .: "Hole17"
    <*> r .: "Hole18"

rowToPlayer :: ScorecardRow -> PlayerScore
rowToPlayer r = (_player r, scores)
  where scores  = [h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14
              ,h15,h16,h17,h18]
        h1  = hole1  (r :: ScorecardRow)
        h2  = hole2  (r :: ScorecardRow)
        h3  = hole3  (r :: ScorecardRow)
        h4  = hole4  (r :: ScorecardRow)
        h5  = hole5  (r :: ScorecardRow)
        h6  = hole6  (r :: ScorecardRow)
        h7  = hole7  (r :: ScorecardRow)
        h8  = hole8  (r :: ScorecardRow)
        h9  = hole9  (r :: ScorecardRow)
        h10 = hole10 (r :: ScorecardRow)
        h11 = hole11 (r :: ScorecardRow)
        h12 = hole12 (r :: ScorecardRow)
        h13 = hole13 (r :: ScorecardRow)
        h14 = hole14 (r :: ScorecardRow)
        h15 = hole15 (r :: ScorecardRow)
        h16 = hole16 (r :: ScorecardRow)
        h17 = hole17 (r :: ScorecardRow)
        h18 = hole18 (r :: ScorecardRow)

rowsToCard :: [ScorecardRow] -> Scorecard
rowsToCard rows = Scorecard
  { course        = c
  , layout        = l
  , datetime      = dt
  , pars          = ps
  , playerscores  = players
  }
  where info    = head rows
        c       = _course info
        l       = _layout info
        dt      = parseTimeOrError True locale "%F %R" dt'
        dt'     = datetime (info :: ScorecardRow)
        locale  = defaultTimeLocale
        ps      = snd $ rowToPlayer info
        players = map rowToPlayer $ tail rows

playerAlias :: [[T.Text]] -> T.Text -> T.Text
playerAlias as p
  | hasalias  = alias
  | otherwise = p
  where potential = concat $ [a | a <- as, p `elem` a]
        hasalias  = not $ null potential
        alias     = head potential

-- apply aliases to a score card
applyCardAliases :: [[T.Text]] -> Scorecard -> Scorecard
applyCardAliases as sc = sc { playerscores = ps' }
  where ps  = playerscores sc
        ps' = [(playerAlias as p, s) | (p,s) <- ps]

-- apply player aliases to score cards
applyAliases :: [[T.Text]] -> Scorecards -> Scorecards
applyAliases _ [] = []
applyAliases as scs = map (applyCardAliases as) scs
