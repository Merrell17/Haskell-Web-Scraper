{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where

import Text.HTML.Scalpel
import Data.List(isInfixOf)
import Data.List.Split

import Data.Maybe
import Control.Monad
import Control.Applicative

numProfessors :: String -> IO (Maybe Int)
numProfessors x = do
  res <- scrapeURL (getURL x) scrapeLinks
  return $ addMaybe res


addMaybe :: Maybe [Int] -> Maybe Int 
addMaybe xs = fmap sum xs

scrapeLinks :: Scraper String [Int]
scrapeLinks =  
        do
        staffListA <- text $ "ul" @: ["id" @="research-teachinglist"] 
        let zA = length (purgeFakeProfessors staffListA)
        
        staffListB <- text $ "ul" @: ["id" @="tutors-demonstratorslist"]
        let zB = length (purgeFakeProfessors staffListB)

        staffListC <- text $ "ul" @: ["id" @="affiliatelist"]
        let zC = length (purgeFakeProfessors staffListC)
        return [zA, zB, zC]


{-- Takes String from schools to form URL to scrape --}
getURL :: String -> String
getURL department = "https://www.gla.ac.uk/schools/" ++ department ++ "/staff/"

{-- Splits scraper's string return string on commas, keeps profs, removes associate & assistants --}
purgeFakeProfessors :: String -> [String]
purgeFakeProfessors scraperInput = [x | x <- (splitOn "," scraperInput), "Professor" `isInfixOf` x && (not ("Associate" `isInfixOf` x)) && (not("Assistant" `isInfixOf` x))]




