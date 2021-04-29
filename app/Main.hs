{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.Scalpel
import ProfScrapeLib
import Text.LaTeX
import Control.Monad
import Control.Applicative
import System.IO

schools :: [String]
schools = [
          "chemistry",
          "computing",
          "engineering",
          "ges",
          "mathematicsstatistics",
          "physics",
          "psychology"
         ]

main :: IO ()
main = do
     counts <- mapM numProfessors schools
     let results = zip schools counts
     mapM_ (\x -> putStrLn $ (fst x) ++ ": " ++ ((show.snd) x)) results
     execLaTeXT (simple results)  >>= renderFile "results.tex"



simple :: Monad m => [ (String, Maybe Int) ] -> LaTeXT_ m
simple results = do     
  thePreamble     
  document (theBody results)


thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
     documentclass [] article
     author "2310969m"
     title "COMPSCI4021 Coursework | Professor Web Scraper" 


theBody :: Monad m => [ (String, Maybe Int) ] -> LaTeXT_ m
theBody results = do    
  maketitle     
  section "Results"
  
  "The Results are: " 
  lnbk
  tabular Nothing [VerticalLine, LeftColumn, VerticalLine, RightColumn, VerticalLine] $ 
    hline <> foldMap
      (\(label, num) -> 
           fromString label 
        & case num of
            Just n -> fromString (show n) 
            Nothing -> "N/A"
        <> lnbk
        <> hline)
      results

     


     
