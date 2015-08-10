{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- -*- mode: haskell -*-

-- What I Wish I Knew When Learning Nix - Shake build script
--
-- The build script that compiles the Markdown guide to a website
--
-- Written in 2015 by Remy Goldschmidt <taktoa@gmail.com>
-- Copyright © 2015 Remy Goldschmidt
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.



module Build (main) where

import qualified Data.DList                  as DL
import qualified Data.Text                   as T
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Language.Preprocessor.Cpphs
import           Text.Pandoc
import           Text.Pandoc.Walk

(//) = (<//>)

pat ~~> target = (norm pat) %> (target . norm)
  where
    norm = toNative . normaliseEx


mdFiles   = "public" // "*" <.> "md"
htmlFiles = "public" // "*" <.> "html"

htmlTarget :: FilePath -> Action ()
htmlTarget file = do
  liftIO $ putStrLn $ "HTML Target: " ++ file
  need name
  liftIO $ print name
  where
    name = case splitFileName $ dropExtension file of
      ("public", f) -> ["public" // f <.> "md"]
      _             -> []


mdTarget :: FilePath -> Action ()
mdTarget file = do
  contents <- readFile' $ file -<.> "macro"
  need [ contents ]
  liftIO $ putStrLn contents


--  unit $ command [] "cat" ["Makefile"]
--  liftIO $ putStrLn $ "MD Target: " ++
--  return ()

buildRules :: Rules ()
buildRules = do
  want ["public" // "tutorial" <.> "html"]
  htmlFiles ~~> htmlTarget
  mdFiles ~~> mdTarget

--  "" ~~> testTarget
--  "" ~~> testTarget
--  "" ~~> testTarget
--  "" ~~> testTarget

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build"

                              } buildRules
