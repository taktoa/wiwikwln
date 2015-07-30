{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main module
module Main (main) where

import           Text.Pandoc      (def, Block (..), readMarkdown, writeMarkdown)
import           Text.Pandoc.Walk (walkM, Walkable)

-- Look up a name in a CodeBlock
lookupCB :: String -> Block -> Maybe String
lookupCB tag (CodeBlock (_, _, names) _) = lookup tag names
lookupCB _   _                           = Nothing

-- Mutator for the contents of a CodeBlock
updateContentsCB :: Block -> String -> Block
updateContentsCB (CodeBlock attr _) c = CodeBlock attr c
updateContentsCB block              _ = block

doInclude :: Block -> IO Block
doInclude cb = case lookupCB "include" cb of
                    Just f  -> updateContentsCB cb <$> readFile f
                    Nothing -> return cb
doInclude cb = return cb

doHTML :: Block -> IO Block
doHTML cb = case lookupCB "literal" cb of
                 Just f  -> RawBlock "html" <$> readFile f
                 Nothing -> return cb
doHTML cb = return cb

appInclude, appHTML :: Walkable Block b => b -> IO b
appInclude = walkM doInclude
appHTML    = walkM doHTML

-- Main function
main :: IO ()
main = do
  putStrLn "Hello world!"
  putStrLn "Hello world!"
  errPandoc <- appHTML =<< appInclude =<< (readMarkdown def <$> getContents)
  case errPandoc of
    Left  err    -> error ("Error in pandoc!" ++ "\n" ++ show err)
    Right pandoc -> putStrLn (writeMarkdown def pandoc)
