module GlobRegex
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' str =
    case str of
      ""             -> ""
      ('*':cs)       -> ".*" ++ globToRegex' cs
      ('?':cs)       -> '.'  :  globToRegex' cs
      ('[':'!':c:cs) -> "[^" ++ c : charClass cs
      ('[':c:cs)     -> '['  :  c : charClass cs
      ('[':_)        -> error "unterminated character class"
      (c:cs)         -> escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat
