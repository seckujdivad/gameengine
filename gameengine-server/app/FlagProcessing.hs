module FlagProcessing (Flag (..), getFlags, getFlagUsageInfo) where

import System.Console.GetOpt (OptDescr (Option), ArgDescr (ReqArg, NoArg), ArgOrder (Permute), getOpt, usageInfo)

data Flag = WorkingDirectory String
    | Help
    deriving (Show)

options :: [OptDescr Flag]
options = [
        Option [] ["cwd"] (ReqArg WorkingDirectory "working directory") "set working directory",
        Option ['h'] ["help"] (NoArg Help) "show help"
    ]

getFlags :: [String] -> Either [Flag] String
getFlags rawFlags = do
    let (flags, unrecognisedOptions, errors) = getOpt Permute options rawFlags --permute = allow any argument order
    if null errors && null unrecognisedOptions then
        Left flags
    else do
        Right ((if null unrecognisedOptions then "" else "\nUnrecognised options:\n" ++ unlines unrecognisedOptions)
            ++ (if null errors then "" else "\nErrors:\n" ++ unlines errors)
            ++ getFlagUsageInfo)

getFlagUsageInfo :: String
getFlagUsageInfo = usageInfo "gameengine-server.exe options:" options