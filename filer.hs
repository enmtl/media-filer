module Main 
  where

import System.IO (stdin, hSetBuffering, BufferMode(..))

import System.Console.CmdArgs.Explicit hiding (mode)
import qualified System.Console.CmdArgs.Explicit as CmdArgsExplicit
import System.Console.CmdArgs.Text 

import Data.Monoid
import Data.List (intercalate, isPrefixOf)

import Control.Applicative
import Control.Pipe
import qualified Control.Filer as Filer


data Cmd = None 
         | Help (Maybe String)
         | Version
         | Add [FilePath] 
         | Put FilePath 
         | Edit [FilePath] 
         | Tags [FilePath] 
         | Check 
  deriving Show

mode name cmd help arg flags suffix= 
    mode' {modeGroupFlags=groupFlags, modeHelpSuffix=suffix}
  where mode' = CmdArgsExplicit.mode name cmd help arg flags
        groupFlags = addedFlags `mappend` (modeGroupFlags mode')
        addedFlags = toGroup . (helpFlag:) . filter (notElem "help" . flagNames) 
                    .  fromGroup $ globalFlags
        helpFlag = flagHelpSimple (const $ Help (Just name))

file = "FILE"
file_ = file ++ "..."
dir = "DIR"
dir_ = dir ++ "..."
fileOrDir = "<FILE or DIRECTORY>"
fileOrDir_ = fileOrDir ++ "..."

add :: Mode Cmd
add = mode "add" (Add []) "Add and rename audio files into the library" 
    (flagArg upd fileOrDir_) [] []
  where upd x cmd = Right $ case cmd of (Add paths) -> Add (x:paths); _ -> cmd

put :: Mode Cmd
put = mode "put" (Put ".") "Copies the audio library to a new location" 
    (flagArg upd dir) [] []
  where upd x cmd = Right $ case cmd of (Put path) -> Put x; _ -> cmd

edit :: Mode Cmd
edit = mode "edit" (Edit []) "Edits the tags of the audio file"
    (flagArg upd fileOrDir_) [] []
  where upd x cmd = Right $ case cmd of (Edit paths) -> Edit (x:paths); _ -> cmd

tags :: Mode Cmd
tags = mode "tags" (Tags []) "Show the tags of the audio file"
    (flagArg upd fileOrDir_) [] []
  where upd x cmd = Right $ case cmd of (Tags paths) -> Tags (x:paths); _ -> cmd

check :: Mode Cmd
check = mode "check" Check "Verifies the consistancy of the library" 
    (flagArg (\_ _ -> Left "Bad argument") "") [] []
  where upd x cmd = case cmd of Check -> Left "Bad argument"; _ -> Right cmd


help :: Mode Cmd
help = mode "help" (Help Nothing) "Display help about audio-filer and audio-filer commands"
    (flagArg upd "Cmd") [] []
  where upd cmdname (Help Nothing) = Right $ Help (Just cmdname)
        upd cmdname hlp = Right $ hlp

noArgFlags :: Group (Flag Cmd)
noArgFlags = Group [] [] [("Other", [flagVersion (const $ Version)])]

globalFlags :: Group (Flag Cmd)
globalFlags = Group [] [] [("Global", [flagHelpSimple (const $ Help Nothing)])] 

commands :: Mode Cmd
commands = cmds
  where
    cmds' = (modes "audio-filer" None "manipulates audio files based on metadata" [])
        {modeGroupFlags=noArgFlags `mappend` globalFlags
        ,modeArgs=([], Just (flagArg helpCmd ""))
        } 

    cmds = cmds' {modeGroupModes=groupModes}

    groupModes = Group [] []
        [("Commands", [help])
        ,("Changing or querying an audio file", [edit, tags])
        ,("Changing the library", [add])
        ,("Administrating libraries", [check, put])
        ]


    helpCmd arg (Help Nothing) = Right $ Help (Just arg)
    helpCmd _ cmd = Left "not a valid command"

toList :: Frame a b IO [a]
toList = Frame go where 
  go = do
    x <- await 
    case x of 
        Nothing -> close $ pure []
        Just a -> fmap (fmap (a:)) go

run :: IO ()
run = do
    hSetBuffering stdin NoBuffering
    let discard x = [] <$ x
    files <- runFrame $ toList <-< discard (Filer.move ".")
    mapM_ print files

topLevelHelp :: [Text]
topLevelHelp = 
    (Line $ "Usage: " ++ (head . modeNames $ commands) ++ " Commands ... "):
        concatMap helpText (groupNamed . modeGroupModes $ commands)
 where
    cmdHelpText cmd = Cols [("  " ++) . head . modeNames $ cmd, ("  " ++) . modeHelp $ cmd]
    helpText (group, cmds) = 
        Line "" : (Line $ group ++ ":"): map cmdHelpText cmds 

cmdHelp :: String -> Either [String] [Text]
cmdHelp cmd = case possible of
        [x] -> Right $ helpText [] HelpFormatDefault x
        xs -> Left $ modes >>= filter (cmd `isPrefixOf`) . modeNames 
    where 
       modes = fromGroup . modeGroupModes $ commands
       possible = filter (any (cmd `isPrefixOf`) . modeNames) modes 
        
  


main = do 
    cmd <- processArgs commands
    case cmd of
        Help arg -> case arg of
            Just arg' -> case cmdHelp arg' of
                Left [] -> putStrLn "No such command"
                Left xs -> putStrLn $ "Did you mean one of " ++ 
                                        intercalate ", " xs ++ "?"
                Right help -> print $ help
            Nothing -> print $ topLevelHelp 
        Version -> print "version 3"
        _ -> do 
            print cmd
            run
             

