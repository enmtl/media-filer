{-# LANGUAGE GADTs, ExistentialQuantification, ScopedTypeVariables #-}
module Main 
  where

import System.IO (stdin, hSetBuffering, BufferMode(..))
import System.Directory

import System.Console.CmdArgs.Explicit hiding (mode)
import qualified System.Console.CmdArgs.Explicit as CmdArgsExplicit
import System.Console.CmdArgs.Text 

import Data.Monoid
import Data.List (intercalate, isPrefixOf)

import Control.Applicative
import Control.Pipe
import Control.Pipe.Utils
import qualified Control.Filer as Filer

class Flags a where 
    flags :: Group (Flag (Cmd a))

data AudioFormatD where
     AudioFormatAll :: AudioFormatD 
     AudioFormat :: { ogg :: Bool, flac :: Bool, mp3 :: Bool } -> AudioFormatD

class AudioFormat a where
    audioFormat :: a -> AudioFormatD
    modifyAudioFormat :: a -> (AudioFormatD -> AudioFormatD) -> a
    
instance AudioFormat AudioFormatD where
    audioFormat = id
    modifyAudioFormat a f = f a

newtype ListOpts = ListOpts AudioFormatD
newtype TagsOpts = TagsOpts AudioFormatD
                                        



data Cmd opts where
    Help    :: (Maybe String) -> Cmd ()
    ListOptions :: Maybe String -> Cmd ()
    Version :: Cmd ()
    Switch  :: HideCmd -> Cmd opts 
    Add     :: [FilePath] -> Cmd ()
    Edit    :: [FilePath] -> Cmd ()
    Tags    :: [FilePath] -> TagsOpts -> Cmd TagsOpts
    List    :: [FilePath] -> ListOpts -> Cmd ListOpts
    Put     :: FilePath -> Cmd ()
    View    :: Cmd ()
    Check   :: Cmd ()


instance Show (Cmd opts) where
    show (Help a)       = "Help " ++ show a
    show (ListOptions a) = "ListOptions " ++ show a
    show (Version)      = "Version"
    show (Switch cmd)   = "Switch " ++ show cmd 
    show (Add paths)    = "Add " ++ show paths
    show (Edit paths)   = "Edit " ++ show paths
    show (Tags paths _)   = "Tags " ++ show paths
    show (List paths _) = "List " ++ show paths
    show (Put path)     = "Path " ++ path
    show (View)         = "View"
    show (Check)        = "Check"
            

data HideCmd = forall a. Hide (Cmd a)
instance Show HideCmd where
    show (Hide a) = show a

class HideMode a where
    hide :: Remap m => m (Cmd a) -> m HideCmd

instance HideMode () where
    hide = remap Hide (\cmd -> (from cmd, Hide)) 
        where from (Hide x@(Help _)) = x
              from (Hide x@(ListOptions _)) = x
              from (Hide (Switch a)) = Switch a
              from (Hide x@(Version)) = x  
              from (Hide x@(Add _)) = x  
              from (Hide x@(Edit _)) = x  
              from (Hide x@(View)) = x  
              from (Hide x@(Put _)) = x  
              from (Hide x@(Check)) = x  

instance HideMode ListOpts where
    hide = remap Hide (\cmd -> (from cmd, Hide))
        where from (Hide x@(List _ _)) = x 
              from (Hide (Switch a)) = Switch a

instance HideMode TagsOpts where
    hide = remap Hide (\cmd -> (from cmd, Hide))
        where from (Hide x@(Tags _ _)) = x 
              from (Hide (Switch a)) = Switch a
        


setOpts :: Cmd opts -> opts -> Cmd opts
setOpts (List paths opts) opts' = List paths opts'
setOpts x _ = x

mode :: String -> (Cmd a) -> String -> Arg (Cmd a) -> [Flag (Cmd a)] -> [String] -> Mode (Cmd a)
mode name cmd help arg flags suffix= 
    mode' {modeGroupFlags=groupFlags, modeHelpSuffix=suffix}
  where 
    mode' = CmdArgsExplicit.mode name cmd help arg flags
    groupFlags = addedFlags `mappend` (modeGroupFlags mode')
    addedFlags = toGroup [helpFlag, listOptionFlag]
    helpFlag = flagHelpSimple (const $ Switch . Hide . Help $ Just name)
    listOptionFlag = flagNone ["list-options"] 
            (const $ Switch . Hide . ListOptions $ Just name) "List options"
        

file = "FILE"
file_ = file ++ "..."
dir = "DIR"
dir_ = dir ++ "..."
fileOrDir = "<FILE or DIRECTORY>"
fileOrDir_ = fileOrDir ++ "..."

type ModeCmd a = Mode (Cmd a)

arg :: (Update (Cmd a)) -> String -> Arg (Cmd a)
arg upd meta = flagArg upd' meta
    where upd' _ cmd@(Switch _) = Right $ cmd
          upd' name cmd = upd name cmd

add :: ModeCmd () 
add = mode "add" (Add []) "Add and rename audio files into the library" 
    (flagArg upd fileOrDir_) [] []
  where upd x cmd = Right $ case cmd of (Add paths) -> Add (x:paths); _ -> cmd

put :: ModeCmd () 
put = mode "put" (Put ".") "Copies the audio library to a new location" 
    (flagArg upd dir) [] []
  where upd x cmd = Right $ case cmd of (Put path) -> Put x; _ -> cmd

edit :: ModeCmd ()
edit = mode "edit" (Edit []) "Edits the tags of the audio file"
    (flagArg upd fileOrDir_) [] []
  where upd x cmd = Right $ case cmd of (Edit paths) -> Edit (x:paths); _ -> cmd

tags :: ModeCmd TagsOpts 
tags = mode "tags" (Tags [] $ TagsOpts AudioFormatAll) "Show the tags of the audio file"
    (arg upd fileOrDir_) [] []
  where upd x (Tags paths opts) = Right $ Tags (x:paths) opts

list :: ModeCmd ListOpts
list = mode "list" (List [] $ ListOpts AudioFormatAll) "List audio files"
    (arg upd fileOrDir_) [] []
  where upd x (List paths opts) = Right $ List (x:paths) opts 

check :: ModeCmd ()
check = mode "check" Check "Verifies the consistancy of the library" 
    (flagArg (\_ _ -> Left "Bad argument") "") [] []
  where upd x cmd = case cmd of Check -> Left "Bad argument"; _ -> Right cmd

view :: ModeCmd ()
view = mode "view" View "Views the contents of the repositiory" 
    (flagArg (\_ _ -> Left "Bad argument") "") [] []
  where upd x cmd = case cmd of View -> Left "Bad argument"; _ -> Right cmd


help :: ModeCmd ()
help = mode "help" (Help Nothing) "Display help about audio-filer and audio-filer commands"
    (flagArg upd "Cmd") [] []
  where upd cmdname (Help Nothing) = Right $ Help (Just cmdname)
        upd cmdname hlp = Right $ hlp

noArgFlags :: Group (Flag (Cmd ()))
noArgFlags = Group [] [] [("Other", [flagVersion (const $ Version)])]

globalFlags :: Group (Flag (Cmd ()))
globalFlags = Group [] [] [("Global", [flagHelpSimple (const $ Help Nothing)])] 


(+>) :: HideMode a => ModeCmd a -> [Mode HideCmd] -> [Mode HideCmd]
next +> cmds = hide next : cmds
infixr 9 +>

commands :: Mode HideCmd
commands = cmds
  where
    cmds' = (modes "audio-filer" (Hide $ Help Nothing) "manipulates audio files based on metadata" [])
        {modeGroupFlags=hide <$> noArgFlags `mappend` globalFlags
        ,modeArgs=([], Just (hide $ flagArg helpCmd ""))
        } 

    cmds = cmds' {modeGroupModes=groupModes}

    groupModes = Group [] []
        [("Commands", help +> [])
        ,("Changing or querying audio files", list +> edit +> tags +> [])
        ,("Changing or querying the library", add +> view +> [])
        ,("Administrating libraries", check +> put +> [])
        ]


    helpCmd arg (Help Nothing) = Right $ Help (Just arg)
    helpCmd _ cmd = Left "not a valid command"



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
        
  

addDefaultPath :: [FilePath] -> IO [FilePath]
addDefaultPath [] = (:[]) <$> getCurrentDirectory
addDefaultPath xs = return xs

{-
run :: IO ()
run = do
    hSetBuffering stdin NoBuffering
    let discard x = [] <$ x
    files <- runFrame $ toList <-< discard (Filer.move ".")
    mapM_ print files
-}
run :: Cmd a -> IO ()
run (Help arg) = case arg of
            Just arg' -> case cmdHelp arg' of
                Left [] -> putStrLn "No such command"
                Left xs -> putStrLn $ "Did you mean one of " ++ 
                                        intercalate ", " xs ++ "?"
                Right help -> print $ help
            Nothing -> print $ topLevelHelp 
run Version = putStrLn "version information"
run (List paths opts) = runFrame . Filer.list =<< addDefaultPath paths  
run (Switch cmd) = runHide cmd
run cmd = print cmd

runHide (Hide a) = run a

main = do 
    cmd <- processArgs commands
    runHide cmd
             

