{-# LANGUAGE GADTs, ExistentialQuantification #-} 
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Main 
  where

import System.IO (stdin, hSetBuffering, BufferMode(..))
import System.IO.Unsafe
import System.Directory

import System.Console.CmdArgs.Explicit hiding (mode)
import qualified System.Console.CmdArgs.Explicit as CmdArgsExplicit
import System.Console.CmdArgs.Text 

import Data.Monoid
import Data.Functor.Identity
import Data.Maybe
import Data.List (intercalate, isPrefixOf)
import Data.Filer

import Control.Applicative
import Control.Monad
import Control.Pipe
import Control.Pipe.Utils
import qualified Control.Filer as Filer

data Proxy a = Proxy
data Flag a = Default a | Return a
flag f _ (Default a) = f a
flag _ g (Return a) = g a
instance Resolveflag Flag Identity where
    resolveFlag (Default a) = Identity a
    resolveFlag (Return a) = Identity a


instance Default AudioFormat where
    def _ = Default (AudioFormat True True True)

dual (AudioFormat a b c) = AudioFormat (not a) (not b) (not c)

type ListFlags = HList (AudioFormat ': '[])
type TagsFlags = HList (AudioFormat ': '[])

    
instance Modify (Flag AudioFormat) l => Flags (Flag AudioFormat) l where
    flags _ = toGroup $ 
        [ flagNone ["ogg"] (mod ogg) "match on ogg format"
        , flagNone ["no-ogg"] (nomod ogg) "don't match on ogg format"
        , flagNone ["flac"] (mod flac) "match on flac format"
        , flagNone ["no-flac"] (nomod flac) "don't match on flac"
        , flagNone ["mp3"] (mod mp3) "match on mp3 format"
        , flagNone ["no-mp3"] (nomod mp3) "don't match on mp3"
        ]
      where 
        mod f = modify $ Return . f . flag dual id
        nomod f = modify $ Return . f . flag id id 
        
        ogg (AudioFormat b x y) = AudioFormat (not b) x y
        flac (AudioFormat x b y) = AudioFormat x (not b) y
        mp3 (AudioFormat x y b) = AudioFormat x y (not b)

audioFlags :: Flags a AudioFormat => [(Flag (Cmd a))]
audioFlags = fromGroup $ flags (Phantom :: Phantom AudioFormat)



data Cmd opts where
    Help    :: (Maybe String) -> Cmd ()
    ListOptions :: Maybe String -> Cmd ()
    Version :: Cmd ()
    Switch  :: HideCmd -> Cmd opts 
    Add     :: [FilePath] -> Cmd ()
    Edit    :: [FilePath] -> Cmd ()
    Tags    :: [FilePath] -> TagsFlags -> Cmd MTagsFlags
    List    :: [FilePath] -> ListFlags -> Cmd MListFlags
    Put     :: FilePath -> Cmd ()
    View    :: Cmd ()
    Check   :: Cmd ()


getFlags :: Cmd opts -> opts
getFlags (Help _)        = ()
getFlags (ListOptions _) = ()
getFlags (Version)       = ()
getFlags (Add _)         = ()
getFlags (Edit _)        = ()
getFlags (Tags _ opts)   = opts
getFlags (List _ opts)   = opts
getFlags (Put _)         = ()
getFlags (View)          = ()
getFlags (Check)         = ()

setFlags :: opts -> Cmd opts -> Cmd opts
setFlags _ x@(Help _) = x
setFlags _ x@(ListOptions _) = x
setFlags _ x@(Version) = x
setFlags _  x@(Switch _) = x 
setFlags _ x@(Add _) = x
setFlags _ x@(Edit _) = x
setFlags opts (Tags arg _) = Tags arg opts
setFlags opts (List arg _) = List arg opts
setFlags _ x@(Put _) = x
setFlags _ x@(View) = x
setFlags _ x@(Check) = x




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

instance HideMode (ListFlagsT Maybe) where
    hide = remap Hide (\cmd -> (from cmd, Hide))
        where from (Hide x@(List _ _)) = x 
              from (Hide (Switch a)) = Switch a

instance HideMode (TagsFlagsT Maybe) where
    hide = remap Hide (\cmd -> (from cmd, Hide))
        where from (Hide x@(Tags _ _)) = x 
              from (Hide (Switch a)) = Switch a
        


mode :: String -> (Cmd a) -> String -> [String] -> Arg (Cmd a) -> [Flag (Cmd a)] -> Mode (Cmd a)
mode name cmd help extendedHelp arg flags = 
    mode' {modeGroupFlags=groupFlags, modeHelpSuffix=extendedHelp}
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
add = mode "add" (Add []) "Add and rename audio files into the library" []
    (flagArg upd fileOrDir_) [] 
  where upd x cmd = Right $ case cmd of (Add paths) -> Add (x:paths); _ -> cmd

put :: ModeCmd () 
put = mode "put" (Put ".") "Copies the audio library to a new location" []
    (flagArg upd dir) [] 
  where upd x cmd = Right $ case cmd of (Put path) -> Put x; _ -> cmd

edit :: ModeCmd ()
edit = mode "edit" (Edit []) "Edits the tags of the audio file" []
    (flagArg upd fileOrDir_) [] 
  where upd x cmd = Right $ case cmd of (Edit paths) -> Edit (x:paths); _ -> cmd

tags :: ModeCmd MTagsFlags 
tags = mode "tags" (Tags [] def) "Show the tags of the audio file" []
    (arg upd fileOrDir_) 
    audioFlags
  where upd x (Tags paths opts) = Right $ Tags (x:paths) opts

list :: ModeCmd MListFlags
list = mode "list" (List [] def) "List audio files" []
    (arg upd fileOrDir_) 
    audioFlags
  where upd x (List paths opts) = Right $ List (x:paths) opts 

check :: ModeCmd ()
check = mode "check" Check "Verifies the consistancy of the library" []
    (flagArg (\_ _ -> Left "Bad argument") "") [] 
  where upd x cmd = case cmd of Check -> Left "Bad argument"; _ -> Right cmd

view :: ModeCmd ()
view = mode "view" View "Views the contents of the repositiory" []
    (flagArg (\_ _ -> Left "Bad argument") "") [] 
  where upd x cmd = case cmd of View -> Left "Bad argument"; _ -> Right cmd


help :: ModeCmd ()
help = mode "help" (Help Nothing) "Display help about audio-filer and audio-filer commands" []
    (flagArg upd "Cmd") [] 
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
run (List paths opts) = do
                let afmt = runIdentity . getAudioFormat . setDefaults $ opts
                print afmt
                runFrame . Filer.list afmt =<< addDefaultPath paths  
run (Switch cmd) = runHide cmd
run cmd = print cmd

runHide (Hide a) = run a

main = do 
    cmd <- processArgs commands
    runHide cmd
             

