{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Command line options
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Options
	( Opts , Format(..), Compiler(..), Verbosity(..)
	, parseOptions
	, format, verbosity
	, compareWith, checkWith
	, outputPath, jsonPath
	-- , isComparing, isChecking
	) where

import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt(getOpt,OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo)
import System.FilePath.Posix (takeDirectory, addExtension, dropExtension, takeFileName, (</>))
import System.Exit (exitWith,ExitCode(..))

{------------------------------------------------------------------------------
    option handling
------------------------------------------------------------------------------}

-- the output format
data Format = JSON | CompactJSON | UnknownFormat String deriving(Show,Eq)

-- possible alternative compilers that we can compare output with
data Compiler = ScalaCompiler | OCamlCompiler | HPCompiler | NoCompare deriving(Show,Eq)

-- verbosity
data Verbosity = Normal | Verbose | Debug deriving(Show,Eq,Ord)

-- the (validated) data from the command line options
data Opts = Opts
	{ format :: Format
	, compareWith :: Compiler
	, checkWith :: Compiler
	, outputPath :: String
	, verbosity :: Verbosity
	}

-- the default options
defaults = Opts
	{ format = JSON
	, compareWith = NoCompare
	, checkWith = NoCompare
	, outputPath = ""
	, verbosity = Normal
	}

-- the command line flags
data OptionFlag = Output String | Compare String | Check String | Format String
				| VerboseOpt | DebugOpt deriving(Show,Eq)

options :: [OptDescr OptionFlag]
options =
	[ Option ['c'] ["compare"]	(ReqArg Compare "scala|ocaml|hp")	"compare with output of other compiler"
	, Option ['d'] ["debug"]	(NoArg DebugOpt)					"debug logging"
	, Option ['f'] ["format"] 	(ReqArg Format "json|compact")		"output format"
	, Option ['o'] ["output"]	(ReqArg Output "DIR")				"directory for json output"
	, Option ['q'] ["quickcheck"] (ReqArg Check "scala|ocaml|hp") 	"quickcheck"
	, Option ['v'] ["verbose"]	(NoArg VerboseOpt)					"verbose"
	]

parseOptions :: [String] -> IO (Opts, [String])
parseOptions argv = case getOpt RequireOrder options argv of
	(opts,fs,[]) -> do
		case (extractOptions opts) of
			(Left e)  -> do
				hPutStrLn stderr e
				exitWith (ExitFailure 1)
			(Right exopts) -> return (exopts,fs)
	(_,_,errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo usage options)
		exitWith (ExitFailure 1)
	where usage = "Usage: options file .."

extractOptions :: [OptionFlag] -> (Either String Opts)
extractOptions [] = Right defaults
extractOptions (f:fs) = do
	o <- extractOptions fs
	case f of
		VerboseOpt -> return $ o { verbosity = Verbose }
		DebugOpt -> return $ o { verbosity = Debug }
		(Output p) -> return $ o { outputPath = p }
		(Format fmt) -> case fmt of
			"json" -> return $ o { format = JSON }
			"compact" -> return $ o { format = CompactJSON }
			otherwise -> Left ( "invalid format: \"" ++ fmt ++ "\"" )
		(Compare c) -> case c of
			"scala" -> return $ o { compareWith = ScalaCompiler }
			"ocaml" -> return $ o { compareWith = OCamlCompiler }
			"hp" -> return $ o { compareWith = HPCompiler }
			otherwise -> Left ( "invalid compiler: \"" ++ c ++ "\"" )
		(Check c) -> case c of
			"scala" -> return $ o { checkWith = ScalaCompiler }
			"ocaml" -> return $ o { checkWith = OCamlCompiler }
			"hp" -> return $ o { checkWith = HPCompiler }
			otherwise -> Left ( "invalid compiler: \"" ++ c ++ "\"" )

-- where to put the json output:
-- the default is the same directory as the source
-- if the output arg is absolute, it is used as the directory for the output
-- if it is relative, it is interpreted relative to the source
-- "-" is interpreted as stdout

jsonPath :: String -> Opts -> String -> String
jsonPath srcPath opts postfix =
	if (((outputPath opts) == "-") && (postfix == ""))
		then "-"
		else ((takeDirectory srcPath) </> (outputPath opts) </>
			(addExtension ((dropExtension (takeFileName srcPath)) ++ postfix) ".json"))
