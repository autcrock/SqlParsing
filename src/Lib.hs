module Lib
    ( examples
    ) where

import qualified System.IO.Strict as SIS
import System.Environment
import Text.Show.Pretty
import System.IO

import Language.SQL.SimpleSQL.Parse
        (parseStatements
        ,ParseError
        ,peFormattedError)

import Language.SQL.SimpleSQL.Syntax (Statement)
import Language.SQL.SimpleSQL.Dialect (Dialect, ansi2011, sqlserver, mysql, postgres)


examples :: IO ()
examples =
    -- example1 ansi2011 >>
    -- example1 sqlserver >>
    -- example1 mysql >>
    example1 postgres


example1 :: Dialect -> IO ()
example1 dialect = do
    args <- getArgs
    case args of
        [] -> getContents >>= doIt dialect                            -- read from stdin
        ["-s", sql] -> doIt dialect sql                                -- parse arg given
        [f] -> withFile f ReadMode SIS.hGetContents >>= doIt dialect      -- read file
        _ -> putStrLn "use no arguments to stream sql from stdin, e.g.:\n\
                        \  cat some.sql | SimpleSQLParserExample\n\
                        \n\
                        \use -s to parse sql on command line, e.g.:\n\
                        \  SimpleSQLParserExample -s \"select * from t\"\n\
                        \use a single arg to parse a file, e.g.\n\
                        \  SimpleSQLParserExample some.sql"

doIt :: Dialect -> String -> IO ()
doIt dialect src = do
    let parsed :: Either ParseError [Statement]
        parsed = parseStatements dialect "" (Just (1,1)) src
    either (error . peFormattedError)
            (putStrLn . ppShow)
            parsed
