module Main where

import Options.Applicative (Parser, strOption, switch, long
  , metavar, help, short, showDefault, option, auto, value
  , helper, fullDesc, progDesc, header, info, (<**>)
  , ParserInfo, execParser)
-- import Options.Applicative.Types.ParserInfo
import Data.Semigroup ((<>))

data Sample = Sample
  { hello      :: String
  , quite      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
  <$> strOption
      ( long "hello"
      <> metavar "TARGET"
      <> help "Target for the greeting" )
  <*> switch
      ( long "quite"
      <> short 'q'
      <> help "Target for the greeting")
  <*> option auto
      ( long "enthusiasm"
      <> help "How enthusiastically to greet"
      <> showDefault
      <> value 1
      <> metavar "INT")

opts :: ParserInfo Sample
opts = info (sample <**> helper)
       ( fullDesc
       <> progDesc "Print a greeting for TARGET"
       <> header "hello - a test for optparse-applicative")

greet :: Sample -> IO ()
greet (Sample h False n) = 
  putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()

main :: IO ()
main = do
  options <- execParser opts
  greet options

main2 :: IO ()
main2 = 
  execParser opts
  >>=
  greet

