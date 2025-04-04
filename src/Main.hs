module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Concurrent (threadDelay)

data Phase = Work | Break deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runPomodoro Work 10 0

runPomodoro :: Phase -> Int -> Int -> IO ()
runPomodoro phase seconds setCount =
  if seconds <= 0
     then do
        let (nextPhase, nextTime, nextSetCount) = case phase of
              Work -> (Break, 5, setCount + 1) -- ★ Work終わったら+1
              Break -> (Work, 10, setCount)    -- ★ Break終わっても+1しない
        runPomodoro nextPhase nextTime nextSetCount
     else do
        -- 表示するセット数を決める
        let displayedSetCount = case phase of
              Work -> setCount         -- Work中はそのまま
              Break -> setCount        -- Break中は増えた後（already +1されてる）
        putStr $ "\r" ++ show phase ++ " " ++ formatTime seconds ++ "  🍅 セット数: " ++ show displayedSetCount
        threadDelay 1000000
        runPomodoro phase (seconds - 1) setCount

formatTime :: Int -> String
formatTime n =
  let m = n `div` 60
      s = n `mod` 60
      pad x = if x < 10 then '0' : show x else show x
  in pad m ++ ":" ++ pad s
