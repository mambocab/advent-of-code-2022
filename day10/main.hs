import Data.List (partition, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, maybeToList)
import Distribution.Simple.Utils (safeTail)
import GHC.List (foldl')
import Text.Read (readMaybe)

data Instruction = Waiting Instruction | Add Int | Noop
  deriving (Show)

type Program = [Instruction]

data UserSpace = USp
  { program :: Program, -- Instructions yet to be executed
    x :: Int, -- Contents of the X register
    inProgress :: Maybe Instruction -- Instruction currently executing
  }
  deriving (Show)

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (h : t) = t

newUserSpace :: Program -> UserSpace
newUserSpace p =
  USp
    { program = p,
      x = 1,
      inProgress = Nothing
    }

x' :: UserSpace -> Int
x' USp {inProgress = Just (Add i), x = x} = i + x
x' USp {x = x} = x

-- Given the currently-executing instruction and the program, what should the next instruction & program be?
progState' :: UserSpace -> (Maybe Instruction, Program)
-- when waiting, advance inProgress but not program
progState' USp {inProgress = Just (Waiting i), program = p} = (Just i, p)
-- if we're not waiting and there's nothing left, we're done
progState' USp {program = []} = (Nothing, [])
-- special case: if the next listed instruction is Add, account for the 1-cycle wait before we execute
progState' USp {program = (Add i : rest)} = (Just $ Waiting $ Add i, rest)
-- everything else -- pop off the head and keep moving
progState' USp {program = (inst : rest)} = (Just inst, rest)

-- progState' USp {program = []} = (Nothing, [])

step :: UserSpace -> Maybe UserSpace
step USp {program = [], inProgress = Nothing} = Nothing
step u = Just USp {inProgress = ip', program = p', x = x' u} where (ip', p') = progState' u

touchingPixel :: Int -> Int -> Bool
touchingPixel pIdx x
  | x < 1 = False
  | x > 240 = False
  | otherwise = pIdx `mod` 40 `elem` [x - 1, x, x + 1]

executeUntilDone :: UserSpace -> [UserSpace]
executeUntilDone = catMaybes . takeWhile isJust . iterate (>>= step) . Just

done :: UserSpace -> Bool
done USp {program = [], inProgress = Nothing} = True
done _ = False

-- regX' = regX u + 1
-- pipeline' = tailSafe $ pipeline u

inputP :: String -> Either String [Instruction]
inputP = sequence <$> map instructionP . lines

instructionP :: String -> Either String Instruction
instructionP "noop" = Right Noop
instructionP s = case words s of
  [] -> Left $ "got empty list" ++ s
  ["addx", iStr] -> case readMaybe iStr of
    Just i -> Right $ Add i
    Nothing -> Left $ "can't parse " ++ iStr ++ " as Int"
  _ -> Left $ "got invalid line " ++ s

atIndexes :: [a] -> [Int] -> [a]
atIndexes [] _ = []
atIndexes xs is = atIndexes' (zip [0 ..] xs) (sort is)
  where
    atIndexes' [] _ = []
    atIndexes' _ [] = []
    -- assumes sorted indexes list
    atIndexes' xs (i : is) = (snd . head) back : atIndexes' (tail back) is
      where
        back = dropWhile ((< i) . fst) xs

-- atIndexes' xs (i : is) = head . snd back : atIndexes' (tail back) is
--   where
--     back = dropWhile ((< i) . fst) xs

-- atIndexes' xs (i : is) = head back : atIndexes' $ tail back is
--   where
--     back = drop i xs

-- printNumbered :: Show a => [a] -> IO ()
printNumbered [] = putStr "empty"
printNumbered xs = putStr . unlines $ zipWith (\n x -> show n ++ ". " ++ show x) [0 ..] $ map show xs

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs
  | n < 1 = error "group got invalid n"
  | length xs < n = []
  | otherwise = take n xs : group n (drop n xs)

main :: IO ()
main = do
  text <- readFile "input.txt"
  case inputP text of
    Left err -> fail err
    Right parsed -> do
      let allStates = executeUntilDone $ newUserSpace parsed
      -- part 1
      -- printNumbered allStates
      let is = [20, 60, 100, 140, 180, 220]
      let registerValues = map x $ atIndexes allStates is
      putStr "registers:\t"
      print registerValues
      let strengths = zipWith (*) registerValues is
      putStr "strengths:\t"
      print strengths
      putStr "sum:\t"
      print $ sum strengths

      -- part 2
      putStr $ unlines $ map (\s -> show (x s, inProgress s)) $ take 21 allStates
      let pixRegPairs = zip [0 ..] (tail $ map x allStates)
      -- print pixRegPairs
      let statusPerPixel = tail $ map (uncurry touchingPixel) pixRegPairs
      putStr $
        unlines $
          map (\line -> [if b then '#' else '.' | b <- line]) $
            group 40 statusPerPixel
