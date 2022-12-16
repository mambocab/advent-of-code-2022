import Data.List (partition)
import Data.Maybe (maybeToList)
import GHC.List (foldl')
import Text.Read (readMaybe)

data Instruction = Add Int | Noop
  deriving (Show)

type Program = [Instruction]

data PipelinedInstruction = PipelinedInstruction
  { instruction :: Instruction,
    countDown :: Int
  }
  deriving (Show)

decrement :: PipelinedInstruction -> PipelinedInstruction
decrement PipelinedInstruction {instruction = i, countDown = c} =
  PipelinedInstruction {instruction = i, countDown = c - 1}

data UserSpace = UserSpace
  { program :: Program, -- Instructions yet to be executed
    regX :: Int, -- Contents of the X register
    pipeline :: [PipelinedInstruction], -- Instructions enqueed for execution
    counter :: Int -- What cycle are we on? Starts at 1, incrememts after each Instruction is popped off of program.
  }
  deriving (Show)

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (h : t) = t

newUserSpace :: Program -> UserSpace
newUserSpace p =
  UserSpace
    { program = p,
      regX = 1,
      pipeline = [],
      counter = 1
    }

-- stepPipeline takes a pipeline, decrements all PipelinedInstruction, and returns all instructions that are
-- ready to execute and the remaining instructions in the pipeline.
stepPipeline :: [PipelinedInstruction] -> ([Instruction], [PipelinedInstruction])
stepPipeline plis = (doneInst, decremented)
  where
    (done, decremented) = partition ((<= 0) . countDown) $ map decrement plis
    doneInst = map instruction done

finishInstruction :: Instruction -> Int -> Int
finishInstruction (Add n) i = n + i
finishInstruction Noop i = i

step :: UserSpace -> UserSpace
step u@UserSpace {program = [], pipeline = []} = u
step UserSpace {program = pg, regX = r, pipeline = pl, counter = c} = UserSpace {program = tailSafe pg, regX = r', pipeline = pl', counter = c + 1}
  where
    (popped, remainingPl) = stepPipeline pl
    r' = foldr finishInstruction r popped
    pl' =
      remainingPl ++ case head pg of
        Noop -> []
        Add i -> [PipelinedInstruction {instruction = Add i, countDown = 2}]

executeUntilDone :: UserSpace -> [UserSpace]
executeUntilDone u = takeWhileInclFinal $ iterate step u
  where
    takeWhileInclFinal [] = []
    takeWhileInclFinal (x : xs) =
      if done x
        then [x]
        else x : takeWhileInclFinal xs

done :: UserSpace -> Bool
done UserSpace {program = [], pipeline = []} = True
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

main :: IO ()
main = do
  text <- readFile "example.txt"
  case inputP text of
    Left err -> fail err
    Right parsed -> do
      print $ executeUntilDone $ newUserSpace parsed

--   print $ firstMatchingOneOfEach (map counter [20, 60, 100, 140, 180, 220])
