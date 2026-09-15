module Exercise6 where

-- Time spent: 100mins

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Matthew: Carl didn't do it, and neither did I.
--          thief /= Carl && thief /= Matthew
-- Peter: It was Matthew or it was Jack
--        thief == Matthew || thief == Jack
-- Jack: Matthew and Peter are both lying.
--       not (accuses Matthew thief) && not (accuses Peter thief) -- lying implies that their whole accusation should be negated
-- Arnold: Matthew or Peter is speaking the truth, but not both.
--         accuses Matthew thief XOR accuses Peter thief -- not both is important here; without it, OR would take place instead
-- Carl: What Arnold says is not true.
--       not (accuses Arnold thief)

accuses :: Boy -> Boy -> Bool
accuses Matthew b = b /= Carl && b /= Matthew
accuses Peter b = b == Matthew || b == Jack
accuses Jack b = not (accuses Matthew b) && not (accuses Peter b)
accuses Arnold b = accuses Matthew b /= accuses Peter b -- https://stackoverflow.com/questions/31484936/write-xor-in-haskell-with-functors
accuses Carl b = not (accuses Arnold b)

accusers :: Boy -> [Boy]
accusers x = filter (\b -> b `accuses` x) boys

-- Since teacher mentioned that 3 boys always tell the truth, and 2 always lie,
-- we can just assume that the guy who is accused by 3 boys, is the guilty one.
guilty :: [Boy]
guilty = filter (\b -> length (accusers b) == 3) boys

-- Honest boys are the ones who accused the guilty one
honest :: [Boy]
honest = accusers (head guilty)

main :: IO ()
main = do
  putStrLn "Exercise 6:"
  putStrLn "Accusers for each suspect:"
  mapM_ (\b -> putStrLn $ "  " ++ show b ++ " is accused by: " ++ show (accusers b)) boys
  putStrLn $ "\nGuilty:  " ++ show guilty
  putStrLn $ "Honest: " ++ show honest

-- === Output ===
-- Exercise 6:
-- Accusers for each suspect:
--   Matthew is accused by: [Peter,Arnold]
--   Peter is accused by: [Matthew,Arnold]
--   Jack is accused by: [Matthew,Peter,Carl]
--   Arnold is accused by: [Matthew,Arnold]
--   Carl is accused by: [Jack,Carl]

-- Guilty:  [Jack]
-- Honest: [Matthew,Peter,Carl]