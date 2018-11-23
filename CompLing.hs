-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- Definitions for testing purposes below
sentence1 = ["Today", "was", "a", "good", "good", "day"]
sentence2 = ["Yesterday", "was","a", "really", "good","day"]
document = [sentence1, sentence2]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS

wordCount :: Document -> WordTally
wordCount doc = getTally (insertionSort (docToSentence doc)) 1

getTally :: Eq a => [a] -> Int -> [(a,Int)]
getTally [] _ = []
getTally [x] n = [(x,n)]
getTally (x:xs) n 
  | x == head xs = getTally xs (n+1)
  | otherwise = (x,n) : (getTally xs 1)
  
docToSentence :: Document -> Sentence
docToSentence [] = []
docToSentence (x:xs) = x ++ docToSentence(xs)

insertionSort xs = insertionSortAux [] xs

insertionSortAux :: Ord a => [a] -> [a] -> [a]
insertionSortAux sorted [] = sorted
insertionSortAux sorted (x:xs) = insertionSortAux (insert x sorted) xs

insert :: Ord a => a -> [a] -> [a]
insert elem [] = [elem]
insert elem (x:xs) 
  | elem < x = elem:x:xs
  | otherwise = x:(insert elem xs)

adjacentPairs :: Document -> Pairs
adjacentPairs doc = adjacentPairsAux (docToSentence doc)

adjacentPairsAux :: Sentence -> Pairs
adjacentPairsAux [x] = []
adjacentPairsAux (x1:x2:xs) = (x1,x2) : (adjacentPairsAux (x2:xs))

initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (x:xs) = initialPairsAux x : (initialPairs (xs))

initialPairsAux :: Sentence -> (String,String)
initialPairsAux (x1:x2:xs) = (x1,x2)

finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (x:xs) = finalPairsAux x : (finalPairs(xs))

finalPairsAux :: Sentence -> (String,String)
finalPairsAux [x1,x2] = (x1,x2)
finalPairsAux (_:xs) = finalPairsAux xs

pairsCount :: Pairs -> PairsTally
pairsCount pairs = getTally (insertionSort pairs) 1

neighbours :: PairsTally -> String -> WordTally
--neighbours = undefined  -- remove "undefined" and write your function here
neighbours [] _ = []
neighbours (x:xs) word 
  | fst (fst x) == word = (snd (fst x),snd x) : (neighbours xs word)
  | snd (fst x) == word = (fst (fst x),snd x) : (neighbours xs word)
  | otherwise = neighbours xs word


mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here



-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]]) 

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])
                      
test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])
                      

-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple" 
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky" 
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in 
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)] 
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a") 

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b") 

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun") 
                                                                  (mostCommonNeighbour input "the") 
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\"" 
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\"" 
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet") 

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]




