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

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS

--wordCount
--Counts the amount of times a word occurs in a given document
--PRECONDITIONS Case sensitive; Lower case and upper case will be treated differently
--RETURNS a list where the tuples containing a string and an integer represent the word and the number of times a word occurs in the document
--Side effects: None
--Example: wordCount [["Hello", "world"], [Bye", "world"]] -> [("Hello",1), ("world",2), ("Bye", 1)]
wordCount :: Document -> WordTally
wordCount doc = getTally (insertionSort (docToSentence doc)) 1

--getTally
--Counts the amount of times an element occurs in a list
--PRECONDITIONS Same equivalence type of the arguements
--RETURNS a list containing each element in the list together with the amount of times it occurs in the list as a tuple
--SIDE EFFECTS none
--EXAMPLE getTally [True, False, True, False, False] -> [(True, 2),(False, 3)]
--VARIANT: length xs
getTally :: Eq a => [a] -> Int -> [(a,Int)]
getTally [] _ = []
getTally [x] n = [(x,n)]
getTally (x:xs) n 
  | x == head xs = getTally xs (n+1)
  | otherwise = (x,n) : (getTally xs 1)

--Converts the entire document into one sentence (list of strings)
docToSentence :: Document -> Sentence
docToSentence [] = []
docToSentence (x:xs) = x ++ docToSentence(xs)

--Trivial, commonly used sorting algorithm
insertionSort xs = insertionSortAux [] xs
  where
    insertionSortAux :: Ord a => [a] -> [a] -> [a]
    insertionSortAux sorted [] = sorted
    insertionSortAux sorted (x:xs) = insertionSortAux (insert x sorted) xs
    --Trivial
    insert :: Ord a => a -> [a] -> [a]
    insert elem [] = [elem]
    insert elem (x:xs) 
      | elem < x = elem:x:xs
      | otherwise = x:(insert elem xs)

--adjacentPairs
--Results in all the adjacent pairs in the document
--PRECONDITIONS Needs to be at least two words in the document for a pair to be returned
--RETURNS: Tuples of strings containing the adjacent words in the document
--SIDE EFFECTS: none
--EXAMPLE adjacentPairs [["Hello", "world"], ["Bye","bye", "world"], ["Word"]] -> [("Hello", "world"), [("Bye","bye"), ("bye", "world")]
adjacentPairs :: Document -> Pairs
adjacentPairs doc = concat (map adjacentPairsAux doc)
  where
    --VARIANT: length xs
    adjacentPairsAux :: Sentence -> Pairs
    adjacentPairsAux [x] = []
    adjacentPairsAux (x1:x2:xs) = (x1,x2) : (adjacentPairsAux (x2:xs))

--initialPairs
--Writes out the first two words in a sentence
--PRECONDITIONS must be sentences consisting of words of 2 or more to create a pair
--RETURNS: a list of tuples containing the first two elements in each sentence from the document
--SIDE EFFECTS: none
--EXAMPLE: initialPairs [["Cheerio", "ole", "chap!"]] -> [("Cheerio", "ole")]
--VARIANT: length xs
initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (x:xs) 
  | length x < 2 = []
  | otherwise = initialPairsAux x : (initialPairs (xs))
      where
        initialPairsAux :: Sentence -> (String,String)
        initialPairsAux (x1:x2:xs) = (x1,x2)

--finalPairs
--Writes out the last two words in a sentence
--PRECONDITIONS must be sentences consisting of words of 2 or more to create a pair
--RETURNS: a list of tuples containing the last two elements in each sentence from the document
--SIDE EFFECTS: none
--EXAMPLE: finalPairs [["Cheerio", "ole", "chap!"]] -> [("ole","chap!")]
--VARIANT: length xs
finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (x:xs) 
  | length x < 2 = []
  | otherwise = finalPairsAux x : (finalPairs(xs))
      where
        finalPairsAux :: Sentence -> (String,String)
        finalPairsAux [x1,x2] = (x1,x2)
        finalPairsAux (_:xs) = finalPairsAux xs



--pairsCount
--Results in a tally of the pairs occuring in the list of pairs
--PRECONDITIONS: Case sensitive when comparing words
--RETURNS:  pair of tuples in another tuple together with the amount of times it occurs in the form of an integer and all of this in a list of course
--SIDE EFFECTS: none
--EXAMPLE: pairsCount [("big","bear"),("bear","big"),("big","dog")] -> [(("bear","big"),2), (("big","dog"),1)]
pairsCount :: Pairs -> PairsTally
pairsCount pairs = getTally (insertionSort (map pairSort pairs)) 1
  where
    --pairSort
    --Sorts a given pair of ordered elements
    --PRECONDITION: Ordered elements of equal type
    --RETURNS: A sorted pair
    --EXAMPLE: pairSort (2,0) -> (0,2)
    pairSort :: Ord a => (a,a) -> (a,a)
    pairSort (x,y) 
      | x < y = (x,y)
      | otherwise = (y,x)

--Computes the times a given word's neighbour occurs within pairsTally
--PRECONDITION Case sensitive.
--SIDE EFFECTS: None
--RETURNS A list containing all neighbours to a given word and the number of occurences as a tuple
--EXAMPLE: neighbours [(("bear","big"),2),(("big","dog"),1)] "big" -> [("bear",2),("dog",1)]
--VARIANT: length xs
neighbours :: PairsTally -> String -> WordTally
neighbours [] _ = []
neighbours (((x1,x2),y):xs) word 
  | x1 == word = (x2,y) : (neighbours xs word)
  | x2 == word = (x1,y) : (neighbours xs word)
  | otherwise = neighbours xs word

--Results in the the word that occurs the most as a neighbour to given word
--PRECONDITIONS: Case sensitive.
--RETURNS Just: a string of given word's most common neighbour in pairsTally
--        Nothing: nothing returns. When pairsTally doesn't contain the given word.
--SIDE EFFECTS: none
--EXAMPLE: mostCommonNeighbour  [(("bear","big"),2),(("big","dog"),1)] "big" -> Just "bear"
--EXAMPLE: mostCommonNeighbour  [(("bear","big"),2),(("big","dog"),1)] "bug" -> Nothing
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour pairs word 
  | pairs `contains` word = Just (mostCommonNeighbourAux (neighbours pairs word))
  | otherwise = Nothing
      where
        --mostCommonNeighbourAux
        --Computes the word with the maximum occurence in a word tally
        --PRECONDITIONS: none
        --RETURNS: The word with the highest tally
        --EXAMPLE: mostCommonNeighbourAux [("hej",2),("då",3)] -> "då"
        --VARIANT: length xs
        mostCommonNeighbourAux :: WordTally -> String
        mostCommonNeighbourAux [(x1,x2)] = x1
        mostCommonNeighbourAux (x@(x1,x2):y@(y1,y2):zs) 
          | x2 > y2 = mostCommonNeighbourAux (x:zs)
          | otherwise = mostCommonNeighbourAux (y:zs)
          
        --contains
        --Checks if a word is contained within a pairsTally
        --RETURNS: True if the word is found
        --         False otherwise
        --EXAMPLE: contains [("hej","då",3),("koko","jojo",2)] "koko" -> True
        --VARIANT: length xs
        contains :: PairsTally -> String -> Bool
        contains [] _ = False
        contains (((x1,x2),_):xs) word 
          | x1 == word || x2  == word = True
          | otherwise = contains xs word

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




