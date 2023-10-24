module Filtering where


mult3 = filter (\x -> (rem x 3) == 0) [1..30]

mult3count = length . filter (\x -> (rem x 3) == 0)

removeArticles:: String -> [String]
removeArticles = filter (\x -> not (elem x ["the", "a", "an"])) . words 