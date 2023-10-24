module Ch03.Exercises where


mycon :: String -> String -> String
mycon s1 s2 = s1 ++ s2

takeNth :: String -> Int -> Char
takeNth s1 n = head (drop (n-1) s1)

myinit :: String -> String
myinit = init

mydrop :: Int -> String -> String
mydrop = drop

thirdLetter :: String -> Char
thirdLetter x = x !! 2

curryIs::String
curryIs = "Curry is awesome"

letterIndex :: Int -> Char
letterIndex x = curryIs !! x

rvrs :: String -> String
rvrs s = awe ++ space ++ is ++ space ++ curr
  where curr = take 5 s
        space = take 1 $ drop 5 s
        is = take 2 $ drop 6 s
        awe = take 7 $ drop 9 s
        
main :: IO ()
main = print $ rvrs curryIs
