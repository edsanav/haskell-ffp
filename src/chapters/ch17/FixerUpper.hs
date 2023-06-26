module FixerUpper where




run :: IO()
run = do
  let ex1 = const <$> Just "Hello" <*> Just "World"
  print ex1
  let ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]
  print ex2
  
  
