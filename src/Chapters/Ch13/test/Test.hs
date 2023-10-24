module Test where
import Test.QuickCheck
import VigCipher
import Data.List (intersperse)


vigGen::Gen (String, String)
vigGen = do
  ws <- listOf1 $ listOf1 $ elements ['a'..'z']
  ciph <- listOf1 $ elements ['a'..'z']
  return (concat ws, ciph)

cesGen::Gen (String, Int)
cesGen = do
  ciph <- arbitrary::Gen Int
  ws <- listOf1 $ listOf1 $ elements ['a'..'z']
  return (concat ws, ciph)

vigAndBack::Property
vigAndBack = forAll vigGen
  (\(w, ciph) -> w == unvigne (vigne w ciph) ciph)

cesAndBack::Property
cesAndBack = forAll cesGen
  (\(w, n) -> w ==  uncaesar (caesar w n) n)


main :: IO ()
main = do
  quickCheck vigAndBack
  quickCheck cesAndBack