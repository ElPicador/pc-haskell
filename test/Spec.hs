import Lib
  ( Result(..)
  , parseDigit
  , parseLowercase
  , parseThreeDigitsAsInt
  , parseThreeDigitsAsStr
  , pchar
  , andThen
  , orElse
  , run
  , sequenceP
  , many
  )

import Test.Hspec

pcharA = pchar 'A'

pcharB = pchar 'B'

pcharAThenB = pcharA `andThen` pcharB

pcharAOrB = pcharA `orElse` pcharB

manyA = many $ pchar 'A'

parsers = [ pchar 'A', pchar 'B', pchar 'C' ]
combined = sequenceP parsers

main :: IO ()
main =
  hspec $ do
    describe "pchar" $ do
      it "parses 'ABC'" $ do
        run pcharA "ABC" `shouldBe` Success ('A', "BC")

      it "parses 'ZBC'" $ do
        run pcharA "ZBC" `shouldBe` Failure "Expecting 'A'. Got 'Z'"

    describe "andThen" $ do
      it "parses 'ABC'" $ do
        run pcharAThenB "ABC" `shouldBe` Success (('A', 'B'), "C")

      it "parses 'ZBC" $ do
        run pcharAThenB "ZBC" `shouldBe` Failure "Expecting 'A'. Got 'Z'"

      it "parses 'AZC'" $ do
        run pcharAThenB "AZC" `shouldBe` Failure "Expecting 'B'. Got 'Z'"

    describe "orElse" $ do
      it "parses 'ABC'" $ do
        run pcharAOrB "ABC" `shouldBe` Success ('A', "BC")

      it "parses 'BBC'" $ do
        run pcharAOrB "BBC" `shouldBe` Success ('B', "BC")

      it "parses 'ZBC'" $ do
        run pcharAOrB "ZBC" `shouldBe` Failure "Expecting 'B'. Got 'Z'"

    describe "parseLowercase" $ do
      it "parses 'aBC'" $ do
        run parseLowercase "aBC" `shouldBe` Success ('a', "BC")

    describe "parseDigit" $ do
      it "parses '1ABC'" $ do
        run parseDigit "1ABC" `shouldBe` Success ('1', "ABC")

    describe "parseThreeDigitsAsStr" $ do
      it "parses '123A'" $ do
        run parseThreeDigitsAsStr "123A" `shouldBe` Success ("123", "A")

    describe "parseThreeDigitsAsInt" $ do
      it "parses '123A'" $ do
        run parseThreeDigitsAsInt "123A" `shouldBe` Success (123, "A")

    describe "sequence" $ do
      it "parses 'ABCD'"$ do
        run combined "ABCD" `shouldBe` Success (['A', 'B', 'C'], "D")

    describe "many" $ do
      it "parses 'ABCD'" $ do
        run manyA "ABCD" `shouldBe` Success (['A'], "BCD")

      it "parses 'AACD'" $ do
        run manyA "AACD" `shouldBe` Success (['A', 'A'], "CD")

      it "parses 'AAAD'" $ do
        run manyA "AAAD" `shouldBe` Success (['A', 'A', 'A'], "CD")

      it "parses '|BCD'" $ do
        run manyA "|BCD" `shouldBe` Success ([], "|BCD")
