import Test.Hspec
import Lib (pchar, run, (.>>.), (<|>), Result(..))

pcharA = pchar 'A'
pcharB = pchar 'B'
pcharAThenB = pcharA .>>. pcharB
pcharAOrB = pcharA <|> pcharB

main :: IO ()
main = hspec $ do
  describe "pchar" $ do
    it "parses 'ABC'" $ do
      run pcharA "ABC" `shouldBe` Success ('A', "BC")

    it "parses 'ZBC'" $ do
      run pcharA "ZBC" `shouldBe` Failure "Expecting 'A'. Got 'Z'"

  describe "andThen" $ do
    it "parses 'ABC'" $ do
      run pcharAThenB "ABC" `shouldBe` Success (('A', 'B'), "C")

    it "parses 'ZBC'" $ do
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
