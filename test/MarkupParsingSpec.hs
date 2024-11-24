module MarkupParsingSpec where

import Test.Hspec
import HsBlog.Markup

spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    it "empty" $
      shouldBe
        (parse "")
        []

