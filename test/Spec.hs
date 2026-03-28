module Main (main) where

import Test.Hspec

import qualified TypesSpec
import qualified AppSpec
import qualified RedisSpec

main :: IO ()
main = hspec $ do
    describe "Types"  TypesSpec.spec
    describe "App"    AppSpec.spec
    describe "Redis"  RedisSpec.spec
