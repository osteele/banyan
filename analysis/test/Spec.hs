import Test.Tasty
import Test.Tasty.HUnit

import FilePathExtras
import Serialize

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [filePathTests, serializationTests]

serializationTests = testGroup "Serialization tests"
    [ testCase "decodePaths" $ do
        decodePaths ["a"] @?= ["/a"]
        decodePaths ["a", "b"] @?= ["/a", "/b"]
        decodePaths ["a/", "b"] @?= ["/a/", "/a/b"]
        decodePaths ["a/", "b", "c/", "d"] @?= ["/a/", "/a/b", "/a/c/", "/a/c/d"]
        decodePaths ["a/", "b", "/c/", "d"] @?= ["/a/", "/a/b", "/c/", "/c/d"]

    , testCase "encodePaths" $ do
        encodePaths ["/a"] @?= ["a"]
        encodePaths ["/a", "/b"] @?= ["a", "b"]
        encodePaths ["/a/", "/a/b"] @?= ["a/", "b"]
        encodePaths ["/a/", "/a/b", "/a/c/", "/a/c/d"] @?=  ["a/", "b", "c/", "d"]
        encodePaths ["/a/", "/a/b", "/c/", "/c/d"] @?= ["a/", "b", "/c/", "d"]
    ]

filePathTests = testGroup "FilePathExtras tests"
    [ testCase "isDirectory" $ do
        isDirectory "dir/" @?= True
        isDirectory "file" @?= False

    , testCase "makeRelativeWithDots" $ do
        let mr = makeRelativeWithDots
        mr "/" "/a" @?= "a"
        mr "/" "/dir/a" @?= "dir/a"
        mr "/dir" "/a" @?= "../a"
        mr "/dir/" "/dir/a" @?= "a"
        mr "/d1/d2/" "/d1/a" @?= "../a"
        mr "/d1/d2/d3/" "/d1/d2/a" @?= "../a"
        mr "/d1/d2/d3/" "/d1/a" @?= "../../a"
        mr "/d1/d2/d3/" "/d4/a" @?= "../../../d4/a"

    , testCase "makeShortestRelative" $ do
        let mr = makeShortestRelative
        mr "/dir/" "/a" @?= "/a"
        mr "/dir/" "/dir/a" @?= "a"
        mr "/d1/d2/" "/d1/d2/a" @?= "a"
        mr "/d1/d2/" "/d1/d3/a" @?= "../d3/a"
        mr "/d1/d2/d3/" "/d1/d2/d3/a" @?= "a"
        mr "/d1/d2/d3/" "/d1/d2/a" @?= "../a"
        mr "/d1/d2/d3/" "/d1/a" @?= "/d1/a"
        mr "/long-name/d2/d3/" "/long-name/a" @?= "../../a"
    ]
