import           Data.List

import           Test.Tasty
import           Test.Tasty.HUnit

import           FilePathExtras
import           ListExtras
import           Serialize

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [listTests, filePathTests, serializationTests]

listTests =
  testGroup "List tests"
    [ testCase "invariant" $ do
        invariant (min 10 . (1 +)) 1 @?= 10
        invariant (`div` 2) 100 @?= 0

    , testCase "shortest" $ do
        shortest [id] "word" @?= "word"
        shortest [id, tail] "hedge" @?= "edge"
        shortest [id, init, init . tail] "hedges" @?= "edge"

    , testCase "withSentinel" $ do
        withSentinel 0 id [2, 3] @?= [2, 3]
        withSentinel 0 (map (+ 1)) [1, 2] @?= [2, 3]
        withSentinel 0 (\a -> zip (init a) (tail a)) [1, 2, 3] @?=
          [(1, 2), (2, 3)]
    ]

serializationTests =
  testGroup "Serialization tests"
    [ testCase "decodePaths" $ do
        decodePaths ["a"] @?= ["/a"]
        decodePaths ["a", "b"] @?= ["/a", "/b"]
        decodePaths ["a/", "b"] @?= ["/a/", "/a/b"]
        decodePaths ["a/", "b", "c/", "d"] @?=
          ["/a/", "/a/b", "/a/c/", "/a/c/d"]
        decodePaths ["a/", "b", "/c/", "d"] @?= ["/a/", "/a/b", "/c/", "/c/d"]

    , testCase "encodePaths" $ do
        encodePaths ["/a"] @?= ["a"]
        encodePaths ["/a", "/b"] @?= ["a", "b"]
        encodePaths ["/a/", "/a/b"] @?= ["a/", "b"]
        encodePaths ["/a/", "/a/b", "/a/c/", "/a/c/d"] @?=
          ["a/", "b", "c/", "d"]
        encodePaths ["/a/", "/a/b", "/c/", "/c/d"] @?= ["a/", "b", "/c/", "d"]

    , testCase "encodePathsWithDots" $ do
        encodePathsWithDots ["/a/b/c/", "/a/b/c/d"] @?= ["a/b/c/", "d"]
        encodePathsWithDots ["/a/b/c/", "/a/b/e"] @?= ["a/b/c/", "../e"]
    ]

filePathTests =
  testGroup "FilePathExtras tests"
    [ testCase "compareByDirectory" $
       do
        -- files are sorted alphabetically
        compareByDirectory "/a/b" "/a/b" @?= EQ
        compareByDirectory "/a/b" "/a/c" @?= LT
        compareByDirectory "/a/c" "/a/b" @?= GT
        compareByDirectory "/a/c" "/b/c" @?= LT
        compareByDirectory "/b/c" "/a/c" @?= GT

        -- directories are sorted alphabetically
        compareByDirectory "/a/b/" "/a/b/" @?= EQ
        compareByDirectory "/a/b/" "/a/c/" @?= LT
        compareByDirectory "/a/c/" "/a/b/" @?= GT

        -- files precede directories in the same parent directory
        compareByDirectory "/a/b" "/a/c/" @?= LT
        compareByDirectory "/a/c" "/a/b/" @?= LT
        compareByDirectory "/a/b/" "/a/c" @?= GT
        compareByDirectory "/a/c/" "/a/b" @?= GT

        -- entries in different parent directories are sorted alphabetically
        compareByDirectory "/a/b/c" "/a/b/a" @?= GT

        -- putting it all together
        sortBy compareByDirectory ["/a/b/c", "/a/b/a", "/a/c", "/a/d"] @?=
          ["/a/c", "/a/d", "/a/b/a", "/a/b/c"]

    , testCase "isDirectory" $ do
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

    , testCase "makeRelativeIfShorter makeRelativeWithDots" $ do
        let mr = makeRelativeIfShorter makeRelativeWithDots
        mr "/dir/" "/a" @?= "/a"
        mr "/dir/" "/dir/a" @?= "a"
        mr "/d1/d2/" "/d1/d2/a" @?= "a"
        mr "/d1/d2/" "/d1/d3/a" @?= "../d3/a"
        mr "/d1/d2/d3/" "/d1/d2/d3/a" @?= "a"
        mr "/d1/d2/d3/" "/d1/d2/a" @?= "../a"
        mr "/d1/d2/d3/" "/d1/a" @?= "/d1/a"
        mr "/long-name/d2/d3/" "/long-name/a" @?= "../../a"

    , testCase "makeRelativeMultidots" $ do
        let mr = makeRelativeWithMultidots
        mr "/d1/d2/d3/" "/d1/a" @?= ".../a"
        mr "/d1/d2/d3/" "/d4/a" @?= "..../d4/a"

    , testCase "makeRelativeIfShorter makeRelativeWithMultidots" $ do
        let mr = makeRelativeIfShorter makeRelativeWithMultidots
        mr "/d1/d2/d3/" "/d1/a" @?= "/d1/a"
        mr "/d1/d2/d3/" "/d1/d2/a" @?= "../a"
    ]
