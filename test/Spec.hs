import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestCase (assertEqual "for (foo 3)," 2 3)

