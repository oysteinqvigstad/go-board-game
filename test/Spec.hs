import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc", "app/Main.hs"]
