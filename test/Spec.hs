{-# LANGUAGE OverloadedLists #-}

import Control.Exception (assert)
import Control.Monad.Writer
    ( MonadIO (liftIO)
    , MonadWriter (tell)
    , WriterT
    , execWriterT
    )
import Parser
    ( eof
    , list
    , natural
    , recordP
    , skipSpaces
    , valueP
    , word
    )
import Parser.Type (ParserS, runParser)
import Value
    ( Record (Record)
    , Value (VInt, VList, VRecord, VString)
    )

main :: IO ()
main = do
    bs <- execWriterT test
    assert (and bs) $ pure ()

runTest :: ParserS a -> String -> Maybe a
runTest p = fmap snd . runParser (p <* eof)

assert' :: String -> Bool -> IO ()
assert' x True = putStrLn $ "OK: " ++ x
assert' x False = putStrLn $ "FAIL: " ++ x

assertTest
    :: Eq a
    => String
    -> ParserS a
    -> String
    -> a
    -> WriterT [Bool] IO ()
assertTest name p input expected = do
    let result = runTest p input == Just expected
    liftIO $ assert' name result
    tell [result]

test :: WriterT [Bool] IO ()
test = do
    assertTest "skipSpaces" skipSpaces "   " ()
    assertTest "word" word "hello" "hello"
    assertTest "natural" natural "123" 123
    assertTest "list" (list word) "[hello, world]" ["hello", "world"]
    assertTest "valueP VInt" valueP "123" (VInt 123)
    assertTest "valueP VString" valueP "hello" (VString "hello")
    assertTest
        "valueP VList of String"
        valueP
        "[hello, world]"
        $ VList [VString "hello", VString "world"]
    assertTest
        "valueP Record empty"
        recordP
        "{  }"
        $ Record mempty
    assertTest
        "valueP Record"
        recordP
        "{x:123}"
        $ Record [("x", VInt 123)]
    assertTest
        "valueP VRecord Int"
        valueP
        "{ x : 123 }"
        $ VRecord
        $ Record [("x", VInt 123)]
    assertTest
        "valueP VRecord String"
        valueP
        "{y : hello }"
        $ VRecord
        $ Record [("y", VString "hello")]
    assertTest
        "valueP VRecord List"
        valueP
        "{z : [hello, world]}"
        $ VRecord
        $ Record [("z", VList [VString "hello", VString "world"])]
    assertTest
        "valueP VRecord Int String"
        valueP
        "{y:hello,  x:123}"
        $ VRecord
        $ Record
            [ ("x", VInt 123)
            , ("y", VString "hello")
            ]
    assertTest
        "valueP VRecord nested record"
        valueP
        "{r : {x:123}}"
        $ VRecord
        $ Record
            [("r", VRecord $ Record [("x", VInt 123)])]

    assertTest
        "valueP VRecord Record List"
        valueP
        "{r : {x:123}, l : [hello, world]}"
        $ VRecord
        $ Record
            [ ("r", VRecord $ Record [("x", VInt 123)])
            , ("l", VList [VString "hello", VString "world"])
            ]
