{-# LANGUAGE OverloadedLists #-}

import Control.Monad (unless)
import Control.Monad.Writer
    ( MonadIO (liftIO)
    , MonadWriter (tell)
    , WriterT
    , execWriterT
    )
import Data.TreeDiff (Edit, EditExpr, ToExpr, ediff, prettyEditExpr)
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
import System.Exit (exitFailure)
import Text.PrettyPrint (render)
import Value
    ( Record (Record)
    , Value (VInt, VList, VRecord, VString)
    )

main :: IO ()
main = do
    bs <- execWriterT test
    unless (null bs) exitFailure

runTest :: ParserS a -> String -> Maybe a
runTest p = fmap snd . runParser (p <* eof)

data Result = ParserProblem | ValueProblem (Edit EditExpr) | Ok

assert' :: String -> Result -> IO ()
assert' x Ok = putStrLn $ "Ok: " ++ x
assert' x ParserProblem = putStrLn $ "Parsing problem in: " ++ x
assert' x (ValueProblem e) = do
    putStrLn $ "Value difference on: " ++ x
    putStrLn $ render $ prettyEditExpr e

assertTest
    :: (Eq a, ToExpr a)
    => String
    -> ParserS a
    -> String
    -> a
    -> WriterT [()] IO ()
assertTest name p input expected = do
    case runTest p input of
        Just expected' ->
            if expected == expected'
                then liftIO $ assert' name Ok
                else do
                    liftIO $ assert' name $ ValueProblem $ ediff expected expected'
                    tell [()]
        Nothing -> do
            liftIO $ assert' name ParserProblem
            tell [()]

test :: WriterT [()] IO ()
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
    assertTest
        "valueP VRecord of list and record and natural and string"
        valueP
        "{ l : [hello, 123], r : {x:123}, i: 123, s: hello}"
        $ VRecord
        $ Record
            [ ("l", VList [VString "hello", VInt 123])
            , ("r", VRecord $ Record [("x", VInt 123)])
            , ("i", VInt 123)
            , ("s", VString "hello")
            ]
