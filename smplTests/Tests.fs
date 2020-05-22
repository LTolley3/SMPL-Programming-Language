namespace smplTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open ProjectParser
open ProjectInterpreter
open Library

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.PNumParsesNumbers () =
        let input = prepare "4"
        let expected = Number(4)
        let result = pnum input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.PNumDoesNotParseNonNumbers () =
        let input = prepare "test!"
        let result = pnum input
        match result with
        | Success(res,_) -> Assert.IsTrue(false)
        | Failure(_) -> Assert.IsTrue(true)

    [<TestMethod>]
    member this.PStringParsesASCII () =
        let input = prepare "abcABC123!#%"
        let expected = "abcABC123!#%"
        let result = pstring input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.SingleQuoteParsesSingleQuotes () =
        let input = prepare "'hello, world!'"
        let expected = String("hello, world!")
        let result = singlequote input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.DoubleQuoteParsesDoubleQuotes () =
        let input = prepare "\"hello, world!\""
        let expected = String("hello, world!")
        let result = doublequote input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.PQuotesDoesNotParseNonQuoted () =
        let input = prepare "hello, world!"
        let result = doublequote input
        match result with
        | Success(res,_) -> Assert.IsTrue(false)
        | Failure(_) -> Assert.IsTrue(true)

    [<TestMethod>]
    member this.ExprParsesNumber () =
        let input = prepare "4"
        let expected = Number(4)
        let result = expr input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.ExprParsesString () =
        let input = prepare "'hello, world!'"
        let expected = String("hello, world!")
        let result = expr input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.ExprParsesBuiltin () =
        let input = prepare "function1"
        let expected = Builtin("function1",[])
        let result = expr input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.ExprParsesSeq () =
        let input = prepare "function1 function2"
        let expected = Seq(Builtin("function1",[]), Builtin("function2", []))
        let result = expr input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.ArglistParsesArgList () =
        let input = prepare "'arg1', 2,\"arg3\", function"
        let expected = [String("arg1"); Number(2); String("arg3"); Builtin("function",[])]
        let result = arglist input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.ArglistParsesOneArg () =
        let input = prepare "'arg1'"
        let expected = [String("arg1")]
        let result = arglist input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.BuiltinNoParenParsesFunctionsWithoutParen () =
        let input = prepare "function"
        let expected = Builtin("function",[])
        let result = builtinNoParen input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.BuiltinParsesFunctions () =
        let input = prepare "function('arg1')"
        let expected = Builtin("function",[String("arg1")])
        let result = builtin input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.PMany1SeqParsesSeqs () =
        let input = prepare "function1('arg1') function2() function3"
        let expected = Seq(NOP,Seq(Builtin("function1",[String("arg1")]),Seq(Builtin("function2",[]),Builtin("function3",[]))))
        let result = pmany1seq expr input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.SequenceParsesSeqOfFunction () =
        let input = prepare "function(function1 function2)"
        let expected = Builtin("function",[Seq(Builtin("function1",[]),Builtin("function2",[]))])
        let result = expr input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.GrammarParsesValidProgram () =
        let input = prepare "function1(function2('arg1'),function3 function4(\"arg2\")) function5(4) function6()"
        let expected = Seq(NOP,Seq(Builtin("function1",[Builtin("function2",[String("arg1")]); Seq(Builtin("function3",[]), Builtin("function4",[String("arg2")]))]), Seq(Builtin("function5",[Number(4)]),Builtin("function6",[]))))
        let result = grammar input
        match result with
        | Success(res,_) -> Assert.AreEqual(res,expected)
        | Failure(_) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.GrammarDoesNotParseAnInvalidProgram () =
        let input = prepare "function)("
        let result = grammar input
        match result with
        | Success(res,_) -> Assert.IsTrue(false)
        | Failure(_) -> Assert.IsTrue(true)

    (* -=-=-=-=-=- End of Parser Tests, Beginning of Eval Tests -=-=-=-=-=- *)
    [<TestMethod>]
    member this.LengthGetsLength () =
        Assert.AreEqual((length "hello, world!"), 13)

    [<TestMethod>]
    member this.FirstGetsFirst () =
        Assert.AreEqual((first "hello, world!"), "h")

    [<TestMethod>]
    member this.LastGetsLast () =
        Assert.AreEqual((last "hello, world!"), "!")

    [<TestMethod>]
    member this.MiddleGetsMiddle () =
        Assert.AreEqual((middle "hello, world!"), "ello, world")

    [<TestMethod>]
    member this.GetEndGetsEnd () =
        Assert.AreEqual((getEnd "hello, world!"), 12)

    [<TestMethod>]
    member this.IsUpperSucceedsOnUpper () =
        Assert.AreEqual((isUpper "HELLO, WORLD!"), true)

    [<TestMethod>]
    member this.IsUpperFailsOnLower () =
        Assert.AreEqual((isUpper "hello, world!"), false)

    [<TestMethod>]
    member this.IsLowerSucceedsOnLower () =
        Assert.AreEqual((isLower "hello, world!"), true)

    [<TestMethod>]
    member this.IsLowerFailsOnUpper () =
        Assert.AreEqual((isLower "HELLO, WORLD!"), false)

    [<TestMethod>]
    member this.ToUpperSendsToUpper () =
        Assert.AreEqual((toUpper "hElLo, WoRlD!"), "HELLO, WORLD!")

    [<TestMethod>]
    member this.ToLowerSendsToLower () =
        Assert.AreEqual((toLower "hElLo, WoRlD!"), "hello, world!")

    [<TestMethod>]
    member this.IsPalindromSucceedsOnPalindrome () =
        Assert.AreEqual((isPalindrome "hello, ,olleh"), true)

    [<TestMethod>]
    member this.IsPalindromeFailsOnNonPalindrome () =
        Assert.AreEqual((isPalindrome "hello, world!"), false)

    [<TestMethod>]
    member this.ReverseReverses () =
        Assert.AreEqual((reverse "hello, world!"), "!dlrow ,olleh")

    [<TestMethod>]
    member this.RepeatRepeats () =
        Assert.AreEqual((repeat "hello, world!" 2), "hello, world!hello, world!")

    [<TestMethod>]
    member this.PrependPrepends () =
        Assert.AreEqual((prepend "world!" "hello, "), "hello, world!")

    [<TestMethod>]
    member this.AppendAppends () =
        Assert.AreEqual((append "hello," " world!"), "hello, world!")

    [<TestMethod>]
    member this.SubstringGetsSubstring () =
        Assert.AreEqual((substring "hello, world!" 1 3), "ell")

    [<TestMethod>]
    member this.ContainsSucceedsWhenContains () =
        Assert.AreEqual((contains "hello, world!" "hello"), true)

    [<TestMethod>]
    member this.ContainsFailsWhenDoesNotContain () =
        Assert.AreEqual((contains "hello, world!" "foo"), false)

    [<TestMethod>]
    member this.ReplaceReplaces () =
        Assert.AreEqual((replace "hello, world!" "hello" "goodbye"), "goodbye, world!")

    [<TestMethod>]
    member this.SubstringCountGetsCount () =
        Assert.AreEqual((substringCount "hello? hello, hello!" "hello"), 3)

    [<TestMethod>]
    member this.IsWordSucceedsOnWord () =
        Assert.AreEqual((isWord "hello" "../../../../lang/dict.txt"), true)

    [<TestMethod>]
    member this.IsWordFailsOnNonWord () =
        Assert.AreEqual((isWord "foo bar" "../../../../lang/dict.txt"), false)

    [<TestMethod>]
    member this.ShuffleKeepsLettersConstant () =
        let input = "hello, world!"
        let result = shuffle input
        for c in input do
            Assert.AreEqual(substringCount input (string c), substringCount result (string c))