module Parser

open FParsec
open Common


let ws = spaces
let str_ws s = pstring s .>> ws

let pidentifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws // skips trailing whitespace

let pstringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

// just yet another string literal.
let pregexLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '/')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "/") (pstring "/")
            (manyChars (normalChar <|> escapedChar))



let pterm =
    (pstringLiteral |>> (fun x -> Atom (String x)))
    <|>(pregexLiteral |>> (fun x -> Atom (String x)))
    <|>(pfloat |>> (fun x -> Atom (Float x)))
    <|>(pint32 |>> (fun x -> Atom (Number x)))
    <|> (attempt ((pstring "$") >>. (pipe2 pint32 ((str_ws ".") >>. pidentifier) (fun num fname ->  FieldAccess {target= Variable.SpecialVariable num; field=fname}))))
    <|> ((pstring "$") >>. (pint32 |>> (fun x ->  Atom (Variable (SpecialVariable x)))))

let pterm_ws = pterm .>> ws


let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
let pexpr = opp.ExpressionParser
opp.TermParser <- pterm_ws <|> between (str_ws "(") (str_ws ")") pexpr

opp.AddOperator(InfixOperator("==", ws, 1, Associativity.Left, fun x y -> BinOp (EqOp, x, y)))
opp.AddOperator(InfixOperator(">=", ws, 1, Associativity.Left, fun x y -> BinOp (GeOp, x, y)))
opp.AddOperator(InfixOperator(">", ws, 1, Associativity.Left, fun x y -> BinOp (GtOp, x, y)))
opp.AddOperator(InfixOperator("<", ws, 1, Associativity.Left, fun x y -> BinOp (LtOp, x, y)))
opp.AddOperator(InfixOperator("<=", ws, 1, Associativity.Left, fun x y -> BinOp (LeOp, x, y)))
opp.AddOperator(InfixOperator("~", ws, 1, Associativity.Left, fun x y -> BinOp (RegMatchOp, x, y)))

let pargs = 
     between (pstring "(") (pstring ")")
        (sepBy pexpr (str_ws ","))

let pfuncall =
    pipe2 pidentifier pargs (fun funid args -> {Identifier=funid; Args = args})

let pPipeline evaluator =
    (sepBy (pfuncall .>> ws) (str_ws "|"))  |>> evaluator
