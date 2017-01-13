namespace Tokamak
module Core = 
    open FParsec
    open System
    
    /// write , logging
    let write s = 
        System.Diagnostics.Debug.WriteLine(s)



    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///                                                                                                  ///
    ///                                         PARSER STATE                                             ///
    ///                                                                                                  ///
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    type ReactorState = | Default | IfBlock | ElifBlock | ElseBlock | WhileBlock | FunctionBlock | ForBlock | ControlBlock

    type UserState =
        { StateStack: ReactorState list } with static member Default = { StateStack = [] }


    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///                                                                                                  ///
    ///                                             KEYWORDS                                             ///
    ///                                                                                                  ///
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    let NIL = "nil"
    let DO = "do"
    let THEN = "then"
    let WHILE = "while"
    let IF  = "if"
    let ELIF = "elif"
    let ELSE = "else"
    let TRUE = "true"
    let FALSE = "false"
    let FUNCTION = "function"
    let END = "end"
    let AND = "and"
    let OR = "or"
    let IN = "in"
    let FOR = "for"


    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///                                                                                                  ///
    ///                                               AST                                                ///
    ///                                                                                                  ///
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    type DataType =
        | Integer of int64
        | Floating of double
        | Literal of string
        | Bool of bool
        | Array of Expression list
        

    and IdentifierName = IdentifierName of string
    

    and IdentifierNameList = IdentifierNameList of IdentifierName list
  
    
    and Variable =
        | Var of IdentifierName
        | Indexed of PrefixExpression * Expression // e.g. someVar[i]
  
        
    and PrefixExpression =
        | Var of Variable
        | FuncCall of FunctionCall
        | Expression
   
        
    and FunctionCall = FunctionCall of FunctionName * Arguments
   
    
    and FunctionName = FunctionName of IdentifierName
   
    
    and Arguments = 
        | Empty
        | Names of ExpressionList
 
    
    and FunctionDeclaration = FunctionDeclaration of FunctionName * FunctionBody
  
    
    and FunctionBody = FunctionBody of Parameters * Block
   
      
    and BinaryOp = |Plus|Minus|Multiply|Divide|Power|Mod|Range|LT|LTE|GT|GTE|Equals|NotEquals|And|Or
    
    
    and UnaryOp = |Minus|Not|Hash
    
   
    and Parameters =
        | Empty
        | Names of IdentifierNameList
   
        
    and ExternalExpression = ExternalExpression of IdentifierName
    
    and Expression =
        | Nil
        | VariableExpression of IdentifierName
        | DT of DataType
        | Func of FunctionCall
        | BinaryExp of Expression * BinaryOp * Expression
        | UnaryExp of UnaryOp * Expression
        | ArrayItemExpression of Expression * Expression
        | ExtExp of ExternalExpression
    
 
        
    and ExpressionList = ExpressionList of Expression list
  
    
    and Statement =
        | VD of VariableDeclaration
        | FD of FunctionDeclaration
        | Exp of Expression
        | If of IfBlock
        | While of WhileStatement
        | For of ForStatement


    and StatementList = StatementList of Statement list


    and Block = Block of StatementList
   
    
    and VariableDeclaration = 
        | Scalar of IdentifierName * Expression
        | Array of IdentifierName * Expression * Expression // id[exp1] = exp2


    and IfStatement = IfStatement of Expression * Block


    and ElifClause = ElifClause of Expression * Block


    and ElseStatement = ElseStatement of Block


    and ElifStatement = ElifStatement of ElifClause list


    and WhileStatement = WhileStatement of Expression * Block // while exp1 do block1 end
  

    and ForStatement = ForStatement of Expression * Expression * Block  // for exp1 in exp2 do block1 end


    and IfBlock = IfBlock of IfStatement * (ElifStatement) * (ElseStatement option)
       


    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///                                                                                                  ///
    ///                                         PARSING RULES                                            ///
    ///                                                                                                  ///
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    let parseExp, parseExpImpl = createParserForwardedToRef()
    let parseBlock, parseBlockImpl = createParserForwardedToRef()

    let LookAndDo parser = 
        lookAhead (parser) >>. parser


    let psmsg str = pstring str |>> fun a -> a


    let ParseNotFollowedByChars str = 
            ((psmsg str) >>. followedBy (spaces1))

 
    let ParseWhile = (spaces >>. ParseNotFollowedByChars WHILE >>= 
                        (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.ControlBlock::stream.UserState.StateStack }

                                            stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.WhileBlock::stream.UserState.StateStack }
                                            Reply(a)
                        ) >>. spaces >>. parseExp .>> (ParseNotFollowedByChars DO) >>= 
                        (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                                            Reply(a)
                        ) .>> spaces) .>>. (parseBlock .>> spaces .>> (ParseNotFollowedByChars END)) >>=
                        (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                                            Reply(a)) |>> fun(a,b) -> WhileStatement (a,b)
    
    let ParseFor = (spaces >>. ParseNotFollowedByChars FOR >>= 
                        (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.ControlBlock::stream.UserState.StateStack }
                                          
                                            stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.ForBlock::stream.UserState.StateStack }
                                            Reply(a)
                        ) >>. (spaces >>. parseExp .>> spaces) .>>. ((ParseNotFollowedByChars IN) >>. spaces >>. parseExp) .>> (ParseNotFollowedByChars DO) >>= 
                        (fun t stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                                            Reply(t)
                        ) .>> spaces) .>>. (parseBlock .>> spaces .>> (ParseNotFollowedByChars END)) >>=
                        (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                                            Reply(a)) |>> fun(a,b) ->
                                                            match a with
                                                            | (a1,a2) -> ForStatement (a1,a2,b)




    let ParseElif = ((spaces >>. (ParseNotFollowedByChars ELIF) >>= 
                        (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.ElifBlock::stream.UserState.StateStack }
                                            Reply(a)
                        ) >>. spaces1 >>. parseExp .>> (ParseNotFollowedByChars THEN))  >>=
                        (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                                            Reply(a)
                        ) .>>. (parseBlock .>> spaces)) |>> fun(a,b) -> ElifClause (a,b)


    let ParseElifStatement = (many (ParseElif)) |>> fun a -> ElifStatement a


    let ParseElseStatement = spaces  >>=
                                (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                                            Reply(a)
                                ) >>. parseBlock .>> spaces .>>  (ParseNotFollowedByChars END) >>=
                                (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                                            Reply(a)
                                ) |>> fun a -> ElseStatement a


    let ParseIfStatement = (spaces >>. (ParseNotFollowedByChars IF) >>= 
                            (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.ControlBlock::stream.UserState.StateStack }

                                            stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.IfBlock::stream.UserState.StateStack }
                                            Reply(a)
                            ) >>. spaces1 >>. parseExp .>> spaces .>> (ParseNotFollowedByChars THEN)) >>=
                            (fun a stream ->
                                            stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                                            Reply(a)
                            ) .>>. (parseBlock .>> spaces) |>> fun(a,b) -> 
                    IfStatement (a,b)
    

    let ParseIfBlock = ParseIfStatement .>>. ParseElifStatement .>>.  ((ParseNotFollowedByChars ELSE) >>= 
                                (fun a stream ->
                                           stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.ElseBlock::stream.UserState.StateStack}
                                           Reply(a)
                                ) >>. (ParseElseStatement |>> fun a -> Some a) <|> (spaces >>.(ParseNotFollowedByChars END)  |>> fun _ -> None)) |>> fun ((a,b),c) -> IfBlock (a,b,c)
   

    let ParseEndOfBlock (q : string) =
        let pq = (spaces >>.(ParseNotFollowedByChars q) )
        let hasStackSize =
            userStateSatisfies (fun us -> if List.length us.StateStack > 0 then true else false)
        pq >>. hasStackSize 


    // unary expressions
    let ParseUnaryMinus = spaces >>. pstring "-" >>. spaces |>> fun a -> UnaryOp.Minus
    let ParseUnaryNot = spaces >>. pstring "~" >>. spaces |>> fun a -> UnaryOp.Not
    let ParseHash = spaces >>. pstring "#" >>. spaces |>> fun a -> UnaryOp.Hash
    let ParseUnary = ParseUnaryMinus <|> ParseUnaryNot <|> ParseHash

    // binary expressions
    let ParsePlus = spaces >>. pstring "+" >>. spaces |>> fun a -> BinaryOp.Plus
    let ParseMinus = spaces >>. pstring "-" >>. spaces |>> fun a -> BinaryOp.Minus
    let ParseMultiply = spaces >>. pstring "*" >>. spaces |>> fun a -> BinaryOp.Multiply
    let ParseDivide = spaces >>. pstring "/" >>. spaces |>> fun a -> BinaryOp.Divide
    let ParseMod = spaces >>. pstring "%" >>. spaces |>> fun a -> BinaryOp.Mod
    let ParsePower = spaces >>. pstring "^" >>. spaces |>> fun a -> BinaryOp.Power
    let ParseLT = spaces >>. attempt(pstring "<" .>> followedBy ( noneOf  ['='])) |>> fun a -> BinaryOp.LT
    let ParseLTE = spaces >>. pstring "<=" >>. spaces |>> fun a -> BinaryOp.LTE
    let ParseGT = spaces >>. attempt(pstring ">" .>> followedBy ( noneOf  ['='])) |>> fun a -> BinaryOp.GT
    let ParseGTE = spaces >>. pstring ">=" >>. spaces |>> fun a -> BinaryOp.GTE
    let ParseEqualsEquals = spaces >>. pstring "==" >>. spaces |>> fun a -> BinaryOp.Equals
    let ParseNotEquals = spaces >>. pstring "~=" >>. spaces |>> fun a -> BinaryOp.NotEquals
    let ParseRange = spaces >>. pstring "$" .>> spaces |>> fun a -> BinaryOp.Range
    let ParseAnd = spaces >>.(pstring AND .>> spaces1) .>> spaces |>> fun a -> BinaryOp.And
    let ParseOr = spaces >>. (pstring OR .>> spaces1) .>> spaces |>> fun a -> BinaryOp.Or
    let ParseBinary = ParsePlus <|> ParseMinus <|> ParseMultiply <|> ParseDivide <|> ParseMod <|> ParsePower <|> ParseLT <|> ParseLTE
                       <|> ParseGT <|> ParseGTE <|> ParseEqualsEquals <|> ParseNotEquals <|> ParseRange <|> ParseAnd <|> ParseOr
    
    let IsFollowedByBinary = spaces >>. (followedBy ParseBinary)
    let NotFollowedByBinary = spaces >>. ( notFollowedBy ParseBinary)
    


    let keyWordSet =
        System.Collections.Generic.HashSet<_>(
            [| END; TRUE; FALSE; AND; OR; NIL; FUNCTION; ELSE; ELIF; IF; THEN; WHILE; DO; FOR; IN|]
        )
    

    let ParseTrue =  pstring TRUE |>> fun _ -> DataType.Bool true
    let ParseFalse = pstring FALSE |>> fun _ -> DataType.Bool false
    let ParseBoolean = ParseTrue <|> ParseFalse

 
    let ParseX : Parser<string,UserState> = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)  >>= fun (a : string)  stream ->
                
                let IdentifierError s = failwith (s + " Fusion reaction has warped the confinement field."+ 
                                                    "Structural stress has reached 1200%. Uncontrolled reaction is inevitable.")
                
                let size = List.length stream.UserState.StateStack
                            
                if a = END then
                    if size = 0 then 
                        IdentifierError ("Illegal END token found.")
                    else
                        let head = List.head stream.UserState.StateStack
                        match head with 
                        | ReactorState.ControlBlock | ReactorState.FunctionBlock -> 
                            Reply(ReplyStatus.Error,ErrorMessageList (ErrorMessage.Unexpected "Keyword found in illegal place."))
                        | _ -> IdentifierError ("END token found outside control or function block.")
                else if a = THEN then
                    if size = 0 then
                        IdentifierError ("Illegal THEN token found.")
                    else 
                        let head = List.head stream.UserState.StateStack
                        match head with 
                        | ReactorState.ElifBlock | ReactorState.IfBlock -> 
                            Reply(ReplyStatus.Error,ErrorMessageList (ErrorMessage.Unexpected "Keyword found in illegal place."))
                        | _ -> IdentifierError ("THEN token found outside elif or if block.")
                else if a = ELSE then
                    if size = 0 then
                        IdentifierError ("Illegal ELSE token found.")
                    else 
                        let head = List.head stream.UserState.StateStack
                        match head with 
                        |  ReactorState.ControlBlock ->
                            Reply(ReplyStatus.Error,ErrorMessageList (ErrorMessage.Unexpected "Keyword found in illegal place."))
                        | _ -> IdentifierError ("ELSE token found outside control block.")
                else if a = DO then
                    if size = 0 then
                        IdentifierError ("Illegal DO token found.")
                    else
                        let head = List.head stream.UserState.StateStack
                        match head with 
                        |  ReactorState.WhileBlock ->
                            Reply(ReplyStatus.Error,ErrorMessageList (ErrorMessage.Unexpected "Keyword found in illegal place."))
                        | _ -> IdentifierError ("DO token found outside while block.")
                else
                    if keyWordSet.Contains(a) then
                        Reply(ReplyStatus.Error,ErrorMessageList (ErrorMessage.Unexpected "Keyword found in illegal place."))
                    else
                        Reply(a)



    let ParseIdentifierName : Parser<IdentifierName, UserState> = ((ParseX .>> spaces )) |>> fun a -> IdentifierName a
    

    let ParseIdentifierNameList = sepBy1 (ParseIdentifierName) (spaces >>. pstring "," >>. spaces) |>> fun a -> IdentifierNameList a


    let ParseLiteral = pchar '"' >>. (manyChars (noneOf "\"")) .>> pchar '"' .>> spaces |>> fun a -> DataType.Literal a
    
    
    let ParseInteger = (LookAndDo(pint32 .>> notFollowedBy (pchar '.')) .>> spaces |>> fun a -> 
                                                write ("parsed INTEGER" + string(a))
                                                DataType.Integer (int64(a)))
    
    
    let ParseFloating = pfloat .>> spaces |>> fun a -> 
                                                write ("parsed FLOATING "  + string(a))
                                                DataType.Floating (double(a))

    
    let ParseArrayElements : Parser<DataType, UserState> = spaces >>. between (pstring "[" .>> spaces) (spaces >>. pstring "]")  
                                                                        ((many (attempt(parseExp .>> spaces .>> pstring ","))) 
                                                                            .>>. (spaces >>. parseExp)) |>> fun (a,b) -> DataType.Array (a@[b])


    let ParseDataType : Parser<DataType, UserState> = (ParseInteger <|> ParseFloating <|> ParseLiteral <|> ParseBoolean <|> ParseArrayElements) .>> spaces


    let ParseNil : Parser<Expression, UserState> = (ParseNotFollowedByChars NIL) |>> fun _ -> 
                                                                Expression.Nil
    
    let ParseEmptyParameters : Parser<Parameters, UserState> =  (pstring ")") |>> fun a -> Parameters.Empty


    let ParseNamedParameters : Parser<Parameters, UserState> = (ParseIdentifierNameList .>> spaces .>> pstring ")") |>> fun a -> Parameters.Names a
   
   
    let ParseParameters : Parser<Parameters, UserState> = pstring "(" >>. spaces >>.(ParseEmptyParameters <|> ParseNamedParameters) .>> spaces
    

    let ParseFunctionName : Parser<FunctionName, UserState> = attempt (ParseIdentifierName .>> lookAhead (spaces .>> pstring "(")) |>> fun a -> FunctionName a



    let ParseFunctionBody : Parser<FunctionBody, UserState> = 
            (ParseParameters .>> spaces) .>>. (parseBlock .>> pstring END >>= 
                (fun block stream ->
                    stream.UserState <- {stream.UserState with UserState.StateStack = List.tail stream.UserState.StateStack }
                    Reply(block)             
            ) .>> spaces1  ) |>> fun (a,b) -> FunctionBody (a,b)
                                                 
                                                      
    let ParseFunctionDeclaration  : Parser<FunctionDeclaration, UserState> = (pstring FUNCTION >>. spaces1) 
                                                                                >>=( fun a stream -> (
                                                                                                        stream.UserState <- {stream.UserState with UserState.StateStack = ReactorState.FunctionBlock::stream.UserState.StateStack }
                                                                                                        Reply(a)
                                                                                )) >>. (ParseFunctionName .>> spaces ) .>>. (spaces >>. ParseFunctionBody .>> spaces) |>> fun (a,b) -> 
                                                                                                                                    FunctionDeclaration (a,b)
    
       
    let ParseEquals  : Parser<string, UserState> = attempt(pstring "=" .>> (notFollowedBy (pstring "="))) 
    
       
   
    let ParseArrayVariableDeclaration = lookAhead(ParseIdentifierName .>> ((spaces >>. between (pstring "[" >>. spaces) (pstring "]" >>. spaces) (spaces >>. parseExp .>> spaces)) 
                                                                        .>>. (spaces >>. ParseEquals >>. spaces >>. parseExp)))
                                                         >>. (ParseIdentifierName)
                                                        .>>. (spaces >>. between (pstring "[" >>. spaces) (pstring "]" >>. spaces) (spaces >>. parseExp .>> spaces)) 
                                                        .>>. (spaces >>. ParseEquals >>. spaces >>. parseExp) |>> fun ((a,b),c) -> VariableDeclaration.Array (a,b,c)


    let ParseVariableDeclaration  : Parser<VariableDeclaration, UserState> = (lookAhead((ParseIdentifierName .>> spaces .>> ParseEquals)) >>. 
                                                                                            (ParseIdentifierName .>> spaces .>> ParseEquals) .>>.(spaces >>. parseExp) 
                                                                                                |>> fun (a,b) -> VariableDeclaration.Scalar (a,b)) <|>
                                                                                                ParseArrayVariableDeclaration
   
      
    let ParseExpressionList  : Parser<ExpressionList, UserState> = many1 (spaces >>. parseExp) |>> fun a -> ExpressionList a
    

    let ParseVDStatement : Parser<Statement, UserState> = (ParseVariableDeclaration) |>> fun a -> Statement.VD a
        
    
    let ParseFDStatement : Parser<Statement, UserState> = (ParseFunctionDeclaration) |>> fun a -> Statement.FD a


    let ParseExpStatement : Parser<Statement, UserState> = parseExp |>> fun a -> Statement.Exp a


    let ParseStatement : Parser<Statement, UserState> = (ParseVDStatement <|> ParseFDStatement <|> ParseExpStatement <|> (ParseIfBlock |>> fun a -> Statement.If a) 
                                                        <|> (ParseWhile |>> fun a -> Statement.While a) <|> (ParseFor |>> fun a -> Statement.For a)) 
   

    let ParseStatementList : Parser<StatementList, UserState> = many1 (ParseStatement .>> spaces) |>> fun a -> StatementList a


    let ParseBlock : Parser<Block, UserState> = (ParseStatementList |>> fun a -> Block a)


    let ParseEmptyArguments =  (pstring ")") |>> fun a -> Arguments.Empty


    let ParseNamedArguments = ((many (attempt(parseExp .>> spaces .>> pstring ","))) .>>. (spaces >>. parseExp) .>> spaces .>> pstring ")") |>> fun (a,b) -> Arguments.Names (ExpressionList (a@[b]))
   
   
    let ParseArgumentList = pstring "(" >>. spaces >>.(ParseEmptyArguments <|> ParseNamedArguments) .>> spaces

    
    let ParseFunctionCall = ParseFunctionName .>>. (ParseArgumentList) |>> fun (a,b) -> FunctionCall (a,b)
    
    
    let ParseFunctionCallExpression = (ParseFunctionCall) |>> fun a -> Func a

    
    let ParseSubExpression = between (spaces >>. pstring "(") (pstring ")" .>> spaces) (spaces >>. parseExp .>> spaces)

    
    let ParseDataTypeExpression = (ParseDataType |>> fun a -> Expression.DT a)
    

    let ParseDataTypeExpFollowedByBinary = (ParseDataType .>> (spaces .>> IsFollowedByBinary) |>> fun a -> Expression.DT a)
   

    let ParseVariableExpression = (attempt(ParseIdentifierName .>> (notFollowedBy (spaces >>. anyOf "[,("))) |>> fun a -> Expression.VariableExpression a)
    
                                                                                                               
    let ParseUnaryExpression = (ParseUnary .>>. parseExp |>> fun (a,b) -> Expression.UnaryExp (a,b)) 
    

    let ParseArrayItemExpression = attempt (((ParseIdentifierName .>> (followedBy (spaces >>. anyOf "[")) |>> fun s -> Expression.VariableExpression s) <|> ParseSubExpression <|> ParseFunctionCallExpression) .>>. (spaces >>. between (pstring "[" >>. spaces) (pstring "]" >>. spaces) (spaces >>. parseExp .>> spaces))) |>> fun (a,b) -> Expression.ArrayItemExpression (a,b)
                                          

    let ParseBinaryExpression1 = (ParseDataTypeExpression <|> ParseNil <|> ParseSubExpression <|> ParseVariableExpression <|> ParseFunctionCallExpression <|> ParseArrayItemExpression) 
    

    let ParseBinaryExpression = ParseBinaryExpression1 .>>. (spaces >>. ParseBinary .>> spaces) .>>. parseExp |>> fun ((a,b),c) -> Expression.BinaryExp (a,b,c)
   
    let ParseExternalExpression = between (pstring "{" >>. spaces) (spaces >>. pstring "}") (ParseIdentifierName) |>> fun a -> ExternalExpression a
    let ParseExtExp = ParseExternalExpression |>> fun a -> Expression.ExtExp a


    let ParseExpression  : Parser<Expression, UserState> = 
                              (((ParseNil |>> fun _ -> Expression.Nil)) <|> 
                                ((ParseVariableExpression.>>? NotFollowedByBinary)) <|>
                                ((ParseFunctionCallExpression.>>? NotFollowedByBinary)) <|>
                                ((ParseDataTypeExpression.>>? NotFollowedByBinary))<|>
                                ((ParseUnaryExpression.>>? NotFollowedByBinary))<|>
                                ((ParseSubExpression.>>? NotFollowedByBinary)) <|>
                                ((ParseArrayItemExpression .>>? NotFollowedByBinary)) <|>
                                ((ParseExtExp)) <|>
                                ((ParseBinaryExpression))) .>> notFollowedBy (spaces >>. anyOf "=[(" .>> spaces)
                                |>> fun a -> a
    



    parseExpImpl :=  (spaces >>. ParseExpression)
    parseBlockImpl := spaces >>. ParseBlock
    
    

    


    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///                                                                                                  ///
    ///                            COMPILE, RUN SCRIPTS, REACTION                                        ///
    ///                                                                                                  ///
    ////////////////////////////////////////////////////////////////////////////////////////////////////////


    open System
    open System.Collections.Generic


    type ExternalVariable(name : string, value : DataType) =
        member this.name : string = name
        member this.value : Expression = Expression.DT value

    type internal Context(variables : Dictionary<string, Expression>, functions : Dictionary<string, FunctionBody>) =
        let mutable parent : Context option = None
        member internal this.variables = variables
        member internal this.functions = functions

        member internal this.SetParent(cxt) = parent <- Some cxt
     
    
        member internal this.UpdateVariable(varname, value : Expression) =
            this.variables.[varname] <- value
            match parent with
            | None -> ()
            | Some p -> 
                if p.variables.ContainsKey(varname) then
                    p.UpdateVariable(varname, value)
              

    let internal CreateContext (context:Context) =
            let newcxt = Context(new Dictionary<string,Expression>(), new Dictionary<string,FunctionBody>())
            for kvp in context.variables do
                newcxt.variables.Add(kvp.Key, kvp.Value)
            for kvp in context.functions do
                newcxt.functions.Add(kvp.Key, kvp.Value)
            newcxt.SetParent (context)
            newcxt

    let internal toMap (dictionary : Dictionary<string,_>) = 
            (dictionary :> seq<_>)
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq
         

    type PlasmaConfinementUnit<'T> =
        abstract member Extract: unit -> 'T

    type IntegerConfinementUnit(exp : Expression) = 

        let k = match exp with
                        | Expression.DT dt ->
                            match dt with
                            | DataType.Integer i ->
                                i
                            | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."
                        | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."

        interface PlasmaConfinementUnit<int64> with
            member this.Extract() = k

        member this.R =
            (this :> PlasmaConfinementUnit<int64>).Extract()

     
    type FloatConfinementUnit(exp : Expression) = 

        let k = match exp with
                        | Expression.DT dt ->
                            match dt with
                            | DataType.Floating f ->
                                f
                            | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."
                        | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."

        interface PlasmaConfinementUnit<double> with
            member this.Extract() = k
          
        member this.R =
            (this :> PlasmaConfinementUnit<double>).Extract()

    type LiteralConfinementUnit(exp : Expression) = 

        let k = match exp with
                        | Expression.DT dt ->
                            match dt with
                            | DataType.Literal l ->
                                l
                            | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."
                        | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."

        interface PlasmaConfinementUnit<string> with
            member this.Extract() = k

        member this.R =
            (this :> PlasmaConfinementUnit<string>).Extract()

    type BoolConfinementUnit(exp : Expression) = 

        let k = match exp with
                        | Expression.DT dt ->
                            match dt with
                            | DataType.Bool b ->
                                b
                            | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."
                        | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."

        interface PlasmaConfinementUnit<bool> with
            member this.Extract() = k

        member this.R =
            (this :> PlasmaConfinementUnit<bool>).Extract()

    type ArrayConfinementUnit(exp : Expression) = 

        let k = match exp with
                        | Expression.DT dt ->
                            match dt with
                            | DataType.Array arr ->
                                arr
                            | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."
                        | _ -> failwith "Illegal operation - wrong type of Plasma Confinement Unit chosen. Cascading plasma leak..."

        interface PlasmaConfinementUnit<Expression list> with
            member this.Extract() = k

        member this.R =
            (this :> PlasmaConfinementUnit<Expression list>).Extract()


    
    type ReactorCore(block : Block) =

        member this.block = block


    type Compiler() =

        let mutable externalCalls = Dictionary<string, System.Action>()

        

        let rec BlockMap (cxt : Context) item : Expression =
            match item with
            | Block block ->
                match block with 
                | StatementList l ->
                    let expl = List.map (StatementMap cxt) l
                    List.reduce (fun _ i -> i) expl
        
        and StatementMap cxt item =
            match item with
            | Statement.VD a ->
                match a with
                | VariableDeclaration.Array (id,idxExp, exp) ->
                    match (ExpMap cxt idxExp) with
                    | Expression.DT dt ->
                        match dt with
                        | DataType.Integer i ->
                            match id with
                            | IdentifierName name ->
                                let idexp = cxt.variables.[name]
                                match (ExpMap cxt idexp) with
                                | Expression.DT dt' ->
                                    match dt' with 
                                    | DataType.Array a ->
                                        let newlist = [| for j = 0 to ((List.length a)- 1) do
                                                            yield if j = int(i) then (ExpMap cxt exp) else a.[j]|]
                                        cxt.UpdateVariable(name, Expression.DT (DataType.Array (List.ofArray newlist)))
                                        Expression.Nil
                                    | _ -> failwith "Unable to set array item at the given index becaus ethe datatype is not an array. Fusion reaction failed."
                                | _ -> failwith "Unable to look up array. The given expresison does not evaluate to an array datatype. Fusion reaction failed."
                        | _ -> failwith "Unable to get item of array at the given index, this index expression is not an integer datatype. Fusion reaction failed."
                    | _ -> failwith "Unable to get item of array at the given index, this index expression cannot be evaluated to any datatype. Fusion reaction failed."
                | VariableDeclaration.Scalar (id,exp) -> 
                    match id with
                    | IdentifierName i ->
                        let resexp = ExpMap cxt exp
                        cxt.UpdateVariable(i,resexp)
                        resexp
            | Statement.FD a ->
                FunctionDeclarationMap cxt a
            | Statement.Exp a ->
                ExpMap cxt a
            | Statement.If a ->
                IfBlockMap cxt a
            | Statement.While a ->
                WhileStatementMap cxt a
            | Statement.For a ->
                match a with
                | ForStatement (i,j,k) ->
                    let itExp = ExpMap cxt i
                    match itExp with
                    | Expression.VariableExpression (IdentifierName var) ->
                        let rangeExp = ExpMap cxt j
                        match rangeExp with
                        | Expression.DT data ->
                            match data with
                            | DataType.Array arr ->
                                let mutable ret = Expression.Nil
                                for i in arr do
                                    let subcxt = CreateContext cxt
                                    subcxt.UpdateVariable(var, i)
                                    ret <- BlockMap subcxt k
                                ret
                            | _ -> failwith "Could not execute for-loop. Cannot iterate over non-array type. Fusion reaction failed."
                        | _ -> failwith "Could not execute for-loop. Cannot iterate over unevaluated expression. Fusion reaction failed."
                    | _ -> failwith "Could not execute for-loop. The iterating variable is not recognized. Fusion reaction failed." 
                  
               


        and WhileStatementMap cxt item : Expression=
            match item with
            | WhileStatement (a,b) ->
        
                let mutable ret = Expression.Nil // ExpMap cxt a
                let mutable whileLoop = true
                let mutable blockRet = Expression.Nil
                while whileLoop do
                    ret <- ExpMap cxt a
           
                    match ret with
                    | Expression.DT (DataType.Bool true) ->
                        blockRet <- BlockMap cxt b
                    | _ ->  
                        whileLoop <- false
                blockRet

          

        and FunctionDeclarationMap cxt item : Expression =
            match item with
            | FunctionDeclaration (a,b) ->
                match a with
                | FunctionName id ->
                    match id with
                    | IdentifierName i ->
                        cxt.functions.Add(i,b)
                        Expression.VariableExpression id

        and IfBlockMap cxt item : Expression =
            match item with 
            | IfBlock (a,b,c) ->
                let ifres = IfStatementMap cxt a
                match ifres with
                | (a,true) -> a
                | (a, false) ->
                    let elifres = ElifStatementMap cxt b
                    match elifres with
                    | (a,true) -> a
                    | (a, false) -> 
                        if c <> None then
                            ElseStatementMap cxt c
                        else
                            Expression.Nil


        and ElseStatementMap cxt item : Expression =
            match item with
            | Some (ElseStatement a) ->
               BlockMap cxt a
            | _ -> Expression.Nil

      
        and ElifStatementMap cxt item : Expression * bool =
            match item with
            | ElifStatement b ->
                if List.length b = 0 then (Expression.Nil, false)
                else
                    let elifMap = List.map (fun i -> ElifClauseMap cxt i) b
                    List.reduce (fun acc c -> 
                                    match acc with
                                    | (_,true) -> acc
                                    | (_,false) -> c
                                ) elifMap
            


        and ElifClauseMap cxt item : (Expression * bool) =
            match item with
            | ElifClause (a,b) ->
                let ret = ExpMap cxt a
                match ret with
                | Expression.DT (DataType.Bool true) ->
                    ((BlockMap cxt b), true)
                | _ ->  (Expression.Nil, false)

        and IfStatementMap cxt item : Expression * bool =
            match item with 
            | IfStatement (a,b) ->
                let ret = ExpMap cxt a
                match ret with
                | Expression.DT (DataType.Bool true) ->
                    ((BlockMap cxt b),true)
                | _ ->  (Expression.Nil, false)
        
        
           
        and CallFunctionWithArgs (cxt:Context) funcBody args =
            match funcBody with
            | FunctionBody (a,b) ->
                match a with 
                | Parameters.Names idlist ->       
                    match idlist with
                    | IdentifierNameList g ->
                        match args with 
                        | ExpressionList arglist ->
                            for ii = 0 to ((List.length arglist)-1) do
                                let evalExp = ExpMap cxt arglist.[ii]
                                match g.[ii] with
                                | IdentifierName y ->
                                    cxt.variables.[y] <- evalExp
                                    cxt.UpdateVariable(y,evalExp)
                    let mutable subContext = CreateContext cxt 
                    let explist = BlockMap subContext b
                    explist
                | Parameters.Empty ->
                    let mutable subContext = CreateContext cxt 

                    let explist = BlockMap subContext b
                    explist


        and CallFunctionWithoutArgs (cxt:Context) funcBody =
            match funcBody with
            | FunctionBody (a,b) ->
                    let mutable subContext = CreateContext cxt
                    let explist = BlockMap subContext b
                    explist


        and FunctionBodyMap cxt item =
            match item with
            | FunctionBody (a,b) ->
                BlockMap cxt b

      
        and ExpMap cxt item  =
            match item with
            | Expression.ExtExp externalexp ->
                match externalexp with
                | ExternalExpression (IdentifierName idname) ->
                    if externalCalls.ContainsKey(idname) = false then
                        failwith ("Could not find external expression to invoke for name: "+idname+". Fusion reaction cannot proceed.")
                    else externalCalls.[idname].Invoke()
                Expression.Nil
            | Expression.Nil -> 
                Expression.Nil
            | Expression.DT  dt ->
                DataTypeMap cxt dt  
                item
            | Expression.UnaryExp (u,e) ->
                EvaluateUnaryExpression cxt e u
            | Expression.BinaryExp (e1,b,e2) ->
                EvaluateBinaryExpression cxt e1 b e2
            | Expression.VariableExpression e ->
                match e with 
                | IdentifierName varname ->
                    if cxt.variables.ContainsKey(varname) then
                        let a = cxt.variables.[varname]
                        ExpMap cxt a
                    else
                        item
            | Expression.Func a ->
                match a with 
                | FunctionCall (i,j) ->
                    match i with
                    | FunctionName fname ->
                            match fname with
                            | IdentifierName idname ->
                                let f = cxt.functions.[idname]
                        
                                match j with
                                | Arguments.Empty ->
                                    let nextCxt = CreateContext cxt
                                    let er = CallFunctionWithoutArgs nextCxt f
                                    er
                                | Arguments.Names args ->
                                    let nextCxt = CreateContext cxt
                                    CallFunctionWithArgs nextCxt f args
            | Expression.ArrayItemExpression (a,b) ->
                let arrayIdentifier = (ExpMap cxt a)
                let indexExp = (ExpMap cxt b)
                match arrayIdentifier with 
                    | Expression.VariableExpression (IdentifierName id) ->
                        if cxt.variables.ContainsKey(id) then
                            let v = (cxt.variables.[id])
                            let evaluated = ExpMap cxt v
                            match evaluated with
                            | Expression.DT ( dt) ->
                                match dt with
                                | DataType.Array arr ->
                                    match indexExp with
                                    | Expression.DT (indexdt) ->
                                        match indexdt with
                                        | Integer i ->
                                            arr.[int(i)]
                                        | _ -> Expression.Nil
                                    | _ -> Expression.Nil
                                | _ -> Expression.Nil
                            | _ -> Expression.Nil
                        else Expression.Nil
                    | Expression.DT ( somedt) -> 
                        match somedt with
                        | DataType.Integer intdt ->
                              Expression.Nil
                        | DataType.Array arrexp ->
                              match indexExp with
                              | Expression.DT (indexdt) ->
                                        match indexdt with
                                        | Integer i ->
                                            arrexp.[int(i)]
                                        | _ -> Expression.Nil
                              | _ -> Expression.Nil
                        | _ -> Expression.Nil
                    | _ -> Expression.Nil
        


        and ExchangeVariableNamesForExpressions expr (cxt : Context) : Expression =
            match expr with
            | Expression.VariableExpression ve ->
                match ve with 
                | IdentifierName idname ->
                    if cxt.variables.ContainsKey(idname) then
                        let rexpr : Expression = cxt.variables.[idname]
                        rexpr
                    else expr
            | Expression.BinaryExp (e1,bin,e2) ->
                let e1_ = ExchangeVariableNamesForExpressions e1 cxt
                let e2_ = ExchangeVariableNamesForExpressions e2 cxt
                EvaluateBinaryExpression cxt e1_ bin e2_
            | Expression.UnaryExp (unary,e1) ->
                let e1_ = ExchangeVariableNamesForExpressions e1 cxt
                EvaluateUnaryExpression cxt e1_ unary
            | _ -> expr
        
     
        and UnaryExpMap cxt unaryOp expr = 
                            ExpMap cxt expr
    
        and BinaryExpError s = failwith(s+" Core breach! Critical failure imminent!")

        and DataTypeMap cxt item =
            match item with
            | DataType.Floating f ->
                write ("floating "+string(f))
            | DataType.Integer i ->
                write ("integer "+string(i))
            | DataType.Literal l ->
                write ("literal "+l)
            | DataType.Bool b ->
                write ("bool "+string(b))
            | DataType.Array a ->
                write ("array "+string(a)+",  len: "+string(List.length a))
    
    
        and HandlePlusWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Integer (i1 + i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Floating (f1 + f2)
            | (DataType.Floating f), (DataType.Integer i) | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Floating (double(i) + f)
            | (DataType.Literal l1), (DataType.Literal l2) -> 
                DataType.Literal (l1 + l2)
            | (DataType.Bool b), _ ->
                BinaryExpError "Invalid operation."
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandlePlusWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Addition is not applicable with the given datatypes."
    

        and HandleMinusWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Integer (i1 - i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Floating (f1 - f2)
            | (DataType.Floating f), (DataType.Integer i) ->
                DataType.Floating (f - double(i))
            | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Floating (double(i) - f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                   DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleMinusWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Subtraction is not applicable with the given datatypes."
    
    
        and HandleDivideWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Integer (i1 / i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Floating (f1 / f2)
            | (DataType.Floating f), (DataType.Integer i) ->
                DataType.Floating (f / double(i))
            | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Floating (double(i) / f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleDivideWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Division is not applicable with the given datatypes."


        and HandlePowerWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Integer (int64(Math.Pow(float(i1),float(i2))))
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Floating (Math.Pow(f1,f2))
            | (DataType.Floating f), (DataType.Integer i) ->
                DataType.Floating (Math.Pow(f,float(i)))
            | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Floating (Math.Pow(float(i),f))
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandlePowerWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Exponentiation is not applicable with the given datatypes."
    
    
        and HandleMultiplyWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Integer (i1 * i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Floating (f1 * f2)
            | (DataType.Floating f), (DataType.Integer i) | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Floating (double(i) * f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleMultiplyWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Multiplication is not applicable with the given datatypes."


        and HandleModWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Integer (i1 % i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Floating (f1 % f2)
            | (DataType.Floating f), (DataType.Integer i) | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Floating (double(i) % f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleModWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Mod operation is not applicable with the given datatypes."


        and HandleGTWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Bool (i1 > i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Bool (f1 > f2)
            | (DataType.Floating f), (DataType.Integer i) -> 
                DataType.Bool (f > double(i))
            | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Bool (double(i) > f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                   DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleGTWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. > is not applicable with the given datatypes. Wrong data types."


        and HandleGTEWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Bool (i1 >= i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Bool (f1 >= f2)
            | (DataType.Floating f), (DataType.Integer i) | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Bool (double(i) >= f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleGTEWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. >= is not applicable with the given datatypes."


        and HandleLTWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Bool (i1 < i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Bool (f1 < f2)
            | (DataType.Floating f), (DataType.Integer i) ->
                DataType.Bool (f < double(i))
            | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Bool (double(i) < f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleLTWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. < is not applicable with the given datatypes."


        and HandleLTEWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Bool (i1 <= i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Bool (f1 <= f2)
            | (DataType.Floating f), (DataType.Integer i) | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Bool (double(i) <= f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleLTEWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. <= is not applicable with the given datatypes."


        and HandleEqualsWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Bool (i1 = i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Bool (f1 = f2)
            | (DataType.Floating f), (DataType.Integer i) ->
                DataType.Bool (f = double(i))
            | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Bool (double(i) = f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleEqualsWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Equality test is not applicable with the given datatypes."


        and HandleNotEqualsWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                DataType.Bool (i1 <> i2)
            | (DataType.Floating f1), (DataType.Floating f2) ->
                DataType.Bool (f1 <> f2)
            | (DataType.Floating f), (DataType.Integer i) | (DataType.Integer i), (DataType.Floating f) ->
                DataType.Bool (double(i) <> f)
            | (DataType.Array a1, DataType.Array a2) ->
                if List.length a1 <> (List.length a2) then BinaryExpError "Array length mismatch."
                else
                    DataType.Array (List.map2 (fun a b -> 
                                                    let a_ = ExpMap cxt a
                                                    let b_ = ExpMap cxt b
                                                    match (a_, b_) with
                                                    | ((Expression.DT aa), Expression.DT bb) ->
                                                        Expression.DT (HandleNotEqualsWithDataTypes(cxt,aa,bb))
                                                    | _ -> BinaryExpError "Array ops fail.") a1 a2 )
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. ~= is not applicable with the given datatypes."



        and HandleRangeWithDataTypes (dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Integer i1),( DataType.Integer i2) ->
                let ls = [i1 .. i2]
                let ils = List.map (fun a -> Expression.DT(Integer a)) ls
                DataType.Array ils
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Range operation is not applicable with the given datatypes."


        and HandleAndWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Bool b1),( DataType.Bool b2) ->
                DataType.Bool (b1 && b2)
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Logical AND is not applicable with the given datatypes."
   
        
        and HandleOrWithDataTypes (cxt, dt1 , dt2) : DataType =
            match(dt1,dt2) with
            | (DataType.Bool b1),( DataType.Bool b2) ->
                DataType.Bool (b1 || b2)
            | _ -> BinaryExpError "Binary operation fail. Wrong data types. Logical OR is not applicable with the given datatypes."


        and HandleNotWithDataTypes (cxt, dt) : DataType =
            match dt with
            | DataType.Bool b -> DataType.Bool (not b)
            | _ -> BinaryExpError "Logical negation needs boolean datatype. Logical NOT is not applicable with the given datatypes."
    

        and HandleNegativeWithDataTypes (cxt, dt) : DataType =
            match dt with 
            | DataType.Integer i -> DataType.Integer (-i)
            | DataType.Floating f -> DataType.Floating (-f)
            | _ -> BinaryExpError "Negation operation needs numeric datatpe. Negation is not applicable with the given datatypes."


        and EvaluateUnaryExpression cxt e unary =
            let e_ = ExpMap cxt (ExchangeVariableNamesForExpressions e cxt)
            match e_ with
            | (Expression.DT dt) ->
                let er = match unary with
                            | UnaryOp.Not -> Expression.DT(HandleNotWithDataTypes(cxt, dt))
                            | UnaryOp.Minus -> Expression.DT(HandleNegativeWithDataTypes(cxt, dt))
                            | _ -> BinaryExpError "Operaation could not be performed."
                er
            | _ -> Expression.UnaryExp (unary,e_)


        and EvaluateBinaryExpression cxt e1 bin e2 : Expression =
            let e1_ = ExpMap cxt (ExchangeVariableNamesForExpressions e1 cxt)
            let e2_ = ExpMap cxt (ExchangeVariableNamesForExpressions e2 cxt)
            match (e1_,e2_) with
            | (Expression.DT dt1) , (Expression.DT dt2) ->
                let er = match bin with
                            | BinaryOp.Plus -> Expression.DT (HandlePlusWithDataTypes(cxt, dt1,dt2))
                            | BinaryOp.Multiply -> Expression.DT (HandleMultiplyWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.Divide -> Expression.DT (HandleDivideWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.Minus -> Expression.DT (HandleMinusWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.Power -> Expression.DT (HandlePowerWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.Mod -> Expression.DT (HandleModWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.And -> Expression.DT (HandleAndWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.Or -> Expression.DT (HandleOrWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.GT -> Expression.DT (HandleGTWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.GTE -> Expression.DT (HandleGTEWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.LT -> Expression.DT (HandleLTWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.LTE -> Expression.DT (HandleLTEWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.Equals -> Expression.DT (HandleEqualsWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.NotEquals -> Expression.DT (HandleNotEqualsWithDataTypes(cxt,dt1,dt2))
                            | BinaryOp.Range -> Expression.DT (HandleRangeWithDataTypes(dt1,dt2))
                PrintFinalExpression er
            | (Expression.VariableExpression ve), _ | (_, Expression.VariableExpression ve) ->
                let bexp = Expression.BinaryExp (e1_, bin, e2_)
                bexp
            | _ ->  Expression.BinaryExp(e1_, bin, e2_)


        and PrintFinalExpression exp =
            match exp with
            | Expression.DT dt->
                match dt with 
                | DataType.Bool b -> write ("boolean "+ string(b))
                | DataType.Integer i -> write ("integer "+ string(i))
                | DataType.Floating f -> write ("Floating "+ string(f))
                | DataType.Literal l -> write ("literal "+ string(l))
                | DataType.Array a -> write ("array "+ string(a))
            | _ -> write "other expression"
            exp

        
        member internal this.mainContext = Context(new Dictionary<string,Expression>(), new Dictionary<string,FunctionBody>())

        member this.Parse code =
            System.Diagnostics.Debug.WriteLine("BEGIN PARSING")
            match runParserOnString parseBlock UserState.Default "Tokamak reaction stream" code  with
            | Success(result,_,_) -> 
                System.Diagnostics.Debug.WriteLine("FINISH PARSING")
                result
            | Failure(msg,_,_) ->
                System.Diagnostics.Debug.WriteLine("Error  "+ msg+ "Reactor core containment failed!")
                failwith msg


        member this.compile (text : string) : Expression =
            let result = BlockMap this.mainContext (this.Parse text)
            PrintFinalExpression result
            

        member this.compileWithArgs(text : string, [<ParamArray>] arr : ExternalVariable array) : Expression =
            Array.iter (fun (item : ExternalVariable) -> this.mainContext.UpdateVariable(item.name, item.value)) arr
            let result = BlockMap this.mainContext (this.Parse text)
            PrintFinalExpression result

        member this.EjectCore text = new ReactorCore(this.Parse text)

        member this.IgniteCore (reactorCore : ReactorCore) =
            BlockMap this.mainContext reactorCore.block

        member this.EvaluateExpression(exp : Expression) =
            let block = Block (StatementList([Statement.Exp exp]))
            BlockMap this.mainContext block

         
        member this.AddExternalCall(idName, action) =
            externalCalls.[idName] <- action

      

