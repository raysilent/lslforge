module Language.Lsl.Render(renderCompiledScript,renderStatements,renderCtxStatement,renderStatement) where

import Data.List(foldl',intersperse)
import Language.Lsl.Syntax(Expr(..),Func(..),FuncDec(..),Global(..),Handler(..),State(..),Statement(..),
                  Ctx(..),Var(..),LSLType(..),Component(..),ctxItems,CompiledLSLScript(..),
                  SourceContext(..))
import Debug.Trace
tr s x = trace (s ++ show x) x
-- | Generate a string representing an LSL script from a timestamp (string) 
-- and a compiled (i.e. validated, with referenced modules included) LSL script.
renderCompiledScript :: String -> CompiledLSLScript -> String
renderCompiledScript stamp (CompiledLSLScript comment globals funcs states) =
   (renderString "// LSL script generated: " . renderString stamp . renderString "\n" .
    renderString comment .
    renderGlobals globals . renderFuncs funcs . renderStates states . renderString "\n") ""

renderSequence r = (foldl' (.) blank) . (map r)

renderGlobals = renderSequence renderGlobal

renderGlobal (GDecl (Ctx sc var) val) = renderPreText sc . renderVar var . 
    case val of 
        Nothing -> renderString ";"
        Just expr -> renderString " = " . renderSimple expr . renderString ";"

renderCtxSimple (Ctx _ expr) = renderSimple expr
renderSimple (Neg expr) = renderChar '-' . renderCtxExpr expr
renderSimple (ListExpr l) =
    renderChar '[' .
        (foldl' (.) id $ intersperse (renderChar ',') $ map renderCtxSimple l) .
        renderChar ']'
renderSimple (VecExpr x y z) = renderChar '<' . renderCtxSimple x .
                               renderChar ',' . renderCtxSimple y .
                               renderChar ',' . renderCtxSimple z .
                               renderChar '>'
renderSimple (RotExpr x y z s) = renderChar '<' . renderCtxSimple x .
                                 renderChar ',' . renderCtxSimple y .
                                 renderChar ',' . renderCtxSimple z .
                                 renderChar ',' . renderCtxSimple s .
                                 renderChar '>'
renderSimple e = renderExpression e

renderStates = renderSequence renderState

renderState (Ctx ssc (State (Ctx sc "default") handlers)) = 
    renderPreText ssc .
    renderString "default {\n" . renderHandlers handlers . renderString "}"
renderState (Ctx ssc (State (Ctx _ name) handlers)) =
    renderPreText ssc .
    renderString "state " . renderString name . renderString " {\n" . renderHandlers handlers . renderString "}"
 
renderHandlers = renderSequence renderHandler

renderHandler (Ctx _ (Handler (Ctx sc name) vars stmts)) = renderPreText1 (renderIndent 0) sc . renderHandler' name vars stmts

renderHandler' name vars stmts =
    renderString name . renderChar '(' . renderVarList (ctxItems vars) . renderString ") {\n" . 
        renderStatements 1 stmts . renderIndent 0 . renderString "}\n"
        
renderChar = showChar
renderVar (Var nm t) = (renderType t) . renderChar ' ' . (renderString nm)
renderFuncDec (FuncDec name t vars) = 
    let sp = if t == LLVoid then "" else " " in
        renderType t . renderString sp . renderString (ctxItem name) . renderChar '(' . 
        renderVarList (ctxItems vars) . renderChar ')'

renderVarList [] = blank
renderVarList (v:vars) = 
    (renderVar v) .
        let render' [] = blank
            render' (v:vars) = renderChar ',' . renderVar v . render' vars in render' vars

renderFuncs = renderSequence renderCtxFunc

renderCtxFunc (Ctx sc func) = renderPreText sc . renderFunc func
renderFunc (Func dec stmts) = 
    renderFuncDec dec . renderString "{\n" . renderStatements 0 stmts . renderString "}"

renderIndent 0 = renderString "    "
renderIndent n = renderString "    " . renderIndent (n - 1)

renderCtxStatement hang n (Ctx _ s) = renderStatement hang n s

renderStatements n = renderSequence (renderCtxStatement False n)
    
doHang True n = renderString " "
doHang False n = renderIndent n

renderOptionalExpression Nothing = blank
renderOptionalExpression (Just expr) = renderCtxExpr expr

renderStatement hang n stmt = doHang hang n . renderStatement' n stmt
renderStatement' n (Compound stmts) = 
        renderString "{\n" . renderStatements (n+1) stmts . renderIndent n . renderString "}\n"
renderStatement' n (While expr stmt) = 
    renderString "while (" . renderCtxExpr expr . renderChar ')' . 
    case stmt of
        (Ctx _ NullStmt) -> renderString ";\n"
        _ -> renderCtxStatement True n stmt
renderStatement' n (DoWhile stmt expr) = 
    renderString "do " . 
    (case stmt of
         (Ctx _ NullStmt) -> renderString ";\n"
         _ -> renderCtxStatement True n stmt) . doHang False n . renderString "while (" . renderCtxExpr expr . renderString ");\n"
renderStatement' n (For mexpr1 mexpr2 mexpr3 stmt) =
    renderString "for (" . renderCtxExprs "" mexpr1 . renderString "; " . renderOptionalExpression mexpr2 .
    renderString "; " . renderCtxExprs "" mexpr3 . renderString ")" . 
    case stmt of
        (Ctx _ NullStmt) -> renderString ";\n"
        _ -> renderCtxStatement True n stmt
renderStatement' n (If expr stmt1 stmt2) =
    renderString "if (" . renderCtxExpr expr . renderChar ')' . 
        (case stmt1 of
             (Ctx _ NullStmt) -> renderString ";\n"
             (Ctx _ (If expr (Ctx _ (Compound _)) _)) -> renderCtxStatement True n stmt1
             (Ctx _ (If expr _ (Ctx _ NullStmt))) -> case stmt2 of
                 (Ctx _ NullStmt) -> renderCtxStatement True n stmt1
                 _ -> renderStatement True n (Compound [stmt1])
             _ -> renderCtxStatement True n stmt1) .
        case stmt2 of 
            (Ctx _ NullStmt) -> blank
            _ -> renderIndent n . renderString "else " . (renderCtxStatement True n stmt2)
renderStatement' n (Decl var val) = 
    renderVar var . 
        case val of 
            Nothing -> renderString ";\n"
            Just expr -> renderString " = " . renderCtxExpr expr . renderString ";\n"
renderStatement' n (NullStmt) = blank . renderString "\n"
renderStatement' n (Return Nothing) = renderString "return;\n"
renderStatement' n (Return (Just expr)) = renderString "return " . renderCtxExpr expr . renderString ";\n";
renderStatement' n (StateChange name) = renderString "state " . renderString name . renderString ";\n";
renderStatement' n (Do expr) = renderCtxExpr expr . renderString ";\n";
renderStatement' n (Label s) = renderChar '@' . renderString s . renderString ";\n";
renderStatement' n (Jump s) = renderString "jump " . renderString s . renderString ";\n";

renderExpressions prefix [] = blank
renderExpressions prefix (e:es) = renderString prefix . renderExpression e . renderExpressions "," es

renderCtxName (Ctx _ n) = renderString n
renderCtxExpr (Ctx _ e) = renderExpression e

renderCtxExprs prefix [] = blank
renderCtxExprs prefix (e:es) = renderString prefix . renderCtxExpr e . renderCtxExprs "," es

renderExpression ex = case ex of
        (IntLit i) -> shows i
        (FloatLit f) -> shows f
        (StringLit s) -> renderString ('"':go s)
             where go [] = "\""
                   go ('\\':s) = '\\':'\\':go s
                   go ('\t':s) = '\\':'t':go s
                   go ('\n':s) = '\\':'n':go s
                   go ('"':s) = '\\':'"':go s
                   go (c:s) = c:go s
        (KeyLit k) -> shows k
        (VecExpr x y z) -> 
            renderChar '<' . renderCtxExpr x . renderChar ',' .
                             renderCtxExpr y . renderChar ',' .
                             renderCtxExpr z . renderChar '>'
        (RotExpr x y z s) ->
            renderChar '<' . renderCtxExpr x . renderChar ',' .
                             renderCtxExpr y . renderChar ',' .
                             renderCtxExpr z . renderChar ',' .
                             renderCtxExpr s . renderChar '>'
        (ListExpr l) ->
            let r prefix [] = blank
                r prefix (i:is) = renderString prefix . renderCtxExpr i . r "," is
            in renderChar '[' . r "" l . renderChar ']'
        (Add expr1 expr2) -> renderBinExpr "+" expr1 expr2 lo
        (Sub expr1 expr2) -> renderBinExpr "-" expr1 expr2 lo
        (Mul expr1 expr2) -> renderBinExpr "*" expr1 expr2 lo
        (Div expr1 expr2) -> renderBinExpr "/" expr1 expr2 lo
        (Mod expr1 expr2) -> renderBinExpr "%" expr1 expr2 lo
        (BAnd expr1 expr2) -> renderBinExpr "&" expr1 expr2 lo
        (Xor expr1 expr2) -> renderBinExpr "^" expr1 expr2 lo
        (BOr expr1 expr2) -> renderBinExpr "|" expr1 expr2 lo
        (Lt expr1 expr2) -> renderBinExpr "<" expr1 expr2 lo
        (Gt expr1 expr2) -> renderBinExpr ">" expr1 expr2 lo
        (Le expr1 expr2) -> renderBinExpr "<=" expr1 expr2 lo
        (Ge expr1 expr2) -> renderBinExpr ">=" expr1 expr2 lo
        (And expr1 expr2) -> renderBinExpr "&&" expr1 expr2 lo
        (Or expr1 expr2) -> renderBinExpr "||" expr1 expr2 lo
        (ShiftL expr1 expr2) -> renderBinExpr "<<" expr1 expr2 lo
        (ShiftR expr1 expr2) -> renderBinExpr ">>" expr1 expr2 lo
        (Inv expr) -> renderChar '~' . renderInParenIfLower expr lo
        (Not expr) -> renderChar '!' . renderInParenIfLower expr lo
        (Neg expr) -> renderChar '-' . renderInParenIfLower expr lo
        (Call name exprs) -> renderCtxName name . renderChar '(' . renderCtxExprs "" exprs . renderChar ')'
        (Cast t expr) -> renderChar '(' . renderType t . renderChar ')' .
                        renderInParenIfLower expr lo
        (Get var) -> renderVarAccess var
--renderExpression (Const var) = renderVarAccess var
--renderExpression (Set var expr) = renderChar '(' . renderVarAccess var . renderString " = " . renderCtxExpr expr . renderChar ')'
        (Set va expr) -> renderAssignment va "=" expr
        (IncBy va expr) -> renderAssignment va "+=" expr
        (DecBy va expr) -> renderAssignment va "-=" expr
        (MulBy va expr) -> renderAssignment va "*=" expr
        (DivBy va expr) -> renderAssignment va "/=" expr
        (ModBy va expr) -> renderAssignment va "%=" expr
        (Equal expr1 expr2) -> renderBinExpr "==" expr1 expr2 lo
        (NotEqual expr1 expr2) -> renderBinExpr "!=" expr1 expr2 lo
        (PostInc va) -> renderVarAccess va . renderString "++"
        (PostDec va) -> renderVarAccess va . renderString "--"
        (PreInc va) -> renderString "++" . renderVarAccess va
        (PreDec va) -> renderString "--" . renderVarAccess va
    where
        lo = \ t -> isLower ex t || needsBooleanParens ex t || castCast ex t

renderInParens f = renderChar '(' . f . renderChar ')'

renderBinExpr op expr1 expr2 f =
        renderInParenIfLower expr1 f . renderChar ' ' . renderString op . renderChar ' ' .
        renderInParenIfLower expr2 f
renderAssignment va op expr = 
    renderVarAccess va . renderChar ' ' . renderString op . renderChar ' ' . renderCtxExpr expr
renderComponent All = blank
renderComponent X = renderString ".x"
renderComponent Y = renderString ".y"
renderComponent Z = renderString ".z"
renderComponent S = renderString ".s"

renderVarAccess (v,c) = renderCtxName v . renderComponent c

renderString s s' = s ++ s'
renderType LLList = renderString "list"
renderType LLInteger = renderString "integer"
renderType LLVector = renderString "vector"
renderType LLFloat = renderString "float"
renderType LLString = renderString "string"
renderType LLRot = renderString "rotation"
renderType LLKey = renderString "key"
renderType LLVoid = blank

blank :: String -> String
blank = id

renderPreText :: (Maybe SourceContext) -> String -> String
renderPreText = maybe (renderString "\n") (renderString . srcPreText)

renderPreText1 :: (String -> String) -> (Maybe SourceContext) -> String -> String
renderPreText1 f = maybe (renderString "\n" . f) (renderString . srcPreText)

-- Wrap with parentheses if lower precedence

renderInParenIfLower :: (Ctx Expr) -> ((Ctx Expr) -> Bool) -> String -> String
renderInParenIfLower ce f =
        if f ce then renderChar '(' . renderCtxExpr ce . renderChar ')'
                else renderCtxExpr ce

needsBooleanParens :: Expr -> Ctx Expr -> Bool
needsBooleanParens ex0 (Ctx _ ex1) =
        case ex0 of
            (And _ _) -> case ex1 of
                                (Or _ _) -> True
                                _ -> False
            (Or _ _) -> case ex1 of
                                (And _ _) -> True
                                _ -> False
            _ -> False

castCast :: Expr -> Ctx Expr -> Bool
castCast ex0 (Ctx _ ex1) =
        case ex0 of
            (Cast _ _) -> case ex1 of
                               (Cast _ _) -> True
                               _ -> False
            _ -> False

-- Comparing Order of Precedence

isLower :: Expr -> (Ctx Expr) -> Bool
isLower e0 (Ctx _ e1) = prec e0 < prec e1

-- Smaller number is higher precedence

prec :: Expr -> Int
prec e = 
        case e of
            (IntLit _) -> 0
            (FloatLit _) -> 0
            (StringLit _) -> 0
            (ListExpr _) -> 0
            (VecExpr _ _ _) -> 0
            (RotExpr _ _ _ _) -> 0
            (KeyLit _) -> 0
            (Call _ _) -> 0
            (Cast _ _) -> 1
            (Not _) -> 2
            (Inv _) -> 2
            (Neg _) -> 2
            (PostInc _) -> 2
            (PostDec _) -> 2
            (PreInc _) -> 2
            (PreDec _) -> 2
            (Mul _ _) -> 3
            (Div _ _) -> 3
            (Mod _ _) -> 3
            (Add _ _) -> 4
            (Sub _ _) -> 4
            (ShiftL _ _) -> 5
            (ShiftR _ _) -> 5
            (Lt _ _) -> 6
            (Le _ _) -> 6
            (Gt _ _) -> 6
            (Ge _ _) -> 6
            (Equal _ _) -> 7
            (NotEqual _ _) -> 7
            (BAnd _ _) -> 8
            (Xor _ _) -> 9
            (BOr _ _) -> 10
            (And _ _) -> 11       -- usually And is higher than Or,
            (Or _ _) -> 11        -- this seems bug of LSL Compiler. see SVC-779
            (Get _) -> 0
            (Set _ _) -> 12
            (IncBy _ _) -> 12
            (DecBy _ _) -> 12
            (MulBy _ _) -> 12
            (DivBy _ _) -> 12
            (ModBy _ _) -> 12
            _ -> 0