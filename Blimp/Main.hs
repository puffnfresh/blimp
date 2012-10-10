module Blimp.Main (main) where

import Control.Monad.Exception
import Control.Monad.Exception.Base
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Word (Word16)

import Language.Haskell.Exts

import JVM.Assembler
import JVM.Builder hiding (Generate, generate)
import JVM.ClassFile
import JVM.Converter

import qualified Java.Lang
import qualified Java.IO

import Blimp.Generate

type Emitter = Generate (Caught SomeException NoExceptions) ()

compileApp :: Exp -> Emitter -> Emitter
compileApp (Var (UnQual (Ident "putStrLn"))) b = do
  getStaticField Java.Lang.system Java.IO.out
  b
  invokeVirtual Java.IO.printStream Java.IO.println

compileCon :: QName -> Emitter
compileCon (UnQual (Ident "False")) = iconst_0
compileCon (UnQual (Ident "True")) = iconst_1

calculateOffset :: [Emitter] -> Word16
calculateOffset = foldl (+) 0 . map (fromIntegral . generateCodeLength)

i0offset :: (Word16 -> Instruction) -> [Emitter] -> Emitter
i0offset f es = i0 . f . calculateOffset $ (i0 . f $ 0) : es

compileExp :: Exp -> Emitter
compileExp (App a b) = compileApp a (compileExp b)
compileExp (Con c) = compileCon c
compileExp (Lit (String s)) = loadString s
compileExp (Paren a) = compileExp a
compileExp (If b t f) = do
  let trueBlock = do
        compileExp t
        i0offset GOTO [compileExp f]
      falseBlock = do
        compileExp f
  compileExp b
  i0offset (IF C_EQ) [trueBlock]
  trueBlock
  falseBlock
compileExp a = error . show $ a

compileRhs :: Rhs -> Emitter
compileRhs (UnGuardedRhs exp) = compileExp exp >> i0 RETURN

compileDecl :: Decl -> Emitter
compileDecl (PatBind _ (PVar (Ident name)) sig rhs binds) = do
  newMethod [ACC_PUBLIC, ACC_STATIC] (B.pack name) [arrayOf Java.Lang.stringClass] ReturnsVoid $ compileRhs rhs
  return ()

compileModule :: Module -> Emitter
compileModule (Module _ (ModuleName name) _ Nothing exports imports decls) = do
  mapM_ compileDecl decls

compile :: ParseResult Module -> Emitter
compile (ParseOk m) = compileModule m
compile (ParseFailed srcloc msg) = error msg

main :: IO ()
main = do
  result <- parseFile "examples/Test.bl"
  testClass <- return $ generate [] (B.pack "Test") (compile result)
  B.writeFile "Test.class" (encodeClass testClass)
