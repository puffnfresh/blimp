{-# LANGUAGE FlexibleContexts, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Blimp.Generate where

{- This module is an extraction of JVM.Builder.Monad because we need
   to use bytecode length to calculate offsets. hs-java doesn't allow
   access to Generate's GState, which has the length of the bytecode.

   Push this stuff back into the hs-java project. -}

import Prelude hiding (catch)
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.State as St
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word

import Java.ClassPath
import JVM.Assembler
import JVM.Builder.Monad hiding (Generate, generate)
import JVM.ClassFile

newtype Generate e a = Generate {
  runGenerate :: EMT e (State GState) a
} deriving (Monad, MonadState GState)

instance (MonadState GState (EMT e (State GState))) => Generator e Generate where
  throwG e = Generate (throw e)

initClass :: (Generator e g) => B.ByteString -> g e Word16
initClass name = do
  addToPool (CClass "java/lang/Object")
  addToPool (CClass name)
  addToPool (CString "Code")

execGenerate :: [Tree CPEntry]
             -> Generate (Caught SomeException NoExceptions) a
             -> GState
execGenerate cp (Generate emt) = do
    let caught = emt `catch` (\(e :: SomeException) -> fail $ show e)
    execState (runEMT caught) (emptyGState {classPath = cp})

encodedCodeLength :: GState -> Word32
encodedCodeLength st = fromIntegral . B.length . encodeInstructions $ generated st

generateCodeLength :: Generate (Caught SomeException NoExceptions) a -> Word32
generateCodeLength = encodedCodeLength . execGenerate []

genCode :: GState -> Code
genCode st = Code {
    codeStackSize = stackSize st,
    codeMaxLocals = locals st,
    codeLength = encodedCodeLength st,
    codeInstructions = generated st,
    codeExceptionsN = 0,
    codeExceptions = [],
    codeAttrsN = 0,
    codeAttributes = AP [] }

generate :: [Tree CPEntry]
         -> B.ByteString
         -> Generate (Caught SomeException NoExceptions) ()
         -> Class Direct
generate cp name gen =
  let generator = do
        initClass name
        gen
      res = execGenerate cp generator
      code = genCode res
      d = defaultClass :: Class Direct
  in  d {
        constsPoolSize = fromIntegral $ M.size (currentPool res),
        constsPool = currentPool res,
        accessFlags = S.fromList [ACC_PUBLIC, ACC_STATIC],
        thisClass = name,
        superClass = "java/lang/Object",
        classMethodsCount = fromIntegral $ length (doneMethods res),
        classMethods = doneMethods res }
