module Language.CL.C.StdLib 
       ( localMemFence, globalMemFence
       ) where

import Language.CL.C.HOAS.AST
import Language.CL.C.HOAS.Sugar
import Language.CL.C.Types


data CLMemFenceFlags = CLKLocalMemFence | CLKGlobalMemFence


instance Literal CLMemFenceFlags where 
  showLit CLKLocalMemFence  = "CLK_LOCAL_MEM_FENCE"
  showLit CLKGlobalMemFence = "CLK_GLOBAL_MEM_FENCE"  
  
instance LangType CLMemFenceFlags where
  typeOf _ = typeOf (undefined :: CLInt)

instance ParamType CLMemFenceFlags where

barrier :: C CLMemFenceFlags -> C CLVoid
barrier = mkBuiltInFun "barrier" 

localMemFence :: Body r
localMemFence  = sequen $ barrier $ mkLit CLKLocalMemFence

globalMemFence :: Body r
globalMemFence = sequen $ barrier $ mkLit CLKGlobalMemFence
