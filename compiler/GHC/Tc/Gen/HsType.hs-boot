module GHC.Tc.Gen.HsType (
    tcSynthesizeMillerMotive,
  ) where

import GHC.Prelude ( Maybe )
import GHC.Core.Coercion.Axiom ( Branched, CoAxiom )
import GHC.Core.TyCo.Rep ( Kind, Type )
import GHC.Tc.Types ( TcM )
import GHC.Types.SrcLoc ( SrcSpan )
import GHC.Types.Var ( Var )

tcSynthesizeMillerMotive
  :: SrcSpan -> [Var] -> Kind -> Type
  -> TcM (Maybe (Type, CoAxiom Branched, [Type]))
