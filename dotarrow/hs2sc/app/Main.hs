{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude hiding (exp)
import Expr (exp)

import Data.Text (pack, replace, unpack)
import Language.Haskell.TH.Ppr hiding (ppr)
import Language.Haskell.Meta (parseDecs)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH (pprint)
import System.Environment (getArgs)


ppr :: Exp -> String 
ppr = unpack . replace "GHC.Num." "" . pack . pprint

src :: Quasi q => q Exp -> q Exp -> q Exp
src imp obj =
           do
              imp' <- imp
              obj' <- obj
              return $ AppE (ConE (Name (OccName "SmSource") NameS)) (ListE [imp', obj'])

imp :: Quasi q => q Exp
imp =
   do
      return $ AppE (ConE (Name (OccName "SImpExp") NameS))
                    (ParensE (AppE (ConE (Name (OccName "SmImportS") NameS))
                                   (ListE [AppE (AppE (ConE (Name (OccName "SmImporter") NameS))
                                                      (ParensE (AppE (ConE (Name (OccName "RTSelect") NameS))
                                                                     (ParensE (AppE (AppE (ConE (Name (OccName "SmSelectRT") NameS))
                                                                                          (ParensE (AppE (ConE (Name (OccName "TRef") NameS))
                                                                                                         (ParensE (AppE (ConE (Name (OccName "RTName") NameS))
                                                                                                                        (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS))
                                                                                                                                       (LitE (StringL "cats")))))))))
                                                                                    (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS)) (LitE (StringL "effect")))))))))
                                                 (ListE [AppE (ConE (Name (OccName "SmNameI") NameS))
                                                              (ParensE (AppE (ConE (Name (OccName "NName") NameS))
                                                                             (ParensE (AppE (ConE (Name (OccName "SmIndeterminateN") NameS))
                                                                                            (LitE (StringL "IO"))))))
                                                        ,AppE (ConE (Name (OccName "SmNameI") NameS))
                                                              (ParensE (AppE (ConE (Name (OccName "NName") NameS))
                                                                             (ParensE (AppE (ConE (Name (OccName "SmIndeterminateN") NameS))
                                                                                            (LitE (StringL "IOApp"))))))])])))

app :: Quasi q => [Dec] -> q Exp
app [SigD (Name (OccName "main") _)
          (AppT (ConT (Name (OccName "IO") _))
                (ConT (Name (OccName "()") _)))
    ,ValD (VarP (Name (OccName "main") _))
          (NormalB (DoE Nothing stmts))
          []
    ] =
      do
        def' <- def stmts
        return $ AppE (ConE (Name (OccName "SObject") NameS)) (ParensE (AppE (AppE (AppE (ConE (Name (OccName "SmObjectS") NameS)) (ListE [])) (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS)) (LitE (StringL "App"))))) (ParensE (AppE (AppE (AppE (AppE (AppE (ConE (Name (OccName "SmTemplate") NameS)) (ListE [])) (ListE [AppE (AppE (AppE (ConE (Name (OccName "SmInit") NameS)) (ParensE (AppE (ConE (Name (OccName "T'Ref") NameS)) (ParensE (AppE (ConE (Name (OccName "RT'Ref") NameS)) (ParensE (AppE (AppE (ConE (Name (OccName "SmSelectRT'") NameS)) (ParensE (AppE (ConE (Name (OccName "RTName") NameS)) (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS)) (LitE (StringL "IOApp"))))))) (ParensE (AppE (ConE (Name (OccName "SmNameT'") NameS)) (LitE (StringL "Simple"))))))))))) (ParensE (AppE (ConE (Name (OccName "NName") NameS)) (ConE (Name (OccName "SmAnonymousN") NameS))))) (ListE [])])) (ParensE (AppE (AppE (ConE (Name (OccName "SmSelf") NameS)) (ParensE (AppE (ConE (Name (OccName "NName") NameS)) (ConE (Name (OccName "SmAnonymousN") NameS))))) (ConE (Name (OccName "Nothing") NameS))))) (ListE [def'])) (ListE [])))))
app _ = error "app"

def :: Quasi q => [Stmt] -> q Exp
def stmts =
         do
           gen' <- traverse gen stmts
           return $ AppE (ConE (Name (OccName "SDef") NameS))
                         (ParensE (AppE (AppE (AppE (AppE (AppE (ConE (Name (OccName "SmDefS") NameS))
                                                                (ListE [AppE (ConE (Name (OccName "MMod") NameS))
                                                                             (ConE (Name (OccName "SmOverrideM") NameS))]))
                                                          (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS))
                                                                         (LitE (StringL "run")))))
                                                    (ListE []))
                                              (ParensE (AppE (ConE (Name (OccName "Just") NameS))
                                                             (ParensE (AppE (ConE (Name (OccName "T'Apply") NameS))
                                                                            (ParensE (AppE (AppE (ConE (Name (OccName "SmApplyT'") NameS))
                                                                                                 (ParensE (AppE (ConE (Name (OccName "T'Ref") NameS))
                                                                                                                (ParensE (AppE (ConE (Name (OccName "RT'Name") NameS))
                                                                                                                               (ParensE (AppE (ConE (Name (OccName "SmNameT'") NameS))
                                                                                                                                              (LitE (StringL "IO")))))))))
                                                                                           (ParensE (AppE (ConE (Name (OccName "SmArgClauseT'") NameS))
                                                                                                          (ListE [AppE (ConE (Name (OccName "T'Ref") NameS))
                                                                                                                       (ParensE (AppE (ConE (Name (OccName "RT'Name") NameS))
                                                                                                                                      (ParensE (AppE (ConE (Name (OccName "SmNameT'") NameS))
                                                                                                                                                     (LitE (StringL "Unit"))))))]))))))))))
                                        (ParensE (AppE (ConE (Name (OccName "TForYield") NameS))
                                                       (ParensE (AppE (AppE (ConE (Name (OccName "SmForYieldT") NameS))
                                                                            (ListE gen'))
                                                                      (ParensE (AppE (ConE (Name (OccName "TLit") NameS))
                                                                                     (ConE (Name (OccName "SmUnitL") NameS))))))))))

gen :: Quasi q => Stmt -> q Exp
gen (BindS (SigP (VarP (Name (OccName name) _)) (ConT (Name (OccName "()") _)))
           (AppE (VarE (Name (OccName "print") _)) it)
    ) =
     do
        exp' <- $(exp) ("println(" ++ ppr it ++ ")")
        return $ AppE (ConE (Name (OccName "EGenerator") NameS))
                      (ParensE (AppE (AppE (ConE (Name (OccName "SmGeneratorE") NameS))
                                           (ParensE (AppE (ConE (Name (OccName "PPat") NameS))
                                                          (ParensE (AppE (AppE (ConE (Name (OccName "SmTypedP") NameS))
                                                                               (ParensE (AppE (ConE (Name (OccName "PVar") NameS))
                                                                                              (ParensE (AppE (ConE (Name (OccName "SmVarP") NameS))
                                                                                                             (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS))
                                                                                                                            (LitE (StringL name)))))))))
                                                                         (ParensE (AppE (ConE (Name (OccName "T'Ref") NameS))
                                                                                        (ParensE (AppE (ConE (Name (OccName "RT'Name") NameS))
                                                                                                       (ParensE (AppE (ConE (Name (OccName "SmNameT'") NameS))
                                                                                                                      (LitE (StringL "Unit")))))))))))))
                                     (ParensE (AppE (ConE (Name (OccName "TApply") NameS))
                                                    (ParensE (AppE (AppE (ConE (Name (OccName "SmApplyT") NameS))
                                                                         (ParensE (AppE (ConE (Name (OccName "TRef") NameS))
                                                                                        (ParensE (AppE (ConE (Name (OccName "RTName") NameS))
                                                                                                       (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS))
                                                                                                                      (LitE (StringL "IO")))))))))
                                                                   (ParensE (AppE (AppE (ConE (Name (OccName "SmArgClauseT") NameS))
                                                                                        (ListE [AppE (ConE (Name (OccName "TTerm") NameS))
                                                                                                     (ParensE (AppE (ConE (Name (OccName "SmBlockT") NameS))
                                                                                                                    (ListE [exp'])))]))
                                                                                  (ConE (Name (OccName "Nothing") NameS))))))))))
gen (BindS (SigP (VarP (Name (OccName name) _))
                 (ConT (Name (OccName type') _)))
           (AppE (VarE (Name (OccName _) _)) it)
--           (AppE (VarE (Name (OccName "GHC.Base.pure") _)) it)
    ) =
     do
        exp' <- $(exp) (ppr it)
        return $ AppE (ConE (Name (OccName "EGenerator") NameS))
                      (ParensE (AppE (AppE (ConE (Name (OccName "SmGeneratorE") NameS))
                                            (ParensE (AppE (ConE (Name (OccName "PPat") NameS))
                                                           (ParensE (AppE (AppE (ConE (Name (OccName "SmTypedP") NameS))
                                                                                 (ParensE (AppE (ConE (Name (OccName "PVar") NameS))
                                                                                                (ParensE (AppE (ConE (Name (OccName "SmVarP") NameS))
                                                                                                               (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS))
                                                                                                                              (LitE (StringL name)))))))))
                                                                          (ParensE (AppE (ConE (Name (OccName "T'Ref") NameS))
                                                                                         (ParensE (AppE (ConE (Name (OccName "RT'Name") NameS))
                                                                                                        (ParensE (AppE (ConE (Name (OccName "SmNameT'") NameS))
                                                                                                                       (LitE (StringL type')))))))))))))
                                     (ParensE (AppE (ConE (Name (OccName "TApply") NameS))
                                                    (ParensE (AppE (AppE (ConE (Name (OccName "SmApplyT") NameS))
                                                                         (ParensE (AppE (ConE (Name (OccName "TRef") NameS))
                                                                                        (ParensE (AppE (ConE (Name (OccName "RTName") NameS))
                                                                                                       (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS))
                                                                                                                      (LitE (StringL "IO")))))))))
                                                                   (ParensE (AppE (AppE (ConE (Name (OccName "SmArgClauseT") NameS))
                                                                                        (ListE [AppE (ConE (Name (OccName "TTerm") NameS))
                                                                                                     (ParensE (AppE (ConE (Name (OccName "SmBlockT") NameS))
                                                                                                                    (ListE [exp'])))]))
                                                                                  (ConE (Name (OccName "Nothing") NameS))))))))))
gen (NoBindS (AppE (VarE (Name (OccName "return") _)) (ConE (Name (OccName "()") _)))
    ) =
     do
       return $ AppE (ConE (Name (OccName "EGenerator") NameS))
                     (ParensE (AppE (AppE (ConE (Name (OccName "SmGeneratorE") NameS))
                                          (ParensE (AppE (ConE (Name (OccName "PPat") NameS))
                                                         (ParensE (ConE (Name (OccName "SmWildcardP") NameS))))))
                                    (ParensE (AppE (ConE (Name (OccName "TRef") NameS))
                                                   (ParensE (AppE (ConE (Name (OccName "RTSelect") NameS))
                                                                  (ParensE (AppE (AppE (ConE (Name (OccName "SmSelectRT") NameS))
                                                                                       (ParensE (AppE (ConE (Name (OccName "TApply") NameS))
                                                                                                      (ParensE (AppE (AppE (ConE (Name (OccName "SmApplyT") NameS))
                                                                                                                           (ParensE (AppE (ConE (Name (OccName "TRef") NameS))
                                                                                                                                          (ParensE (AppE (ConE (Name (OccName "RTName") NameS))
                                                                                                                                                         (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS))
                                                                                                                                                                        (LitE (StringL "IO")))))))))
                                                                                                                     (ParensE (AppE (AppE (ConE (Name (OccName "SmArgClauseT") NameS))
                                                                                                                                          (ListE [AppE (ConE (Name (OccName "TTerm") NameS))
                                                                                                                                                       (ParensE (AppE (ConE (Name (OccName "SmBlockT") NameS))
                                                                                                                                                                      (ListE [])))])) (ConE (Name (OccName "Nothing") NameS)))))))))
                                                                                 (ParensE (AppE (ConE (Name (OccName "SmNameT") NameS))
                                                                                                (LitE (StringL "void"))))))))))))
gen _ = error "gen"


main :: IO ()
main = do
   [arg] <- getArgs
   str <- readFile arg
   let Right ds = parseDecs str
   src' <- src imp (app ds)
   putStrLn (pprint src')
   return ()
