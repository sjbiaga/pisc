#!/usr/bin/perl -X
# -*- mode: perl; indent-tabs-mode: nil; eval: (set-fill-column 120); -*-

use strict;
use warnings;

use feature "refaliasing";
no warnings "experimental::refaliasing";

$, = "";
$\ = "";

## C #################################################################################################################################################

my $CASE      = { "Case"     => [ "SmCaseC", \(\&Pat), \(\&TermOpt), \(\&Term) ] };
my $TYPE_CASE = { "TypeCase" => [ "SmType'CaseT'C", \(\&Type_), \(\&Type_) ] };

my $CTOR = { "Ctor.Primary" => [ "SmCtorPrimary", \(\&ModList), \(\&Name), \(\&TermPCList) ] };

## E #################################################################################################################################################

my $ENUM = {
    "Enumerator.CaseGenerator" => [ "ECaseGenerator(SmCaseGeneratorE", \(\&Pat), \(\&Term) ],
    "Enumerator.Generator"     => [ "EGenerator(SmGeneratorE",         \(\&Pat), \(\&Term) ],
    "Enumerator.Guard"         => [ "EGuard(SmGuardE",                 \(\&Term) ],
    "Enumerator.Val"           => [ "EVal(SmValE",                     \(\&Pat), \(\&Term) ]
};

## I #################################################################################################################################################

my $INIT = { "Init" => [ "SmInit", \(\&Type_), \(\&Name), \(\&ArgClauseTList) ] };

my $IMPORTEE = {
    "Importee.Given"    => [ "SmGivenI",    \(\&Type_) ],
    "Importee.GivenAll" => [ "SmGivenAllI", \(\&Void) ],
    "Importee.Name"     => [ "SmNameI",     \(\&Name) ],
    "Importee.Rename"   => [ "SmRenameI",   \(\&Name), \(\&Name) ],
    "Importee.Unimport" => [ "SmUnimportI", \(\&Name) ],
    "Importee.Wildcard" => [ "SmWildcardI", \(\&Void) ]
};

my $IMPORTER = { "Importer" => [ "SmImporter", \(\&RefT), \(\&ImporteeList) ] };

## M #################################################################################################################################################

my $LIT = {
    "Lit.Boolean" => [ "SmBooleanL", \(\&Raw) ],
    "Lit.Byte"    => [ "SmByteL",    \(\&Raw) ],
    "Lit.Char"    => [ "SmCharL",    \(\&Raw) ],
    "Lit.Double"  => [ "SmDoubleL",  \(\&Raw) ],
    "Lit.Float"   => [ "SmFloatL",   \(\&Raw) ],
    "Lit.Int"     => [ "SmIntL",     \(\&Raw) ],
    "Lit.Long"    => [ "SmLongL",    \(\&Raw) ],
    "Lit.Null"    => [ "SmNullL",    \(\&Void) ],
    "Lit.Short"   => [ "SmShortL",   \(\&Raw) ],
    "Lit.String"  => [ "SmStringL",  \(\&Str) ],
    "Lit.Symbol"  => [ "SmSymbolL",  \(\&Sym) ],
    "Lit.Unit"    => [ "SmUnitL",    \(\&Void) ]
};

## M #################################################################################################################################################

my $MOD = {
    "Mod.Annot"         => [ "MAnnot(SmAnnotM",       \(\&Init) ],
    "Mod.Abstract"      => [ "MMod(SmAbstractM",      \(\&Void) ],
    "Mod.Case"          => [ "MMod(SmCaseM",          \(\&Void) ],
    "Mod.Covariant"     => [ "MMod(SmCovariantM",     \(\&Void) ],
    "Mod.Contravariant" => [ "MMod(SmContravariantM", \(\&Void) ],
    "Mod.Erased"        => [ "MMod(SmErasedM",        \(\&Void) ],
    "Mod.Final"         => [ "MMod(SmFinalM",         \(\&Void) ],
    "Mod.Implicit"      => [ "MMod(SmImplicitM",      \(\&Void) ],
    "Mod.Infix"         => [ "MMod(SmInfixM",         \(\&Void) ],
    "Mod.Inline"        => [ "MMod(SmInlineM",        \(\&Void) ],
    "Mod.Lazy"          => [ "MMod(SmLazyM",          \(\&Void) ],
    "Mod.Opaque"        => [ "MMod(SmOpaqueM",        \(\&Void) ],
    "Mod.Open"          => [ "MMod(SmOpenM",          \(\&Void) ],
    "Mod.Override"      => [ "MMod(SmOverrideM",      \(\&Void) ],
    "Mod.Private"       => [ "MAccess(SmPrivateM",    \(\&Ref) ],
    "Mod.Protected"     => [ "MAccess(SmProtectedM",  \(\&Ref) ],
    "Mod.Sealed"        => [ "MMod(SmSealedM",        \(\&Void) ],
    "Mod.Super"         => [ "MMod(SmSuperM",         \(\&Void) ],
    "Mod.Transparent"   => [ "MMod(SmTransparentM",   \(\&Void) ],
    "Mod.Using"         => [ "MMod(SmUsingM",         \(\&Void) ],
    "Mod.ValParam"      => [ "MMod(SmValParamM",      \(\&Void) ],
    "Mod.VarParam"      => [ "MMod(SmVarParamM",      \(\&Void) ]
};

## N #################################################################################################################################################

my $NAME = {
    "Name"               => [ "NName(SmIndeterminateN",   \(\&Str) ],
    "Name.Anonymous"     => [ "NName(SmAnonymousN",       \(\&Void) ],
    "Name.Indeterminate" => [ "NName(SmIndeterminateN",   \(\&Str) ],
    "Name.Placeholder"   => [ "NName(SmPlaceholderN",     \(\&Void) ],
    "Name.This"          => [ "NName(SmThisN",            \(\&Void) ],
    "Term.Anonymous"     => [ "NAnonymous(SmAnonymousRT", \(\&Void) ],
    "Term.Name"          => [ "NTName(SmNameT",           \(\&Str) ],
    "Type.Name"          => [ "NT'Name(SmNameT'",         \(\&Str) ]
};

## P #################################################################################################################################################

my $GPC = { "Member.ParamClauseGroup" => [ "ParamClauseGroup", \(\&Type_PC), \(\&TermPCList) ] };

my $ACP = { "Pat.ArgClause" => [ "SmArgClauseP", \(\&PatList) ] };

my $PAT = {
    "Lit.Boolean"      => [ "PLit(SmBooleanL",               \(\&Raw) ],
    "Lit.Byte"         => [ "PLit(SmByteL",                  \(\&Raw) ],
    "Lit.Char"         => [ "PLit(SmCharL",                  \(\&Raw) ],
    "Lit.Double"       => [ "PLit(SmDoubleL",                \(\&Raw) ],
    "Lit.Float"        => [ "PLit(SmFloatL",                 \(\&Raw) ],
    "Lit.Int"          => [ "PLit(SmIntL",                   \(\&Raw) ],
    "Lit.Long"         => [ "PLit(SmLongL",                  \(\&Raw) ],
    "Lit.Null"         => [ "PLit(SmNullL",                  \(\&Void) ],
    "Lit.Short"        => [ "PLit(SmShortL",                 \(\&Raw) ],
    "Lit.String"       => [ "PLit(SmStringL",                \(\&Str) ],
    "Lit.Symbol"       => [ "PLit(SmSymbolL",                \(\&Sym) ],
    "Lit.Unit"         => [ "PLit(SmUnitL",                  \(\&Void) ],
    "Pat.Alternative"  => [ "PPat(SmAlternativeP",           \(\&Pat),  \(\&Pat) ],
    "Pat.Bind"         => [ "PPat(SmBindP",                  \(\&Pat),  \(\&Pat) ],
    "Pat.Extract"      => [ "PExtract(SmExtractP",           \(\&Term), \(\&ArgClauseP) ],
    "Pat.ExtractInfix" => [ "PExtractInfix(SmExtractInfixP", \(\&Pat),  \(\&NameT), \(\&ArgClauseP) ],
    "Pat.Given"        => [ "PPat(SmGivenP",                 \(\&Type_) ],
    "Pat.Interpolate"  => [ "PPat(SmInterpolateP",           \(\&NameT), \(\&LitList), \(\&PatList) ],
    "Pat.Macro"        => [ "PMacro(SmMacroP",               \(\&Term) ],
    "Pat.Repeated"     => [ "PPat(SmRepeatedP",              \(\&NameT) ],
    "Term.Name"        => [ "PPat(SmNameP(SmNameT",          \(\&Str) ],
    "Term.Select"      => [ "PPat(SmSelectP(SmSelectRT",     \(\&Term), \(\&NameT) ],
    "Pat.SeqWildcard"  => [ "PPat(SmSeqWildcardP",           \(\&Void) ],
    "Pat.Tuple"        => [ "PTuple(SmTupleP",               \(\&PatList) ],
    "Pat.Typed"        => [ "PPat(SmTypedP",                 \(\&Pat), \(\&Type_) ],
    "Pat.Var"          => [ "PVar(SmVarP",                   \(\&NameT) ],
    "Pat.Wildcard"     => [ "PPat(SmWildcardP",              \(\&Void) ],
    "Pat.Xml"          => [ "PPat(SmXmlP",                   \(\&LitList), \(\&PatList) ]
};

## R #################################################################################################################################################

my $REF = {
    "Importee.Given"    => [ "RImportee(SmGivenI",    \(\&Type_) ],
    "Importee.GivenAll" => [ "RImportee(SmGivenAllI", \(\&Void) ],
    "Importee.Name"     => [ "RImportee(SmNameI",     \(\&Name) ],
    "Importee.Rename"   => [ "RImportee(SmRenameI",   \(\&Name), \(\&Name) ],
    "Importee.Unimport" => [ "RImportee(SmUnimportI", \(\&Name) ],
    "Importee.Wildcard" => [ "RImportee(SmWildcardI", \(\&Void) ],
    "Init"              => [ "RInit(SmInit",          \(\&Type_), \(\&Name), \(\&ArgClauseTList) ],

    #    "Term.Anonymous"     => [ "RName(NAnonymous(SmAnonymousRT",    \(\&Void) ],
    "Name"               => [ "RName(NName(SmIndeterminateN", \(\&Str) ],
    "Name.Anonymous"     => [ "RName(NName(SmAnonymousN",     \(\&Void) ],
    "Name.Indeterminate" => [ "RName(NName(SmIndeterminateN", \(\&Str) ],
    "Name.Placeholder"   => [ "RName(NName(SmPlaceholderN",   \(\&Void) ],
    "Name.This"          => [ "RName(NName(SmThisN",          \(\&Void) ],

    #    "Term.Anonymous"  => [ "R_TRef(RRTAnonymous(SmAnonymousRT", \(\&Void) ],
    "Term.ApplyUnary" => [ "R_TRef(RTRef(SmApplyUnaryRT",   \(\&NameT), \(\&Term) ],
    "Term.Name"       => [ "R_TRef(RTName(SmNameT",         \(\&Str) ],
    "Term.Select"     => [ "R_TRef(RTSelect(SmSelectRT",    \(\&Term), \(\&NameT) ],
    "Term.Super"      => [ "R_TRef(RTRef(SmSuperRT",        \(\&Name), \(\&Name) ],
    "Term.This"       => [ "R_TRef(RTRef(SmThisRT",         \(\&Name) ],
    "Type.Name"       => [ "R_T'Ref(RT'Name(SmNameT'",      \(\&Str) ],
    "Type.Project"    => [ "R_T'Ref(RT'Ref(SmProjectRT'",   \(\&Type_), \(\&NameT_) ],
    "Type.Select"     => [ "R_T'Ref(RT'Ref(SmSelectRT'",    \(\&RefT),  \(\&NameT_) ],
    "Type.Singleton"  => [ "R_T'Ref(RT'Ref(SmSingletonRT'", \(\&RefT) ]
};

## S #################################################################################################################################################

my $SELF = { "Self" => [ "SmSelf", \(\&Name), \(\&DecltpeOpt) ] };

my $SOURCE = { "Source" => [ "SmSource", \(\&StatList) ] };

my $STAT = {
    "Ctor.Secondary"         => [ "SCtorSecondary(SmCtorSecondaryS", \(\&ModList), \(\&Name), \(\&TermPCList), \(\&Init), \(\&StatList) ],
    "Decl.Def"               => [ "SDef'(SmDef'S",                             \(\&ModList),    \(\&NameT),   \(\&GroupPCList), \(\&Decltpe) ],
    "Decl.Given"             => [ "SGiven'(SmGiven'S",                         \(\&ModList),    \(\&NameT),   \(\&GroupPCOpt),  \(\&Decltpe) ],
    "Decl.Type"              => [ "SType'(SmType'S",                           \(\&ModList),    \(\&NameT_),  \(\&Type_PC),     \(\&Bounds_) ],
    "Decl.Val"               => [ "SVal'(SmVal'S",                             \(\&ModList),    \(\&PatList), \(\&Decltpe) ],
    "Decl.Var"               => [ "SVar'(SmVar'S",                             \(\&ModList),    \(\&PatList), \(\&Decltpe) ],
    "Defn.Class"             => [ "SClass(SmClassS",                           \(\&ModList),    \(\&NameT_), \(\&Type_PC),     \(\&Ctor),       \(\&Templ) ],
    "Defn.Def"               => [ "SDef(SmDefS",                               \(\&ModList),    \(\&NameT),  \(\&GroupPCList), \(\&DecltpeOpt), \(\&Term) ],
    "Defn.Enum"              => [ "SEnum(SmEnumS",                             \(\&ModList),    \(\&NameT_), \(\&Type_PC),     \(\&Ctor),       \(\&Templ) ],
    "Defn.EnumCase"          => [ "SEnumCase(SmEnumCaseS",                     \(\&ModList),    \(\&NameT),  \(\&Type_PC),     \(\&Ctor),       \(\&InitList) ],
    "Defn.ExtensionGroup"    => [ "SExtensionGroup(SmExtensionGroupS",         \(\&GroupPCOpt), \(\&Stat) ],
    "Defn.Given"             => [ "SGiven(SmGivenS",                           \(\&ModList),    \(\&Name),  \(\&GroupPCOpt),  \(\&Templ) ],
    "Defn.GivenAlias"        => [ "SGivenAlias(SmGivenAliasS",                 \(\&ModList),    \(\&Name),  \(\&GroupPCOpt),  \(\&Decltpe),    \(\&Term) ],
    "Defn.Macro"             => [ "SMacro(SmMacroS",                           \(\&ModList),    \(\&NameT), \(\&GroupPCList), \(\&DecltpeOpt), \(\&Term) ],
    "Defn.Object"            => [ "SObject(SmObjectS",                         \(\&ModList),    \(\&NameT), \(\&Templ) ],
    "Defn.RepeatedEnumCase"  => [ "SRepeatedEnumCase(SmRepeatedEnumCase",      \(\&ModList),    \(\&NameTList) ],
    "Defn.Trait"             => [ "STrait(SmTraitS",                           \(\&ModList),    \(\&NameT_),  \(\&Type_PC),    \(\&Ctor),  \(\&Templ) ],
    "Defn.Type"              => [ "SType(SmTypeS",                             \(\&ModList),    \(\&NameT_),  \(\&Type_PC),    \(\&Type_), \(\&Bounds_) ],
    "Defn.Val"               => [ "SVal(SmValS",                               \(\&ModList),    \(\&PatList), \(\&DecltpeOpt), \(\&Term) ],
    "Defn.Var"               => [ "SVar(SmVarS",                               \(\&ModList),    \(\&PatList), \(\&DecltpeOpt), \(\&Term) ],
    "Export"                 => [ "SImpExp(SmExportS",                         \(\&ImporterList) ],
    "Import"                 => [ "SImpExp(SmImportS",                         \(\&ImporterList) ],
    "Pkg"                    => [ "SPkg(SmPkgS",                               \(\&RefT),    \(\&StatList) ],
    "Pkg.Object"             => [ "SPkgObject(SmPkgObjectS",                   \(\&ModList), \(\&NameT), \(\&Templ) ],
    "Lit.Boolean"            => [ "STerm(TLit(SmBooleanL",                     \(\&Raw) ],
    "Lit.Byte"               => [ "STerm(TLit(SmByteL",                        \(\&Raw) ],
    "Lit.Char"               => [ "STerm(TLit(SmCharL",                        \(\&Raw) ],
    "Lit.Double"             => [ "STerm(TLit(SmDoubleL",                      \(\&Raw) ],
    "Lit.Float"              => [ "STerm(TLit(SmFloatL",                       \(\&Raw) ],
    "Lit.Int"                => [ "STerm(TLit(SmIntL",                         \(\&Raw) ],
    "Lit.Long"               => [ "STerm(TLit(SmLongL",                        \(\&Raw) ],
    "Lit.Null"               => [ "STerm(TLit(SmNullL",                        \(\&Void) ],
    "Lit.Short"              => [ "STerm(TLit(SmShortL",                       \(\&Raw) ],
    "Lit.String"             => [ "STerm(TLit(SmStringL",                      \(\&Str) ],
    "Lit.Symbol"             => [ "STerm(TLit(SmSymbolL",                      \(\&Sym) ],
    "Lit.Unit"               => [ "STerm(TLit(SmUnitL",                        \(\&Void) ],
    "Term.Annotate"          => [ "STerm(TTerm(SmAnnotateT",                   \(\&Term), \(\&AnnotList) ],
    "Term.AnonymousFunction" => [ "STerm(TTerm(SmAnonymousFunctionT",          \(\&Term) ],
    "Term.Apply"             => [ "STerm(TApply(SmApplyT",                     \(\&Term), \(\&ArgClauseT) ],
    "Term.ApplyUsing"        => [ "STerm(TApply(SmApplyUsingT",                \(\&Term), \(\&ArgClauseT) ],
    "Term.ApplyInfix"        => [ "STerm(TApplyInfix(SmApplyInfixT",           \(\&Term), \(\&NameT), \(\&ArgClauseT_), \(\&ArgClauseT) ],
    "Term.ApplyType"         => [ "STerm(TApplyType'(SmApplyType'T",           \(\&Term), \(\&ArgClauseT_) ],
    "Term.Ascribe"           => [ "STerm(TTerm(SmAscribeT",                    \(\&Term), \(\&Type_) ],
    "Term.Assign"            => [ "STerm(TAssign(SmAssignT",                   \(\&Term), \(\&Term) ],
    "Term.Block"             => [ "STerm(TTerm(SmBlockT",                      \(\&StatList) ],
    "Term.ContextFunction"   => [ "STerm(TContextFunction(SmContextFunctionT", \(\&TermPC), \(\&Term) ],
    "Term.Do"                => [ "STerm(TDo(SmDoT",                           \(\&Term),   \(\&Term) ],
    "Term.EndMarker"         => [ "STerm(TTerm(SmEndMarkerT",                  \(\&NameT) ],
    "Term.Eta"               => [ "STerm(TTerm(SmEtaT",                        \(\&Term) ],
    "Term.For"               => [ "STerm(TFor(SmForT",                         \(\&EnumList), \(\&Term) ],
    "Term.ForYield"          => [ "STerm(TForYield(SmForYieldT",               \(\&EnumList), \(\&Term) ],
    "Term.Function"          => [ "STerm(TFunction(SmFunctionT",               \(\&TermPC),   \(\&Term) ],
    "Term.If"                => [ "STerm(TTerm(SmIfT",                         \(\&Term),     \(\&Term),     \(\&Term), \(\&ModList) ],
    "Term.Interpolate"       => [ "STerm(TTerm(SmInterpolateT",                \(\&NameT),    \(\&LitList),  \(\&TermList) ],
    "Term.Match"             => [ "STerm(TMatch(SmMatchT",                     \(\&Term),     \(\&CaseList), \(\&ModList) ],
    "Term.New"               => [ "STerm(TTerm(SmNewT",                        \(\&Init) ],
    "Term.NewAnonymous"      => [ "STerm(TNewAnonymous(SmNewAnonymousT",       \(\&Templ) ],
    "Term.PartialFunction"   => [ "STerm(TPartialFunction(SmPartialFunctionT", \(\&CaseList) ],
    "Term.Placeholder"       => [ "STerm(TTerm(SmPlaceholder",                 \(\&Void) ],
    "Term.PolyFunction"      => [ "STerm(TPolyFunction(SmPolyFunctionT",       \(\&Type_PC), \(\&Term) ],
    "Term.QuotedMacroExpr"   => [ "STerm(TTerm(SmQuotedMacroExprT",            \(\&Term) ],
    "Term.QuotedMacroType"   => [ "STerm(TTerm(SmQuotedMacroType'T",           \(\&Type_) ],
    "Term.Repeated"          => [ "STerm(TTerm(SmRepeatedT",                   \(\&Term) ],
    "Term.Return"            => [ "STerm(TTerm(SmReturnT",                     \(\&Term) ],
    "Term.SplicedMacroExpr"  => [ "STerm(TTerm(SmSplicedMacroExprT",           \(\&Term) ],
    "Term.SplicedMacroPat"   => [ "STerm(TTerm(SmSplicedMacroPatT",            \(\&Pat) ],
    "Term.Throw"             => [ "STerm(TTerm(SmThrowT",                      \(\&Term) ],
    "Term.Try"               => [ "STerm(TTry(SmTryT",                         \(\&Term), \(\&CaseList), \(\&TermOpt) ],
    "Term.TryWithHandler"    => [ "STerm(TTerm(SmTryWithHandlerT",             \(\&Term), \(\&Term),     \(\&TermOpt) ],
    "Term.Tuple"             => [ "STerm(TTuple(SmTupleT",                     \(\&TermList) ],
    "Term.While"             => [ "STerm(TWhile(SmWhileT",                     \(\&Term),    \(\&Term) ],
    "Term.Xml"               => [ "STerm(TTerm(SmXmlT",                        \(\&LitList), \(\&TermList) ],
    "Term.Anonymous"         => [ "STerm(TRef(RTAnonymous(SmAnonymousRT",      \(\&Void) ],
    "Term.ApplyUnary"        => [ "STerm(TRef(RTRef(SmApplyUnaryRT",           \(\&NameT), \(\&Term) ],
    "Term.Name"              => [ "STerm(TRef(RTName(SmNameT",                 \(\&Str) ],
    "Term.Select"            => [ "STerm(TRef(RTSelect(SmSelectRT",            \(\&Term), \(\&NameT) ],
    "Term.Super"             => [ "STerm(TRef(RTRef(SmSuperRT",                \(\&Name), \(\&Name) ],
    "Term.This"              => [ "STerm(TRef(RTRef(SmThisRT",                 \(\&Name) ]
};

## T #################################################################################################################################################

my $TEMPLATE = { "Template" => [ "SmTemplate", \(\&StatList), \(\&InitList), \(\&Self), \(\&StatList), \(\&Type_List) ] };

my $TERM = {
    "Lit.Boolean"            => [ "TLit(SmBooleanL",                     \(\&Raw) ],
    "Lit.Byte"               => [ "TLit(SmByteL",                        \(\&Raw) ],
    "Lit.Char"               => [ "TLit(SmCharL",                        \(\&Raw) ],
    "Lit.Double"             => [ "TLit(SmDoubleL",                      \(\&Raw) ],
    "Lit.Float"              => [ "TLit(SmFloatL",                       \(\&Raw) ],
    "Lit.Int"                => [ "TLit(SmIntL",                         \(\&Raw) ],
    "Lit.Long"               => [ "TLit(SmLongL",                        \(\&Raw) ],
    "Lit.Null"               => [ "TLit(SmNullL",                        \(\&Void) ],
    "Lit.Short"              => [ "TLit(SmShortL",                       \(\&Raw) ],
    "Lit.String"             => [ "TLit(SmStringL",                      \(\&Str) ],
    "Lit.Symbol"             => [ "TLit(SmSymbolL",                      \(\&Sym) ],
    "Lit.Unit"               => [ "TLit(SmUnitL",                        \(\&Void) ],
    "Term.Annotate"          => [ "TTerm(SmAnnotateT",                   \(\&Term), \(\&AnnotList) ],
    "Term.AnonymousFunction" => [ "TTerm(SmAnonymousFunctionT",          \(\&Term) ],
    "Term.Apply"             => [ "TApply(SmApplyT",                     \(\&Term), \(\&ArgClauseT) ],
    "Term.ApplyUsing"        => [ "TApply(SmApplyUsingT",                \(\&Term), \(\&ArgClauseT) ],
    "Term.ApplyInfix"        => [ "TApplyInfix(SmApplyInfixT",           \(\&Term), \(\&NameT), \(\&ArgClauseT_), \(\&ArgClauseT) ],
    "Term.ApplyType"         => [ "TApplyType'(SmApplyType'T",           \(\&Term), \(\&ArgClauseT_) ],
    "Term.Ascribe"           => [ "TTerm(SmAscribeT",                    \(\&Term), \(\&Type_) ],
    "Term.Assign"            => [ "TAssign(SmAssignT",                   \(\&Term), \(\&Term) ],
    "Term.Block"             => [ "TTerm(SmBlockT",                      \(\&StatList) ],
    "Term.ContextFunction"   => [ "TContextFunction(SmContextFunctionT", \(\&TermPC), \(\&Term) ],
    "Term.Do"                => [ "TDo(SmDoT",                           \(\&Term),   \(\&Term) ],
    "Term.EndMarker"         => [ "TTerm(SmEndMarkerT",                  \(\&NameT) ],
    "Term.Eta"               => [ "TTerm(SmEtaT",                        \(\&Term) ],
    "Term.For"               => [ "TFor(SmForT",                         \(\&EnumList), \(\&Term) ],
    "Term.ForYield"          => [ "TForYield(SmForYieldT",               \(\&EnumList), \(\&Term) ],
    "Term.Function"          => [ "TFunction(SmFunctionT",               \(\&TermPC),   \(\&Term) ],
    "Term.If"                => [ "TTerm(SmIfT",                         \(\&Term),     \(\&Term),     \(\&Term), \(\&ModList) ],
    "Term.Interpolate"       => [ "TTerm(SmInterpolateT",                \(\&NameT),    \(\&LitList),  \(\&TermList) ],
    "Term.Match"             => [ "TMatch(SmMatchT",                     \(\&Term),     \(\&CaseList), \(\&ModList) ],
    "Term.New"               => [ "TTerm(SmNewT",                        \(\&Init) ],
    "Term.NewAnonymous"      => [ "TNewAnonymous(SmNewAnonymousT",       \(\&Templ) ],
    "Term.PartialFunction"   => [ "TPartialFunction(SmPartialFunctionT", \(\&CaseList) ],
    "Term.Placeholder"       => [ "TTerm(SmPlaceholder",                 \(\&Void) ],
    "Term.PolyFunction"      => [ "TPolyFunction(SmPolyFunctionT",       \(\&Type_PC), \(\&Term) ],
    "Term.QuotedMacroExpr"   => [ "TTerm(SmQuotedMacroExprT",            \(\&Term) ],
    "Term.QuotedMacroType"   => [ "TTerm(SmQuotedMacroType'T",           \(\&Type_) ],
    "Term.Repeated"          => [ "TTerm(SmRepeatedT",                   \(\&Term) ],
    "Term.Return"            => [ "TTerm(SmReturnT",                     \(\&Term) ],
    "Term.SplicedMacroExpr"  => [ "TTerm(SmSplicedMacroExprT",           \(\&Term) ],
    "Term.SplicedMacroPat"   => [ "TTerm(SmSplicedMacroPatT",            \(\&Pat) ],
    "Term.Throw"             => [ "TTerm(SmThrowT",                      \(\&Term) ],
    "Term.Try"               => [ "TTry(SmTryT",                         \(\&Term), \(\&CaseList), \(\&TermOpt) ],
    "Term.TryWithHandler"    => [ "TTerm(SmTryWithHandlerT",             \(\&Term), \(\&Term),     \(\&TermOpt) ],
    "Term.Tuple"             => [ "TTuple(SmTupleT",                     \(\&TermList) ],
    "Term.While"             => [ "TWhile(SmWhileT",                     \(\&Term),    \(\&Term) ],
    "Term.Xml"               => [ "TTerm(SmXmlT",                        \(\&LitList), \(\&TermList) ],
    "Term.Anonymous"         => [ "TRef(RTAnonymous(SmAnonymousRT",      \(\&Void) ],
    "Term.ApplyUnary"        => [ "TRef(RTRef(SmApplyUnaryRT",           \(\&NameT), \(\&Term) ],
    "Term.Name"              => [ "TRef(RTName(SmNameT",                 \(\&Str) ],
    "Term.Select"            => [ "TRef(RTSelect(SmSelectRT",            \(\&Term), \(\&NameT) ],
    "Term.Super"             => [ "TRef(RTRef(SmSuperRT",                \(\&Name), \(\&Name) ],
    "Term.This"              => [ "TRef(RTRef(SmThisRT",                 \(\&Name) ]
};

my $ACT = { "Term.ArgClause" => [ "SmArgClauseT", \(\&TermList), \(\&ArgsTypeOpt) ] };

my $NAMET = { "Term.Name" => [ "SmNameT", \(\&Str) ] };

my $PARAMT = { "Term.Param" => [ "SmParamT", \(\&ModList), \(\&Name), \(\&DecltpeOpt), \(\&TermOpt) ] };

my $PCT = { "Term.ParamClause" => [ "SmParamClauseT", \(\&ParamTList), \(\&ParamsTypeOpt) ] };

my $REFT = {
    "Term.Anonymous"  => [ "RTAnonymous(SmAnonymousRT", \(\&Void) ],
    "Term.ApplyUnary" => [ "RTRef(SmApplyUnaryRT",      \(\&NameT), \(\&Term) ],
    "Term.Name"       => [ "RTName(SmNameT",            \(\&Str) ],
    "Term.Select"     => [ "RTSelect(SmSelectRT",       \(\&Term), \(\&NameT) ],
    "Term.Super"      => [ "RTRef(SmSuperRT",           \(\&Name), \(\&Name) ],
    "Term.This"       => [ "RTRef(SmThisRT",            \(\&Name) ]
};

my $TYPE_ = {
    "Lit.Boolean"           => [ "T'Lit(SmBooleanL",                      \(\&Raw) ],
    "Lit.Byte"              => [ "T'Lit(SmByteL",                         \(\&Raw) ],
    "Lit.Char"              => [ "T'Lit(SmCharL",                         \(\&Raw) ],
    "Lit.Double"            => [ "T'Lit(SmDoubleL",                       \(\&Raw) ],
    "Lit.Float"             => [ "T'Lit(SmFloatL",                        \(\&Raw) ],
    "Lit.Int"               => [ "T'Lit(SmIntL",                          \(\&Raw) ],
    "Lit.Long"              => [ "T'Lit(SmLongL",                         \(\&Raw) ],
    "Lit.Null"              => [ "T'Lit(SmNullL",                         \(\&Void) ],
    "Lit.Short"             => [ "T'Lit(SmShortL",                        \(\&Raw) ],
    "Lit.String"            => [ "T'Lit(SmStringL",                       \(\&Str) ],
    "Lit.Symbol"            => [ "T'Lit(SmSymbolL",                       \(\&Sym) ],
    "Lit.Unit"              => [ "T'Lit(SmUnitL",                         \(\&Void) ],
    "Type.And"              => [ "T'Type'(SmAndT'",                       \(\&Type_), \(\&Type_) ],
    "Type.Annotate"         => [ "T'Type'(SmAnnotateT'",                  \(\&Type_), \(\&AnnotList) ],
    "Type.AnonymousLambda"  => [ "T'Type'(SmAnonymousLambdaT'",           \(\&Type_) ],
    "Type.AnonymousName"    => [ "T'Type'(SmAnonymousNameT'",             \(\&Void) ],
    "Type.AnonymousParam"   => [ "T'Type'(SmAnonymousParamT'",            \(\&VariantOpt) ],
    "Type.Apply"            => [ "T'Apply(SmApplyT'",                     \(\&Type_),        \(\&ArgClauseT_) ],
    "Type.ApplyInfix"       => [ "T'ApplyInfix(SmApplyInfixT'",           \(\&Type_),        \(\&NameT_), \(\&Type_) ],
    "Type.Block"            => [ "T'Type'(SmBlockT'",                     \(\&Type_DefList), \(\&Type_) ],
    "Type.ByName"           => [ "T'Type'(SmByNameT'",                    \(\&Type_) ],
    "Type.ContextFunction"  => [ "T'ContextFunction(SmContextFunctionT'", \(\&FuncPC),    \(\&Type_) ],
    "Type.Existential"      => [ "T'Type'(SmExistentialT'",               \(\&Type_),     \(\&StatList) ],
    "Type.Function"         => [ "T'Function(SmFunctionT'",               \(\&FuncPC),    \(\&Type_) ],
    "Type.ImplicitFunction" => [ "T'Type'(SmImplicitFunctionT'",          \(\&Type_List), \(\&Type_) ],
    "Type.Lambda"           => [ "T'Lambda(SmLambdaT'",                   \(\&Type_PC),   \(\&Type_) ],
    "Type.Macro"            => [ "T'Macro(SmMacroT'",                     \(\&Term) ],
    "Type.Match"            => [ "T'Match(SmMatchT'",                     \(\&Type_),      \(\&Type_CaseList) ],
    "Type.Method"           => [ "T'Type'(SmMethodT'",                    \(\&TermPCList), \(\&Type_) ],
    "Type.Or"               => [ "T'Type'(SmOrT'",                        \(\&Type_),      \(\&Type_) ],
    "Type.PolyFunction"     => [ "T'PolyFunction(SmPolyFunctionT'",       \(\&Type_PC),    \(\&Type_) ],
    "Type.Refine"           => [ "T'Type'(SmRefineT'",                    \(\&Type_Opt),   \(\&StatList) ],
    "Type.Repeated"         => [ "T'Type'(SmRepeatedT'",                  \(\&Type_) ],
    "Type.PatWildcard"      => [ "T'Type'(SmPatWildcardT'",               \(\&Void) ],
    "Type.Tuple"            => [ "T'Tuple(SmTupleT'",                     \(\&Type_List) ],
    "Type.Var"              => [ "T'Var(SmVarT'",                         \(\&NameT_) ],
    "Type.With"             => [ "T'Type'(SmWithT'",                      \(\&Type_), \(\&Type_) ],
    "Type.Wildcard"         => [ "T'Type'(SmWildcardT'",                  \(\&Bounds_) ],
    "Type.Name"             => [ "T'Ref(RT'Name(SmNameT'",                \(\&Str) ],
    "Type.Project"          => [ "T'Ref(RT'Ref(SmProjectRT'",             \(\&Type_), \(\&NameT_) ],
    "Type.Select"           => [ "T'Ref(RT'Ref(SmSelectRT'",              \(\&RefT),  \(\&NameT_) ],
    "Type.Singleton"        => [ "T'Ref(RT'Ref(SmSingletonRT'",           \(\&RefT) ]
};

my $ACT_ = { "Type.ArgClause" => [ "SmArgClauseT'", \(\&Type_List) ] };

my $BOUNDS_ = { "Type.Bounds" => [ "SmBounds'", \(\&Type_Opt), \(\&Type_Opt) ] };

my $NAMET_ = { "Type.Name" => [ "SmNameT'", \(\&Str) ] };

my $PARAMT_ = { "Type.Param" => [ "SmParamT'", \(\&ModList), \(\&Name), \(\&Type_PC), \(\&Bounds_), \(\&Type_List), \(\&Type_List) ] };

my $PCT_ = { "Type.ParamClause" => [ "SmParamClauseT'", \(\&ParamT_List) ] };

my $REFT_ = {
    "Type.Name"      => [ "RT'Name(SmNameT'",      \(\&Str) ],
    "Type.Project"   => [ "RT'Ref(SmProjectRT'",   \(\&Type_), \(\&NameT_) ],
    "Type.Select"    => [ "RT'Ref(SmSelectRT'",    \(\&RefT),  \(\&NameT_) ],
    "Type.Singleton" => [ "RT'Ref(SmSingletonRT'", \(\&RefT) ]
};

my $FPC = { "Type.FuncParamClause" => [ "FuncParamClause'", \(\&Type_List) ] };

################################################################################################################################################################

sub Main {
    while (<STDIN>) {
        chomp;
        &Any(\"Source", $SOURCE, \"Main", \$_);
        print "\n"
    }
    0
}

exit &Main() if $#ARGV == -1;

$_ = join(" ", @ARGV);
&Any(\"Stat", $STAT, \"Main", \$_);

exit 0;

################################################################################################################################################################

sub Any {
    \my ($what, %any, $ctx, $in) = @_;

    foreach my $k (keys %any) {
        if ($in =~ /^\s*$k\s*[(]/) {
            my ($sm, @subs) = @{ $any{$k} };
            my $n = split /[(]/, $sm;
            print " (", $sm;
            $in =~ s/^\s*$k\s*[(]//;
            my $io = \$in;
            while (my ($i, $s) = each(@subs)) {
                $io = &${$s}(\"$ctx >> $what", $io);
                die "$what: no comma at index $i"
                  unless $i == $#subs || ${$io} =~ s/^\s*[,]//;
            }
            die "$what: no closing parenthesis"
              unless ${$io} =~ s/^\s*[)]//;
            print ")" x $n;
            return $io
        }
    }
    die "$what: unknown `$what' or no open parenthesis - in=|$in|"
}

# `Nil'
sub Nil {
    \my ($io) = @_;
    $io =~ s/^\s*Nil//;
    print " []";
    \$io
}

# Nil or List(nonempty, ...)
sub List {
    \my ($lst, $sub, $what, $in) = @_;
    return &Nil(\$in) if $in =~ /^\s*Nil\s*[,)]/;
    &Wrap("$what >> $lst", \$in, "List" => ("[", "]"), \$sub, "neither `Nil' nor `List(' encountered", ~0)
}

# `None'
sub None {
    \my ($io) = @_;
    $io =~ s/^\s*None//;
    print " Nothing";
    \$io
}

# None or Some(thing)
sub Opt {
    \my ($opt, $sub, $what, $in) = @_;
    return &None(\$in) if $in =~ /^\s*None\s*[,)]/;
    &Wrap("$what >> $opt", \$in, "Some" => ("Just", ""), \$sub, "neither `None' nor `Some(' encountered")
}

# everything
sub Raw {
    \my ($what, $in) = @_;
    die "$what: no raw data between parentheses"
      unless my ($raw, $out) = (split /^\s*([^)]+)\s*[)]/, $in)[ 1, 2 ];
    $out = ")" . $out;
    $raw =~ s/[dDfFlL]$// unless /^\s*0[xX]/;    # double, float, long
    print " $raw";
    \$out
}

# String literal
# https://stackoverflow.com/questions/18547501/regex-to-replace-all-string-literals-in-a-java-file/27225768#27225768
sub Str {
    \my ($what, $in) = @_;
    die "$what: no string literal"
      unless my ($lit, $out) = (split /^\s*("(?:\\[\\'"tnbfru01234567]|[^\\"])*?")/, $in)[ 1, 2 ];
    print " $lit";
    \$out
}

# Symbol(string)
sub Sym { &Wrap(@_) if push @_, "Symbol" => ("Symbol", ""), \(\&Str), "`Symbol' not encountered" }

# nothing
sub Void {
    \my ($what, $in) = @_;
    die "$what: no empty parentheses"
      unless my $out = ")" . (split /^\s*([)])/, $in)[2];
    \$out
}

sub Wrap {
    my ($what, $io, ($from, ($beg, $end)), $sub, $msg, $loop) = @_;
    $loop = 1 unless defined $loop;
    die "$what: $msg"
      unless ${$io} =~ s/^\s*$from\s*[(]//;
    print " ($beg";
    my $n = 0;
    while ($n < $loop) {
        if ($n > 0) {
            last if ${$io} =~ /^\s*[)]/;
            die "$what: no comma before element $n"
              unless ${$io} =~ s/^\s*[,]//;
            print ","
        }
        $io = &${$sub}(\$what, $io);
        $n++
    }
    die "$what: no closing parenthesis"
      unless ${$io} =~ s/^\s*[)]//;
    print "$end)";
    $io
}

## A #################################################################################################################################################

sub ArgsType { &Any(@_) if unshift @_, \"ArgsType", { "Mod.Using" => $MOD->{"Mod.Using"} } }

sub ArgsTypeOpt { &Opt(@_) if unshift @_, \"ArgsTypeOpt", \(\&ArgsType) }

## AC ################################################################################################################################################

sub ArgClauseT { &Any(@_) if unshift @_, \"ArgClauseT", $ACT }

sub ArgClauseTList { &List(@_) if unshift @_, \"ArgClauseTList", \(\&ArgClauseT) }

sub ArgClauseT_ { &Any(@_) if unshift @_, \"ArgClauseT_", $ACT_ }

## B #################################################################################################################################################

sub Bounds_ { &Any(@_) if unshift @_, \"Bounds_", $BOUNDS_ }

## C #################################################################################################################################################

sub Case { &Any(@_) if unshift @_, \"Case", $CASE }

sub CaseList { &List(@_) if unshift @_, \"CaseList", \(\&Case) }

sub Type_Case { &Any(@_) if unshift @_, \"Type_Case", $CASE }

sub Type_CaseList { &List(@_) if unshift @_, \"Type_CaseList", \(\&Type_Case) }

sub Ctor { &Any(@_) if unshift @_, \"Ctor", $CTOR }

## D #################################################################################################################################################

sub Decltpe { &Type_ }

sub DecltpeOpt { &Opt(@_) if unshift @_, \"DecltpeOpt", \(\&Decltpe) }

## E #################################################################################################################################################

sub Enum { &Any(@_) if unshift @_, \"Enum", $ENUM }

sub EnumList { &List(@_) if unshift @_, \"EnumList", \(\&Enum) }

## I #################################################################################################################################################

sub Importee { &Any(@_) if unshift @_, \"Importee", $IMPORTEE }

sub ImporteeList { &List(@_) if unshift @_, \"ImporteeList", \(\&Importee) }

sub Importer { &Any(@_) if unshift @_, \"Importer", $IMPORTER }

sub ImporterList { &List(@_) if unshift @_, \"ImporterList", \(\&Importer) }

sub Init { &Any(@_) if unshift @_, \"Init", $INIT }

sub InitList { &List(@_) if unshift @_, \"InitList", \(\&Init) }

## L #################################################################################################################################################

sub Lit { &Any(@_) if unshift @_, \"Lit", $LIT }

sub LitList { &List(@_) if unshift @_, \"LitList", \(\&Lit) }

## M #################################################################################################################################################

sub Mod { &Any(@_) if unshift @_, \"Mod", $MOD }

sub ModList { &List(@_) if unshift @_, \"ModList", \(\&Mod) }

sub Annot { &Any(@_) if unshift @_, \"Annot", { "Mod.Annot" => $MOD->{"Mod.Annot"} } }

sub AnnotList { &List(@_) if unshift @_, \"AnnotList", \(\&Annot) }

sub Variant {
    &Any(@_) if unshift @_, \"Variant",
      {
        "Mod.Covariant"     => $MOD->{"Mod.Covariant"},
        "Mod.Contravariant" => $MOD->{"Mod.Contravariant"}
      }
}

sub VariantOpt { &Opt(@_) if unshift @_, \"VariantOpt", \(\&Variant) }

## N #################################################################################################################################################

sub Name { &Any(@_) if unshift @_, \"Name", $NAME }

sub NameT { &Any(@_) if unshift @_, \"NameT", $NAMET }

sub NameT_ { &Any(@_) if unshift @_, \"NameT_", $NAMET_ }

## P #################################################################################################################################################

sub Pat { &Any(@_) if unshift @_, \"Pat", $PAT }

sub PatList { &List(@_) if unshift @_, \"PatList", \(\&Pat) }

## PC ############################################################################################################################

sub ParamsType {
    &Any(@_) if unshift @_, \"ParamsType",
      {
        "Mod.Implicit" => $MOD->{"Mod.Implicit"},
        "Mod.Using"    => $MOD->{"Mod.Using"}
      }
}

sub ParamsTypeOpt { &Opt(@_) if unshift @_, \"ParamsTypeOpt", \(\&ParamsType) }

sub FuncParamClause { &Any(@_) if unshift @_, \"FuncPC", $FPC }

sub GroupPC { &Any(@_) if unshift @_, \"GroupPC", $GPC }

sub GroupPCList { &List(@_) if unshift @_, \"GroupPCList", \(\&GroupPC) }

sub GroupPCOpt { &Opt(@_) if unshift @_, \"GroupPCOpt", \(\&GroupPC) }

sub ParamT { &Any(@_) if unshift @_, \"ParamT", $PARAMT }

sub ParamTList { &List(@_) if unshift @_, \"ParamTList", \(\&ParamT) }

sub TermPC { &Any(@_) if unshift @_, \"TermPC", $PCT }

sub TermPCList { &List(@_) if unshift @_, \"TermPCList", \(\&TermPC) }

sub ParamT_ { &Any(@_) if unshift @_, \"ParamT_", $PARAMT_ }

sub ParamT_List { &List(@_) if unshift @_, \"ParamT_List", \(\&ParamT_) }

sub Type_PC { &Any(@_) if unshift @_, \"Type_PC", $PCT_ }

sub Type_PCList { &List(@_) if unshift @_, \"Type_PCList", \(\&Type_PC) }

## R #################################################################################################################################################

sub Ref { &Any(@_) if unshift @_, \"Ref", $REF }

sub RefT { &Any(@_) if unshift @_, \"RefT", $REFT }

## S #################################################################################################################################################

sub Self { &Any(@_) if unshift @_, \"Self", $SELF }

sub Stat { &Any(@_) if unshift @_, \"Stat", $STAT }

sub StatList { &List(@_) if unshift @_, \"StatList", \(\&Stat) }

## T #################################################################################################################################################

sub Templ { &Any(@_) if unshift @_, \"Templ", $TEMPLATE }

sub Term { &Any(@_) if unshift @_, \"Term", $TERM }

sub TermList { &List(@_) if unshift @_, \"TermList", \(\&Term) }

sub TermOpt { &Opt(@_) if unshift @_, \"TermOpt", \(\&Term) }

sub Type_Def {
    &Any(@_)
      if unshift @_, \"Type_Def",
      {
        "Decl.Type" => [ "T'DType'(SmType'S", \(\&ModList), \(\&NameT_), \(\&Type_PC), \(\&Bounds_) ],
        "Defn.Type" => [ "T'DType(SmTypeS",   \(\&ModList), \(\&NameT_), \(\&Type_PC), \(\&Type_), \(\&Bounds_) ]
      }
}

sub Type_DefList { &List(@_) if unshift @_, \"Type_DefList", \(\&Type_Def) }

sub Type_ { &Any(@_) if unshift @_, \"Type_", $TYPE_ }

sub Type_List { &List(@_) if unshift @_, \"Type_List", \(\&Type_) }

sub Type_Opt { &Opt(@_) if unshift @_, \"Type_Opt", \(\&Type_) }
