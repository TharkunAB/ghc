TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk
SHELL = /bin/bash

HC_BASE = '$(TEST_HC)' $(TEST_HC_OPTS) -package-env -
DEPENDENT_BASE_DIR ?= /home/tritlo/Code/dependent-base
DEPENDENT_OBS_EQ_DIR ?= /home/tritlo/Code/dependent-obs-eq

.PHONY: DepRecompHomeRules DepRecompSourceUpgrade DepRecompClosedEval \
        DepIfaceBinaryMutate DepInstalledBootIfaceDb DepPackageFacadeDb \
        DepPluginsPackageDb

# 1. Home-rule interface-hash invalidation, scoped dictionary Core reload,
#    and type-lambda hidden family / record-selector recompilation.
DepRecompHomeRules:
	@set -euo pipefail; \
	for mode in code nocode; do \
	  mkdir -p "hr_$$mode"; \
	  cp recomp_home_rules/FoldProvider.hs recomp_home_rules/RuleHelper.hs \
	     recomp_home_rules/RuleBridge.hs recomp_home_rules/SafeConsumer.hs \
	     recomp_home_rules/RuleProof.hs "hr_$$mode/"; \
	  ( cd "hr_$$mode"; \
	    flags="-no-link"; \
	    if [ "$$mode" = nocode ]; then flags="-fno-code -fwrite-interface"; fi; \
	    $(HC_BASE) -O2 -v1 --make $$flags RuleProof.hs SafeConsumer.hs > initial.log 2>&1; \
	    $(HC_BASE) -O2 -v1 --make $$flags RuleProof.hs SafeConsumer.hs > noop.log 2>&1; \
	    ! grep -Fq 'Compiling ' noop.log; \
	    cp SafeConsumer.hi safe-before.hi; \
	    sed 's/NOINLINE private/INLINE private/' RuleHelper.hs > RuleHelper.tmp && mv RuleHelper.tmp RuleHelper.hs; \
	    $(HC_BASE) -O2 -v1 --make $$flags RuleProof.hs SafeConsumer.hs > changed.log 2>&1; \
	    grep -Eq 'Compiling RuleHelper .*Source file changed' changed.log; \
	    grep -Eq 'Compiling RuleProof .*RuleHelper changed \(raw\)' changed.log; \
	    ! grep -Eq 'Compiling (RuleBridge|SafeConsumer|FoldProvider)' changed.log; \
	    cmp -s safe-before.hi SafeConsumer.hi; \
	    if [ "$$mode" = code ]; then \
	      printf '\n{-# RULES "late/home-fold-empty" [0] fold lateStep False [] = True #-}\n' >> RuleHelper.hs; \
	      ! $(HC_BASE) -O2 -v1 --make $$flags RuleProof.hs SafeConsumer.hs > rule.log 2>&1; \
	      grep -Eq 'RuleProof.hs:.*error: \[GHC-83605\]' rule.log; \
	    fi ); \
	done
	@set -euo pipefail; \
	mkdir -p sd_prov sd_cli sd_inc sd_bc_prov sd_bc_cli sd_reify; \
	cp recomp_home_rules/Provider.hs sd_prov/; \
	( cd sd_prov && $(HC_BASE) $(ghcThWayFlags) -v0 -O2 -DSCOPED_DICTIONARY_EXPORT -fwrite-if-simplified-core -c Provider.hs ); \
	cp sd_prov/Provider.hi recomp_home_rules/CompileTime.hs recomp_home_rules/ScopedMain.hs sd_cli/; \
	( cd sd_cli && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -fprefer-byte-code -fignore-interface-pragmas -c CompileTime.hs && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -c ScopedMain.hs && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 ScopedMain.o CompileTime.o -o run && \
	  test "$$(./run)" = '((True,False),(False,True),(True,()))' ); \
	cp sd_prov/Provider.hi recomp_home_rules/ScopedReify.hs sd_reify/; \
	( cd sd_reify && ! $(HC_BASE) $(ghcThWayFlags) -v0 -fno-code -c ScopedReify.hs > reify.log 2>&1 && grep -Fq '[GHC-75721]' reify.log ); \
	cp recomp_home_rules/Provider.hs recomp_home_rules/CompileTime.hs recomp_home_rules/ScopedMain.hs sd_inc/; \
	( cd sd_inc && \
	  printf ':main\n:quit\n' | $(HC_BASE) --interactive -ignore-dot-ghci -fbyte-code -fwrite-if-simplified-core -fprefer-byte-code -DSCOPED_DICTIONARY_EXPORT -v3 -ddump-if-trace ScopedMain.hs > first.log 2>&1 && \
	  grep -Fq '((True,False),(False,True),(True,()))' first.log && \
	  sed 's/opposite True, opposite False/opposite False, opposite True/' CompileTime.hs > CompileTime.tmp && mv CompileTime.tmp CompileTime.hs && \
	  printf ':main\n:quit\n' | $(HC_BASE) --interactive -ignore-dot-ghci -fbyte-code -fwrite-if-simplified-core -fprefer-byte-code -DSCOPED_DICTIONARY_EXPORT -v3 -ddump-if-trace ScopedMain.hs > second.log 2>&1 && \
	  grep -Eq 'Skipping +Provider' second.log && \
	  grep -Fq 'Generating ByteCode for Provider' second.log && \
	  grep -Fq '((False,True),(True,False),(True,()))' second.log ); \
	cp recomp_home_rules/Provider.hs sd_bc_prov/; \
	( cd sd_bc_prov && $(HC_BASE) $(ghcThWayFlags) -v0 -DSCOPED_DICTIONARY_EXPORT -fbyte-code -fwrite-byte-code -fwrite-interface -c Provider.hs ); \
	cp sd_bc_prov/Provider.hi sd_bc_prov/Provider.gbc recomp_home_rules/CompileTime.hs recomp_home_rules/ScopedMain.hs sd_bc_cli/; \
	( cd sd_bc_cli && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -fprefer-byte-code -c CompileTime.hs && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -c ScopedMain.hs && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 ScopedMain.o CompileTime.o -o run && \
	  test "$$(./run)" = '((True,False),(False,True),(True,()))' )
	@set -euo pipefail; \
	mkdir -p tl_recomp; \
	cp TypeLambda.hs NativeTypeLambdaScope.hs SchemaColumns.hs tl_recomp/; \
	cp recomp_home_rules/TypeLambdaOrdinary.hs tl_recomp/Ordinary.hs; \
	( cd tl_recomp && \
	  $(HC_BASE) -v0 -fomit-interface-pragmas --make -no-link Ordinary.hs && \
	  $(HC_BASE) -v1 -fomit-interface-pragmas --make -no-link Ordinary.hs > warm.log 2>&1 && \
	  ! grep -Fq 'Compiling ' warm.log && \
	  sed 's/ -> Either a b)/ -> Either b a)/' ../TypeLambda.hs > TypeLambda.hs && \
	  ! $(HC_BASE) -v1 -fomit-interface-pragmas --make -no-link Ordinary.hs > changed.log 2>&1 && \
	  grep -Fq 'Compiling Ordinary' changed.log && \
	  cp ../TypeLambda.hs TypeLambda.hs && \
	  $(HC_BASE) -v0 -fomit-interface-pragmas --make -no-link Ordinary.hs && \
	  sed '/{ checkedCells ::/s/Either e a/Either a e/' ../TypeLambda.hs > TypeLambda.hs && \
	  ! $(HC_BASE) -v1 -fomit-interface-pragmas --make -no-link Ordinary.hs > field.log 2>&1 && \
	  grep -Fq '[GHC-25897]' field.log )

# 2. Source-equation & rule interface upgrades across separate compilation and --make.
DepRecompSourceUpgrade:
	@set -euo pipefail; \
	mkdir -p ch_prov ch_exp ch_cli ch_fwd ch_rules; \
	cp CharacterCase.hs ch_prov/; \
	cp recomp_source_upgrade/CharForward.hs ch_exp/Forward.hs; \
	cp recomp_source_upgrade/CharAliasExport.hs ch_exp/AliasExport.hs; \
	cp CharacterExport.hs ch_exp/; \
	cp NativeOpenCharFamily.hs ch_cli/; \
	cp recomp_source_upgrade/CharProof.hs ch_cli/Proof.hs; \
	( cd ch_prov && $(HC_BASE) -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c CharacterCase.hs ); \
	cp ch_prov/CharacterCase.hi ch_exp/; \
	( cd ch_exp && $(HC_BASE) -v0 -O0 -i -i. -fomit-interface-pragmas -fno-write-if-simplified-core -c Forward.hs CharacterExport.hs AliasExport.hs ); \
	cp ch_exp/*.hi ch_cli/; \
	( cd ch_cli && \
	  $(HC_BASE) -v0 -O0 -i -i. -c NativeOpenCharFamily.hs Proof.hs && \
	  $(HC_BASE) -v0 -O0 NativeOpenCharFamily.o ../ch_exp/CharacterExport.o ../ch_prov/CharacterCase.o -o run && \
	  test "$$(./run)" = '[88,201,33]' && \
	  $(HC_BASE) -v1 -O0 -i -i. -c NativeOpenCharFamily.hs > reuse.log 2>&1 && \
	  grep -Fxq 'compilation IS NOT required' reuse.log ); \
	sed -e "s/upper 'x' = 'X'/upper 'x' = 'Y'/" -e "s/flag 'x' = True/flag 'x' = False/" CharacterCase.hs > ch_prov/CharacterCase.hs; \
	( cd ch_prov && $(HC_BASE) -v0 -O0 -fforce-recomp -fomit-interface-pragmas -fno-write-if-simplified-core -c CharacterCase.hs ); \
	cp ch_prov/CharacterCase.hi ch_cli/; \
	( cd ch_cli && ! $(HC_BASE) -v0 -O0 -i -i. -fomit-interface-pragmas -fno-write-if-simplified-core -c Proof.hs > changed.log 2>&1 && { grep -Fq '[GHC-83865]' changed.log || { cat changed.log >&2; exit 1; }; } ); \
	cp CharacterCase.hs NativeOpenCharFamily.hs ch_fwd/; \
	cp recomp_source_upgrade/CharForward.hs ch_fwd/Forward.hs; \
	sed -e 's/import qualified CharacterCase/import qualified Forward/' -e 's/CharacterCase.upper/Forward.convert/' CharacterExport.hs > ch_fwd/CharacterExport.hs; \
	( cd ch_fwd && \
	  $(HC_BASE) -v0 -O0 -i -i. --make NativeOpenCharFamily.hs -o run && \
	  test "$$(./run)" = '[88,201,33]' && \
	  sed "s/upper 'x' = 'X'/upper 'x' = 'Y'/" ../CharacterCase.hs > CharacterCase.hs && \
	  $(HC_BASE) -v0 -O0 -i -i. --make NativeOpenCharFamily.hs -o run && \
	  test "$$(./run)" = '[89,201,33]' ); \
	cp CharacterCase.hs CharacterExport.hs ch_rules/; \
	printf '\n{-# NOINLINE upper #-}\n' >> ch_rules/CharacterCase.hs; \
	cp recomp_source_upgrade/CharOrphan.hs ch_rules/Orphan.hs; \
	sed '/^import CharacterExport/a import Orphan ()' NativeOpenCharFamily.hs > ch_rules/Client.hs; \
	( cd ch_rules && ! $(HC_BASE) -v0 -O1 -i -i. -fno-omit-interface-pragmas -fno-ignore-interface-pragmas --make Client.hs -o stale > invalid.log 2>&1 && grep -Fq '[GHC-83605]' invalid.log )
	@set -euo pipefail; \
	mkdir -p mp_recomp mp_cons mp_cons_cli mp_comp mp_comp_cli; \
	cp PreludeMapLengthSource.hs recomp_source_upgrade/CountOne.hs mp_recomp/; \
	cp PreludeMapLengthSize.hs mp_recomp/PreludeMapLengthSize.hs; \
	( cd mp_recomp && \
	  $(HC_BASE) -v0 -O1 -fomit-interface-pragmas -fno-code -fwrite-interface --make CountOne.hs && \
	  sed 's/= Succ (size xs)$$/= Succ (Succ (size xs))/' ../PreludeMapLengthSize.hs > PreludeMapLengthSize.hs && \
	  ! $(HC_BASE) -v0 -O1 -fomit-interface-pragmas -fno-code -fwrite-interface --make CountOne.hs > changed.log 2>&1 && \
	  grep -Fq '[GHC-83865]' changed.log && \
	  sed -e '1s/DependentTypes, ListTuplePuns/DataKinds, NoDependentTypes, TypeOperators/' -e '/^{-# OPTIONS_GHC -fwrite-source-equations #-}/d' ../PreludeMapLengthSize.hs > PreludeMapLengthSize.hs && \
	  ! $(HC_BASE) -v0 -O1 -fomit-interface-pragmas -fno-code -fwrite-interface --make CountOne.hs > withdrawn.log 2>&1 && \
	  grep -Fq '[GHC-70566]' withdrawn.log && \
	  cp ../PreludeMapLengthSize.hs PreludeMapLengthSize.hs && \
	  $(HC_BASE) -v0 -O1 -fomit-interface-pragmas -fno-code -fwrite-interface --make CountOne.hs ); \
	cp NativeOpenInstalledMapProducer.hs PreludeMapSource.hs mp_cons/; \
	sed 's/import qualified Prelude as P/import qualified NativeOpenInstalledMapProducer as W\nimport qualified Prelude as P/; s/P\.map/W.mapper/g' PreludeMapSource.hs > mp_cons/PreludeMapSource.hs; \
	( cd mp_cons && $(HC_BASE) -v0 -O1 -fenable-rewrite-rules -DCONS_RULE -c NativeOpenInstalledMapProducer.hs PreludeMapSource.hs ); \
	cp mp_cons/*.hi recomp_source_upgrade/ConsProof.hs mp_cons_cli/; \
	( cd mp_cons_cli && $(HC_BASE) -v0 -O1 -i -i. -fenable-rewrite-rules -c ConsProof.hs ); \
	( cd mp_cons && $(HC_BASE) -v0 -O1 -fenable-rewrite-rules -DCONS_RULE -DFORCE_HEAD -fforce-recomp -c NativeOpenInstalledMapProducer.hs ); \
	cp mp_cons/NativeOpenInstalledMapProducer.hi mp_cons_cli/; \
	( cd mp_cons_cli && ! $(HC_BASE) -v0 -O1 -i -i. -fenable-rewrite-rules -c ConsProof.hs > changed.log 2>&1 && grep -Fq '[GHC-83605]' changed.log ); \
	cp NativeOpenInstalledMapProducer.hs recomp_source_upgrade/TwiceSource.hs mp_comp/; \
	( cd mp_comp && $(HC_BASE) -v0 -O1 -fenable-rewrite-rules -DCOMPOSE_RULE -c NativeOpenInstalledMapProducer.hs TwiceSource.hs ); \
	cp mp_comp/*.hi recomp_source_upgrade/TwiceProof.hs mp_comp_cli/; \
	( cd mp_comp_cli && $(HC_BASE) -v0 -O1 -i -i. -fenable-rewrite-rules -c TwiceProof.hs ); \
	( cd mp_comp && $(HC_BASE) -v0 -O1 -fenable-rewrite-rules -DCOMPOSE_RULE -DFORCE_INTERMEDIATE -fforce-recomp -c NativeOpenInstalledMapProducer.hs ); \
	cp mp_comp/NativeOpenInstalledMapProducer.hi mp_comp_cli/; \
	( cd mp_comp_cli && ! $(HC_BASE) -v0 -O1 -i -i. -fenable-rewrite-rules -c TwiceProof.hs > changed.log 2>&1 && grep -Fq '[GHC-83605]' changed.log )
	@set -euo pipefail; \
	mkdir -p lo_prov lo_cli lo_stale; \
	cp NativeLocalOpenProducer.hs lo_prov/; \
	cp NativeLocalOpen.hs lo_cli/; \
	( cd lo_prov && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -c NativeLocalOpenProducer.hs ); \
	cp lo_prov/NativeLocalOpenProducer.hi lo_cli/; \
	( cd lo_cli && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i. -c NativeLocalOpen.hs ); \
	sed 's/^select x _ = x$$/select _ y = y/' NativeLocalOpenProducer.hs > lo_prov/NativeLocalOpenProducer.hs; \
	( cd lo_prov && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fforce-recomp -c NativeLocalOpenProducer.hs ); \
	cp lo_prov/NativeLocalOpenProducer.hi lo_cli/; \
	( cd lo_cli && ! $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i. -c NativeLocalOpen.hs > changed.log 2>&1 && grep -Fq 'GHC-83865' changed.log ); \
	cp recomp_source_upgrade/StaleOpen1.hs lo_stale/NativeLocalOpenStale.hs; \
	( cd lo_stale && \
	  $(HC_BASE) -v0 -O0 -c NativeLocalOpenStale.hs && \
	  test -s NativeLocalOpenStale.o && \
	  cp ../recomp_source_upgrade/StaleOpen2.hs NativeLocalOpenStale.hs && \
	  $(HC_BASE) -v0 -O0 -fno-code NativeLocalOpenStale.hs && \
	  cp ../recomp_source_upgrade/StaleOpenBad.hs NativeLocalOpenStale.hs && \
	  ! $(HC_BASE) -v0 -O0 -fforce-recomp -c NativeLocalOpenStale.hs > bad.log 2>&1 && \
	  grep -Fq '[GHC-83865]' bad.log )
	@set -euo pipefail; \
	mkdir -p ie_prov ie_cli; \
	for level in 1 2 3; do \
	  $(HC_BASE) -v0 -O0 -fwrite-if-compression=$$level -fforce-recomp -c implicit-existential-source/MixedModes.hs -outputdir ie_prov; \
	done; \
	$(HC_BASE) -v0 -O0 -c implicit-existential-source/Vector.hs -outputdir ie_prov; \
	$(HC_BASE) -v0 -O0 -i -iie_prov -fwrite-ide-info -c implicit-existential-source/BarProvider.hs -outputdir ie_prov; \
	$(HC_BASE) -v0 -O0 -i -iie_prov -fwrite-ide-info -c implicit-existential-source/DotHie.hs -outputdir ie_prov; \
	test -s ie_prov/BarProvider.hie && test -s ie_prov/DotHie.hie; \
	$(HC_BASE) -v0 -O2 -fno-omit-interface-pragmas -c implicit-existential-source/LexicalLetProvider.hs -outputdir ie_prov; \
	$(HC_BASE) -v0 -O2 -fno-omit-interface-pragmas -i -iie_prov -c implicit-existential-source/LexicalLinearRule.hs -outputdir ie_prov; \
	cp ie_prov/LexicalLetProvider.hi ie_prov/LexicalLinearRule.hi implicit-existential-source/LexicalLinearRuleMain.hs ie_cli/; \
	( cd ie_cli && \
	  $(HC_BASE) -v0 -O2 -i -i. -fno-omit-interface-pragmas -ddebug-output -ddump-rule-firings -c LexicalLinearRuleMain.hs > firings.log 2>&1 && \
	  test "$$(grep -Fc 'Rule fired: read/linear-let (LexicalLinearRule)' firings.log)" = 2 && \
	  $(HC_BASE) -v0 -O2 LexicalLinearRuleMain.o ../ie_prov/LexicalLinearRule.o ../ie_prov/LexicalLetProvider.o -o run && \
	  test "$$(./run)" = '(5,9)' )

# 3. Closed-eval object/BCO dependency tracking and stale linkable invalidation.
DepRecompClosedEval:
	@set -euo pipefail; \
	mkdir -p rp_orig rp_chg rp_lib rp_cli; \
	cp NativeClosedReturnedPairLib.hs rp_orig/; \
	sed 's/^pair n = (n + 1, n)$$/pair n = (n + 2, n)/' NativeClosedReturnedPairLib.hs > rp_chg/NativeClosedReturnedPairLib.hs; \
	( cd rp_orig && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeClosedReturnedPairLib.hs ); \
	( cd rp_chg && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeClosedReturnedPairLib.hs ); \
	cp rp_orig/*.hi rp_orig/*.o rp_lib/; \
	cp NativeClosedReturnedPair.hs rp_cli/; \
	( cd rp_cli && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i../rp_lib -c NativeClosedReturnedPair.hs && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 NativeClosedReturnedPair.o ../rp_lib/NativeClosedReturnedPairLib.o -o run && \
	  test "$$(./run)" = '(2,3)' && \
	  $(HC_BASE) $(ghcThWayFlags) -v1 -O0 -i -i../rp_lib -c NativeClosedReturnedPair.hs > reuse.log 2>&1 && \
	  grep -Fxq 'compilation IS NOT required' reuse.log ); \
	cp rp_chg/NativeClosedReturnedPairLib.o rp_lib/NativeClosedReturnedPairLib.o; \
	( cd rp_cli && ! $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i../rp_lib -c NativeClosedReturnedPair.hs > changed.log 2>&1 && grep -Fq "Couldn't match type" changed.log )
	@set -euo pipefail; \
	mkdir -p pp_prod pp_if; \
	cp recomp_closed_eval/Builder.hs NativeClosedFamilyProjection.hs pp_prod/; \
	( cd pp_prod && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c Builder.hs NativeClosedFamilyProjection.hs ); \
	cp pp_prod/*.hi pp_prod/Builder.o recomp_closed_eval/Alias.hs recomp_closed_eval/Executable.hs pp_if/; \
	( cd pp_if && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i. -c Alias.hs Executable.hs ); \
	sed 's/^defined n _ = (n, n + 1)$$/defined n _ = (n, n + 2)/' recomp_closed_eval/Builder.hs > pp_prod/Builder.hs; \
	( cd pp_prod && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fforce-recomp -fomit-interface-pragmas -fno-write-if-simplified-core -c Builder.hs ); \
	cp pp_prod/Builder.hi pp_prod/Builder.o pp_if/; \
	( cd pp_if && \
	  $(HC_BASE) $(ghcThWayFlags) -v1 -O0 -i -i. -c Alias.hs > reuse.log 2>&1 && \
	  grep -Fq 'compilation IS NOT required' reuse.log && \
	  ! $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i. -c Executable.hs > err.log 2>&1 && \
	  grep -Eq 'GHC-(83865|25897)' err.log )
	@set -euo pipefail; \
	mkdir -p nd_orig nd_chg nd_inner nd_outer; \
	cp NativeClosedListsLib.hs NativeOpenRetryLib.hs nd_orig/; \
	sed 's/^reverseNaturals = reverse$$/reverseNaturals = drop 1/' NativeClosedListsLib.hs > nd_chg/NativeClosedListsLib.hs; \
	sed 's/^total = sum$$/total = (1 +) . sum/' NativeOpenRetryLib.hs > nd_chg/NativeOpenRetryLib.hs; \
	( cd nd_orig && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeClosedListsLib.hs NativeOpenRetryLib.hs ); \
	( cd nd_chg && $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeClosedListsLib.hs NativeOpenRetryLib.hs ); \
	mkdir -p nd_inner/lib nd_outer/lib; \
	cp nd_orig/*.hi nd_orig/*.o nd_inner/lib/; \
	cp NativeClosedNestedDependencies.hs nd_inner/; \
	( cd nd_inner && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -ilib -c NativeClosedNestedDependencies.hs && \
	  $(HC_BASE) $(ghcThWayFlags) -v1 -O0 -i -ilib -c NativeClosedNestedDependencies.hs > reuse.log 2>&1 && \
	  grep -Fxq 'compilation IS NOT required' reuse.log && \
	  cp ../nd_chg/NativeClosedListsLib.o lib/NativeClosedListsLib.o && \
	  ! $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -ilib -c NativeClosedNestedDependencies.hs > err.log 2>&1 && \
	  grep -Eq 'GHC-(83865|18872)' err.log ); \
	cp nd_orig/*.hi nd_orig/*.o nd_outer/lib/; \
	cp NativeClosedNestedDependencies.hs nd_outer/; \
	( cd nd_outer && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -ilib -c NativeClosedNestedDependencies.hs && \
	  cp ../nd_chg/NativeOpenRetryLib.o lib/NativeOpenRetryLib.o && \
	  ! $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -ilib -c NativeClosedNestedDependencies.hs > err.log 2>&1 && \
	  grep -Eq 'GHC-(83865|18872)' err.log )
	@set -euo pipefail; \
	mkdir -p sd_sum; \
	cp NativeClosedSumDependencies.hs NativeClosedSumDependenciesHelper.hs NativeClosedSumDependenciesInstance.hs sd_sum/; \
	( cd sd_sum && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O1 -fno-ignore-interface-pragmas -fenable-rewrite-rules -fno-write-if-simplified-core --make -no-link NativeClosedSumDependencies.hs -ddump-rule-firings > rules.log 2>&1 && \
	  grep -Fq 'Class op pick' rules.log && \
	  $(HC_BASE) $(ghcThWayFlags) -v2 -O1 -fno-ignore-interface-pragmas -fenable-rewrite-rules -fno-write-if-simplified-core --make -no-link NativeClosedSumDependencies.hs > skip.log 2>&1 && \
	  grep -Eq 'Skipping[[:space:]]+NativeClosedSumDependencies' skip.log && \
	  sed 's/first/second/g' ../NativeClosedSumDependenciesInstance.hs > NativeClosedSumDependenciesInstance.hs && \
	  ! $(HC_BASE) $(ghcThWayFlags) -v0 -O1 -fno-ignore-interface-pragmas -fenable-rewrite-rules -fno-write-if-simplified-core --make -no-link NativeClosedSumDependencies.hs > changed.log 2>&1 && \
	  grep -Fq 'Expected: NatTag 11' changed.log )
	@set -euo pipefail; \
	mkdir -p opt_rb opt_rb_cli sm_stale; \
	cp recomp_closed_eval/Value.hs recomp_closed_eval/Rule.hs recomp_closed_eval/Producer.hs opt_rb/; \
	cp recomp_closed_eval/OptMain.hs opt_rb/Main.hs; \
	( cd opt_rb && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O1 -fomit-interface-pragmas -fno-write-if-simplified-core -ddump-rule-firings -ddump-to-file --make Main.hs -o run && \
	  grep -Fq 'Rule fired: score/empty (Rule)' Producer.dump-rule-firings && \
	  test "$$(./run)" = '(29,36)' ); \
	cp opt_rb/*.hi opt_rb/*.o NativeClosedFamilyProjection.hs recomp_closed_eval/OptProof.hs opt_rb_cli/; \
	( cd opt_rb_cli && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i. -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeClosedFamilyProjection.hs OptProof.hs && \
	  sed 's/:~: 36$$/:~: 14/' OptProof.hs > Stale.hs && \
	  ! $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i. -fomit-interface-pragmas -fno-write-if-simplified-core -fforce-recomp -c Stale.hs > stale.log 2>&1 && \
	  grep -Fq 'GHC-83865' stale.log ); \
	cp recomp_closed_eval/SameModStale1.hs sm_stale/StaleMod.hs; \
	( cd sm_stale && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -c StaleMod.hs && \
	  cp ../recomp_closed_eval/SameModStale2.hs StaleMod.hs && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fno-code StaleMod.hs )

# 4. Binary interface mutation and warm closed-family rule invalidation.
DepIfaceBinaryMutate:
	@set -euo pipefail; \
	libdir="$$('$(TEST_HC)' --print-libdir)"; \
	mkdir -p df_mut; \
	cp NativeDepFunCoMutate.hs df_mut/; \
	$(HC_BASE) $(ghcThWayFlags) -v0 -O1 -package ghc df_mut/NativeDepFunCoMutate.hs -outputdir df_mut -o df_mut/mutate; \
	for comp in 1 2; do \
	  mkdir -p "df_$$comp/client"; \
	  cp NativeDepFunCoProvider.hs "df_$$comp/"; \
	  ( cd "df_$$comp" && $(HC_BASE) -v0 -O1 -fwrite-if-compression=$$comp -c NativeDepFunCoProvider.hs ); \
	  cp "df_$$comp/NativeDepFunCoProvider.hi" NativeDepFunCoClient.hs NativeDepFunCoWire.hs "df_$$comp/client/"; \
	  ( cd "df_$$comp/client" && \
	    $(HC_BASE) -v0 -O1 -i -i. -fomit-interface-pragmas -fno-write-if-simplified-core -ddump-inlinings -c NativeDepFunCoClient.hs > inl.log 2>&1 && \
	    grep -Fq 'Inlining done: NativeDepFunCoProvider.transport' inl.log && \
	    $(HC_BASE) -v0 -O1 -i -i. -c NativeDepFunCoWire.hs && \
	    $(HC_BASE) -v0 -O1 NativeDepFunCoClient.o NativeDepFunCoWire.o ../NativeDepFunCoProvider.o -o run && \
	    test "$$(./run)" = 9 ); \
	done; \
	for mut in role scope; do \
	  mkdir -p "df_$$mut"; \
	  ./df_mut/mutate "$$libdir" "$$mut" df_2/NativeDepFunCoProvider.hi "df_$$mut/NativeDepFunCoProvider.hi"; \
	  cp NativeDepFunCoClient.hs "df_$$mut/"; \
	  ( cd "df_$$mut" && \
	    ! $(HC_BASE) -v0 -O1 -i -i. -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeDepFunCoClient.hs > err.log 2>&1 && \
	    if [ "$$mut" = role ]; then grep -Fq 'Expected nominal evidence.' err.log; else grep -Fq 'Iface id out of scope:' err.log; fi ); \
	done
	@set -euo pipefail; \
	libdir="$$('$(TEST_HC)' --print-libdir)"; \
	mkdir -p gf_prov gf_leg gf_read gf_warm; \
	cp PackedVecLibrary.hs NativeNonuniformGadtFieldsProvider.hs gf_prov/; \
	( cd gf_prov && $(HC_BASE) -v0 -O2 -c PackedVecLibrary.hs && $(HC_BASE) -v0 -O2 -DLEGACY_ONLY -outputdir ../gf_leg -c NativeNonuniformGadtFieldsProvider.hs ); \
	cp NativeLegacyGadtFieldsMutate.hs gf_leg/; \
	$(HC_BASE) $(ghcThWayFlags) -v0 -O0 -package ghc gf_leg/NativeLegacyGadtFieldsMutate.hs -outputdir gf_leg -o gf_leg/mutate; \
	cp gf_prov/PackedVecLibrary.hi gf_leg/NativeNonuniformGadtFieldsProvider.hi iface_binary_mutate/LegacyReader.hs gf_read/; \
	( cd gf_read && \
	  $(HC_BASE) -v0 -O2 -i -i. -ddump-inlinings -c LegacyReader.hs > valid.log 2>&1 && \
	  grep -Fq 'Inlining done: NativeNonuniformGadtFieldsProvider.inspectSwap' valid.log ); \
	./gf_leg/mutate "$$libdir" gf_leg/NativeNonuniformGadtFieldsProvider.hi gf_read/NativeNonuniformGadtFieldsProvider.hi; \
	( cd gf_read && ! $(HC_BASE) -v0 -O2 -i -i. -fforce-recomp -c LegacyReader.hs > dup.log 2>&1 && grep -Fq 'Missing or unsupported constructor proof in interface alternative: Next' dup.log ); \
	cp PackedVecLibrary.hs iface_binary_mutate/StepSource.hs iface_binary_mutate/NestedProvider.hs iface_binary_mutate/WarmClientMethod.hs iface_binary_mutate/WarmClientStep.hs gf_warm/; \
	cp iface_binary_mutate/WarmRules1.hs gf_warm/WarmRules.hs; \
	( cd gf_warm && \
	  $(HC_BASE) -v1 -O2 --make -no-link WarmClientMethod.hs WarmClientStep.hs > init.log 2>&1 && \
	  $(HC_BASE) -v1 -O2 --make -no-link WarmClientMethod.hs WarmClientStep.hs > noop.log 2>&1 && \
	  ! grep -Fq 'Compiling ' noop.log && \
	  cp ../iface_binary_mutate/WarmRules2.hs WarmRules.hs && \
	  ! $(HC_BASE) -v1 -O2 --make -no-link WarmClientMethod.hs > rej1.log 2>&1 && \
	  grep -Fq '[GHC-83605]' rej1.log && \
	  ! $(HC_BASE) -v1 -O2 --make -no-link WarmClientStep.hs > rej2.log 2>&1 && \
	  grep -Fq '[GHC-83605]' rej2.log )

# 5. Cloned ghc-internal interface-only boot DB (built once for Upper, Not, and Pair).
DepInstalledBootIfaceDb:
	@set -euo pipefail; \
	libdir="$$('$(TEST_HC)' --print-libdir)"; \
	'$(PYTHON)' installed_boot_db/clone_boot_iface_db.py "$$libdir" '$(GHC_PKG)' boot_out; \
	db_flags="-clear-package-db -package-db ../boot_out/db"; \
	if_dir="../boot_out/interfaces"; \
	mkdir -p up_exp up_cli; \
	cp UnicodeExport.hs up_exp/; \
	( cd up_exp && $(HC_BASE) $$db_flags -v0 -O0 -i -fomit-interface-pragmas -c UnicodeExport.hs ); \
	cp up_exp/UnicodeExport.hi boot_out/interfaces/; \
	cp NativeOpenInstalledUpper.hs installed_boot_db/Specialize.hs installed_boot_db/WrongTitle.hs up_cli/; \
	( cd up_cli && \
	  $(HC_BASE) $$db_flags -v0 -O0 -i "-i$$if_dir" -c NativeOpenInstalledUpper.hs && \
	  $(HC_BASE) $$db_flags -v0 -O0 -i "-i$$if_dir" -c Specialize.hs && \
	  ! $(HC_BASE) $$db_flags -v0 -O0 -i "-i$$if_dir" -c WrongTitle.hs > wrong.log 2>&1 && \
	  test "$$(grep -Fc '[GHC-83865]' wrong.log)" = 2 && \
	  $(HC_BASE) -v0 -O0 NativeOpenInstalledUpper.o ../up_exp/UnicodeExport.o -o client && \
	  ./client > actual.out && \
	  diff -u ../NativeOpenInstalledUpper.stdout actual.out ); \
	mkdir -p not_work; \
	cp OpenMapWorker.hs InstalledNotSource.hs InstalledNotProof.hs \
	   installed_boot_db/Override.hs installed_boot_db/Bridge.hs installed_boot_db/Call.hs installed_boot_db/Empty.hs not_work/; \
	( cd not_work && \
	  $(HC_BASE) -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c OpenMapWorker.hs && \
	  rm -f OpenMapWorker.hs OpenMapWorker.o && \
	  $(HC_BASE) $$db_flags -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c InstalledNotSource.hs && \
	  rm -f InstalledNotSource.hs InstalledNotSource.o && \
	  $(HC_BASE) $$db_flags -v0 -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -c InstalledNotProof.hs && \
	  rflags="-fenable-rewrite-rules -fno-omit-interface-pragmas -fno-ignore-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing"; \
	  $(HC_BASE) $$db_flags -v0 -O0 $$rflags -c Override.hs Bridge.hs && \
	  rm -f Override.o Bridge.o && \
	  $(HC_BASE) $$db_flags -v0 -O1 $$rflags -ddump-rule-firings -c Call.hs > fired.log && \
	  grep -Fq 'Rule fired: not/override' fired.log && \
	  $(HC_BASE) $$db_flags -v0 -O0 $$rflags -c Empty.hs && \
	  sed -e 's/module InstalledNotProof/module Ruled/' -e 's/import InstalledNotSource/import Bridge/' InstalledNotProof.hs > Ruled.hs && \
	  ! $(HC_BASE) $$db_flags -v0 -O0 $$rflags -c Ruled.hs > rule.log 2>&1 && \
	  grep -Fq '[GHC-83605]' rule.log ); \
	mkdir -p pair_exp pair_cli pair_staged; \
	cp PreludePairExport.hs WrongRigidPair.hs pair_exp/; \
	( cd pair_exp && \
	  $(HC_BASE) $(ghcThWayFlags) $$db_flags -v0 -O0 -i -fno-omit-interface-pragmas -c PreludePairExport.hs && \
	  cp PreludePairExport.hi "$$if_dir/" && \
	  ! $(HC_BASE) $(ghcThWayFlags) $$db_flags -v0 -O0 -i "-i$$if_dir" -c WrongRigidPair.hs > rej.log 2>&1 && \
	  grep -Fq '[GHC-25897]' rej.log ); \
	cp NativeOpenInstalledPair.hs WrongGroundPair.hs WrongPairNamespace.hs pair_cli/; \
	( cd pair_cli && \
	  $(HC_BASE) $(ghcThWayFlags) $$db_flags -v0 -O0 -i "-i$$if_dir" -c NativeOpenInstalledPair.hs && \
	  ! $(HC_BASE) $(ghcThWayFlags) $$db_flags -v0 -O0 -i "-i$$if_dir" -c WrongGroundPair.hs > g.log 2>&1 && \
	  grep -Eq 'GHC-(83865|18872)' g.log && \
	  ! $(HC_BASE) $(ghcThWayFlags) $$db_flags -v0 -O0 -i "-i$$if_dir" -c WrongPairNamespace.hs > ns.log 2>&1 && \
	  grep -Fq '[GHC-37479]' ns.log && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 NativeOpenInstalledPair.o ../pair_exp/PreludePairExport.o -o client && \
	  ./client > actual.out && \
	  diff -u ../NativeOpenInstalledPair.stdout actual.out ); \
	cp InstalledStagedPair.hs WrongPairRule.hs pair_staged/; \
	( cd pair_staged && \
	  $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -fno-omit-interface-pragmas -fenable-rewrite-rules -c InstalledStagedPair.hs && \
	  ! $(HC_BASE) $(ghcThWayFlags) -v0 -O0 -i -i. "-i$$if_dir" -fno-ignore-interface-pragmas -fenable-rewrite-rules -c WrongPairRule.hs > wr.log 2>&1 && \
	  grep -Fq '[GHC-83605]' wr.log )

# 6. Installed package facade DB hiding internal retained vector representations.
DepPackageFacadeDb:
	@set -euo pipefail; \
	libdir="$$('$(TEST_HC)' --print-libdir)"; \
	unit="retained-vector-facade-0.1"; \
	base_id="$$('$(GHC_PKG)' --global-package-db "$$libdir/package.conf.d" --no-user-package-db field base id --simple-output | head -n1)"; \
	exp_id="$$('$(GHC_PKG)' --global-package-db "$$libdir/package.conf.d" --no-user-package-db field ghc-experimental id --simple-output | head -n1)"; \
	ar_cmd="$$('$(TEST_HC)' --info | sed -n 's/^ ,("ar command","\(.*\)")$$/\1/p')"; \
	mkdir -p fac_prov fac_lib fac_cli; \
	cp PackedVecLibrary.hs RetainedCharVector.hs RetainedCharLength.hs RetainedCharProjection.hs package_facade_db/PublicVector.hs fac_prov/; \
	( cd fac_prov && \
	  $(HC_BASE) -v0 -O2 -i -i. -hide-all-packages -package base -package-id "$$exp_id" -fwrite-source-equations -this-unit-id "$$unit" --make -no-link PublicVector.hs && \
	  cp ./*.hi ../fac_lib/ && \
	  "$$ar_cmd" rcs "../fac_lib/libHS$$unit.a" ./*.o ); \
	'$(GHC_PKG)' init fac_db; \
	printf 'name: retained-vector-facade\nversion: 0.1\nid: %s\nkey: %s\nexposed: True\nexposed-modules: PublicVector\nhidden-modules: PackedVecLibrary RetainedCharVector RetainedCharLength RetainedCharProjection\nimport-dirs: "%s/fac_lib"\nlibrary-dirs: "%s/fac_lib"\nhs-libraries: HS%s\ndepends: %s %s\n' \
	  "$$unit" "$$unit" "$$PWD" "$$PWD" "$$unit" "$$base_id" "$$exp_id" | '$(GHC_PKG)' --global-package-db "$$libdir/package.conf.d" --no-user-package-db --package-db fac_db register - >/dev/null; \
	cp package_facade_db/FacadeMain.hs fac_cli/Main.hs; \
	cp package_facade_db/FacadeForward.hs package_facade_db/FacadeForge.hs package_facade_db/FacadeHidden.hs fac_cli/; \
	( cd fac_cli && \
	  cflags="-v0 -i -i. -hide-all-packages -package base -static -package-db ../fac_db -package-id $$unit"; \
	  for opt in -O0 -O2; do \
	    $(HC_BASE) $$cflags $$opt -fforce-recomp Main.hs -o observe && \
	    test "$$(./observe)" = '(1,"r")' && \
	    $(HC_BASE) $$cflags $$opt -fforce-recomp -c FacadeForward.hs; \
	  done && \
	  ! $(HC_BASE) $$cflags -fforce-recomp -DWRONG -c FacadeForward.hs > wrong.log 2>&1 && \
	  grep -Fq 'GHC-83865' wrong.log && \
	  ! $(HC_BASE) $$cflags -fforce-recomp -c FacadeForge.hs > forge.log 2>&1 && \
	  grep -Fq 'GHC-01928' forge.log && \
	  ! $(HC_BASE) $$cflags -fforce-recomp -c FacadeHidden.hs > hidden.log 2>&1 && \
	  grep -Fq 'GHC-87110' hidden.log )

# 7. Shared plugin package DB exercising general GHC plugins and dependent-obs-eq.
DepPluginsPackageDb:
	@set -euo pipefail; \
	libdir="$$('$(TEST_HC)' --print-libdir)"; \
	ghc_ver="$$('$(TEST_HC)' --numeric-version)"; \
	ar_cmd="$$('$(TEST_HC)' --info | sed -n 's/^ ,("ar command","\(.*\)")$$/\1/p')"; \
	'$(PYTHON)' plugins_package_db/clone_plugin_pkg_db.py '$(TEST_HC)' "$$libdir" '$(GHC_PKG)' plug_db; \
	pflags="-v0 -clear-package-db -package-db plug_db"; \
	mkdir -p api_o2 gen_lib obs_lib; \
	$(HC_BASE) $$pflags -O2 -dynamic -package ghc NativePluginsGeneral.hs -outputdir api_o2 -o api_o2/api-check; \
	./api_o2/api-check; \
	g_unit="test-general-plugins-0.1"; \
	g_so="libHS$${g_unit}-ghc$${ghc_ver}.so"; \
	g_mods="plugins_package_db/TestTcPlugin.hs plugins_package_db/TestSourcePlugin.hs plugins_package_db/TestHoleFitPlugin.hs plugins_package_db/TestCoreInspectPlugin.hs plugins_package_db/TestCoreMutatePlugin.hs plugins_package_db/TestLatePlugin.hs"; \
	$(HC_BASE) $$pflags -O2 -dynamic-too -this-unit-id "$$g_unit" -package ghc -iplugins_package_db -odir gen_lib -hidir gen_lib -c $$g_mods; \
	"$$ar_cmd" rcs "gen_lib/libHS$${g_unit}.a" gen_lib/*.o; \
	$(HC_BASE) $$pflags -shared -dynamic -no-hs-main -package ghc "-optl-Wl,-soname,$$g_so" gen_lib/*.dyn_o -o "gen_lib/$$g_so" 2>/dev/null; \
	base_id="$$('$(GHC_PKG)' --package-db plug_db field base id --simple-output | head -n1)"; \
	ghc_id="$$('$(GHC_PKG)' --package-db plug_db field ghc id --simple-output | head -n1)"; \
	cont_id="$$('$(GHC_PKG)' --package-db plug_db field containers id --simple-output | head -n1)"; \
	time_id="$$('$(GHC_PKG)' --package-db plug_db field time id --simple-output | head -n1)"; \
	printf 'name: test-general-plugins\nversion: 0.1\nid: %s\nkey: %s\nexposed: True\nexposed-modules: TestTcPlugin TestSourcePlugin TestHoleFitPlugin TestCoreInspectPlugin TestCoreMutatePlugin TestLatePlugin\nimport-dirs: "%s/gen_lib"\nlibrary-dirs: "%s/gen_lib"\ndynamic-library-dirs: "%s/gen_lib"\nhs-libraries: HS%s\ndepends: %s %s\n' \
	  "$$g_unit" "$$g_unit" "$$PWD" "$$PWD" "$$PWD" "$$g_unit" "$$base_id" "$$ghc_id" | '$(GHC_PKG)' --package-db plug_db register - >/dev/null; \
	o_unit="dependent-obs-eq-0.1.0.0"; \
	o_so="libHS$${o_unit}-ghc$${ghc_ver}.so"; \
	o_mods="$(DEPENDENT_OBS_EQ_DIR)/src/Dependent/ObsEq.hs $(DEPENDENT_OBS_EQ_DIR)/src/Dependent/Plugin/ObsEq.hs"; \
	$(HC_BASE) $$pflags -O2 -XGHC2024 -dynamic-too -this-unit-id "$$o_unit" -package base -package containers -package ghc -package time -i"$(DEPENDENT_OBS_EQ_DIR)/src" -odir obs_lib -hidir obs_lib -c $$o_mods; \
	"$$ar_cmd" rcs "obs_lib/libHS$${o_unit}.a" obs_lib/Dependent/ObsEq.o obs_lib/Dependent/Plugin/ObsEq.o; \
	$(HC_BASE) $$pflags -shared -dynamic -no-hs-main -package base -package containers -package ghc -package time "-optl-Wl,-soname,$$o_so" obs_lib/Dependent/ObsEq.dyn_o obs_lib/Dependent/Plugin/ObsEq.dyn_o -o "obs_lib/$$o_so" 2>/dev/null; \
	printf 'name: dependent-obs-eq\nversion: 0.1.0.0\nid: %s\nkey: %s\nexposed: True\nexposed-modules: Dependent.ObsEq Dependent.Plugin.ObsEq\nimport-dirs: "%s/obs_lib"\nlibrary-dirs: "%s/obs_lib"\ndynamic-library-dirs: "%s/obs_lib"\nhs-libraries: HS%s\ndepends: %s %s %s %s\n' \
	  "$$o_unit" "$$o_unit" "$$PWD" "$$PWD" "$$PWD" "$$o_unit" "$$base_id" "$$cont_id" "$$ghc_id" "$$time_id" | '$(GHC_PKG)' --package-db plug_db register - >/dev/null; \
	export LD_LIBRARY_PATH="$$PWD/gen_lib:$$PWD/obs_lib$${LD_LIBRARY_PATH:+:$$LD_LIBRARY_PATH}"; \
	for plug in TestTcPlugin TestSourcePlugin TestHoleFitPlugin TestCoreInspectPlugin; do \
	  mkdir -p "mat_$$plug/nc" "mat_$$plug/cg"; \
	  : > "mat_$$plug/nc/trace"; \
	  sed 's/module Main (main) where/module DepProg (main) where/' plugins_package_db/DepProg.hs > "mat_$$plug/nc/DepProg.hs"; \
	  $(HC_BASE) $$pflags --make -fno-code -fwrite-interface -package ghc-experimental -package ghc-internal -XGHC2024 -package test-general-plugins "-fplugin=$$plug" "-fplugin-opt=$$plug:mat_$$plug/nc/trace" -i -i"$(DEPENDENT_BASE_DIR)/src" -i"mat_$$plug/nc" -odir "mat_$$plug/nc/b" -hidir "mat_$$plug/nc/b" "mat_$$plug/nc/DepProg.hs"; \
	  : > "mat_$$plug/cg/trace"; \
	  cp plugins_package_db/DepProg.hs "mat_$$plug/cg/DepProg.hs"; \
	  $(HC_BASE) $$pflags -O2 -dynamic --make -package ghc-experimental -package ghc-internal -XGHC2024 -package test-general-plugins "-fplugin=$$plug" "-fplugin-opt=$$plug:mat_$$plug/cg/trace" -i -i"$(DEPENDENT_BASE_DIR)/src" -i"mat_$$plug/cg" -odir "mat_$$plug/cg/b" -hidir "mat_$$plug/cg/b" "mat_$$plug/cg/DepProg.hs" -o "mat_$$plug/cg/prog"; \
	  "./mat_$$plug/cg/prog"; \
	  case "$$plug" in \
	    TestTcPlugin) grep -Fq 'tc-init' "mat_$$plug/cg/trace" ;; \
	    TestSourcePlugin) grep -Fq 'parsed:Main' "mat_$$plug/cg/trace" && grep -Fq 'typechecked:Data.Nat.Dependent' "mat_$$plug/cg/trace" ;; \
	    TestCoreInspectPlugin) grep -Fq 'core:Main:' "mat_$$plug/cg/trace" ;; \
	  esac; \
	done; \
	mkdir -p holes neg_m1 neg_m2 neg_late; \
	cp plugins_package_db/ProofWithHoles.hs holes/; \
	! $(HC_BASE) $$pflags --make -fno-code -fwrite-interface -frefinement-level-hole-fits=1 -package ghc-experimental -package ghc-internal -XGHC2024 -package test-general-plugins -fplugin=TestTcPlugin -fplugin=TestSourcePlugin -fplugin=TestHoleFitPlugin -fplugin=TestCoreInspectPlugin -i -i"$(DEPENDENT_BASE_DIR)/src" -iholes -odir holes/b -hidir holes/b holes/ProofWithHoles.hs > holes/out.log 2>&1; \
	grep -Fq 'pluginSynthesizedFit' holes/out.log && grep -Fq 'Refl ::' holes/out.log && grep -Fq 'plusZero (_ :: Nat)' holes/out.log; \
	! $(HC_BASE) $$pflags -O0 -dynamic -c -package test-general-plugins -fplugin=TestCoreMutatePlugin -odir neg_m1 -hidir neg_m1 plugins_package_db/BadLookup.hs > neg_m1/out.log 2>&1; \
	grep -Fq '[GHC-68947]' neg_m1/out.log; \
	cp plugins_package_db/MutatedProvider.hs plugins_package_db/MutatedClient.hs neg_m2/; \
	! $(HC_BASE) $$pflags --make -O0 -dynamic -package test-general-plugins -fplugin=TestCoreMutatePlugin -i -ineg_m2 -odir neg_m2/b -hidir neg_m2/b neg_m2/MutatedClient.hs > neg_m2/out.log 2>&1; \
	grep -Eq '\[GHC-(70565|68947)\]' neg_m2/out.log; \
	! $(HC_BASE) $$pflags -O0 -dynamic -c -package test-general-plugins -fplugin=TestLatePlugin -odir neg_late -hidir neg_late plugins_package_db/NativeLate.hs > neg_late/out.log 2>&1; \
	grep -Fq 'Native dependent Core cannot enter late plugins' neg_late/out.log; \
	mkdir -p obs_prov obs_pos0 obs_pos2 obs_pkg obs_neg obs_ass; \
	cp plugins_package_db/ObsProvider.hs obs_prov/; \
	$(HC_BASE) $$pflags -O2 -package base -XGHC2024 -c obs_prov/ObsProvider.hs -o obs_prov/ObsProvider.o -ohi obs_prov/ObsProvider.hi; \
	$(HC_BASE) $$pflags -O2 -dynamic -fPIC -osuf dyn_o -hisuf dyn_hi -package base -XGHC2024 -c obs_prov/ObsProvider.hs -o obs_prov/ObsProvider.dyn_o -ohi obs_prov/ObsProvider.dyn_hi; \
	$(HC_BASE) $$pflags -O0 -dynamic --make -package base -package ghc-experimental -package dependent-obs-eq -XGHC2024 -i -iobs_prov -odir obs_pos0 -hidir obs_pos0 NativeObsEqPlugin.hs -o obs_pos0/prog && ./obs_pos0/prog >/dev/null; \
	$(HC_BASE) $$pflags -O2 -dynamic --make -package base -package ghc-experimental -package dependent-obs-eq -XGHC2024 -i -iobs_prov -odir obs_pos2 -hidir obs_pos2 NativeObsEqPlugin.hs -o obs_pos2/prog && ./obs_pos2/prog >/dev/null; \
	$(HC_BASE) $$pflags -O2 -dynamic --make -package base -package ghc-experimental -package dependent-obs-eq -XGHC2024 -odir obs_pkg -hidir obs_pkg "$(DEPENDENT_OBS_EQ_DIR)/test/Main.hs" -o obs_pkg/prog && ./obs_pkg/prog >/dev/null; \
	cp obs_prov/ObsProvider.* obs_neg/; \
	! $(HC_BASE) $$pflags -O0 -c -fno-code -package base -package ghc-experimental -XGHC2024 -i -iobs_prov -odir obs_neg -hidir obs_neg plugins_package_db/NoPluginLocal.hs > obs_neg/np1.log 2>&1 && grep -Fq '[GHC-68947]' obs_neg/np1.log; \
	! $(HC_BASE) $$pflags -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code -package base -package ghc-experimental -XGHC2024 -i -iobs_prov -odir obs_neg -hidir obs_neg plugins_package_db/NoPluginImported.hs > obs_neg/np2.log 2>&1 && grep -Fq '[GHC-70565]' obs_neg/np2.log; \
	! $(HC_BASE) $$pflags -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code -package base -package ghc-experimental -package dependent-obs-eq -XGHC2024 -odir obs_neg -hidir obs_neg plugins_package_db/EvenRefuted.hs > obs_neg/er.log 2>&1 && grep -Fq 'Observational equivalence refuted' obs_neg/er.log; \
	! $(HC_BASE) $$pflags -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code -package base -package ghc-experimental -package dependent-obs-eq -XGHC2024 -i -iobs_prov -odir obs_neg -hidir obs_neg plugins_package_db/StrictVsLazy.hs > obs_neg/svl.log 2>&1 && tr '\n' ' ' < obs_neg/svl.log | grep -Eq 'Observational equivalence refuted.*<bottom#'; \
	! $(HC_BASE) $$pflags -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code -package base -package ghc-experimental -package dependent-obs-eq -XGHC2024 -i -iobs_prov -odir obs_neg -hidir obs_neg plugins_package_db/LoopVsZero.hs > obs_neg/lvz.log 2>&1 && tr '\n' ' ' < obs_neg/lvz.log | grep -Eq 'Observational equivalence refuted.*<timeout>'; \
	! $(HC_BASE) $$pflags -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code -package base -package ghc-experimental -package dependent-obs-eq -XGHC2024 -i -iobs_prov -odir obs_neg -hidir obs_neg plugins_package_db/BareReflAssume.hs > obs_neg/bre.log 2>&1 && tr '\n' ' ' < obs_neg/bre.log | grep -Eq 'Couldn.t match type'; \
	$(HC_BASE) $$pflags -O2 -dynamic --make -package base -package ghc-experimental -package dependent-obs-eq -fplugin-opt=Dependent.Plugin.ObsEq:policy=assume -XGHC2024 -i -iobs_prov -odir obs_ass -hidir obs_ass plugins_package_db/BareReflAssume.hs -o obs_ass/prog && ./obs_ass/prog; \
	! $(HC_BASE) $$pflags -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code -package base -package ghc-experimental -package dependent-obs-eq -XGHC2024 -i -iobs_prov -odir obs_neg -hidir obs_neg plugins_package_db/BareReflFalseAssume.hs > obs_neg/brfa.log 2>&1 && tr '\n' ' ' < obs_neg/brfa.log | grep -Eq 'Observational equivalence refuted.*<bottom#'
