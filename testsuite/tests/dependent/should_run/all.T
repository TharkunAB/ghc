# Existing native tests in dependent/should_run (decoupled from docs/)
test('T11964', normal, multimod_compile, ['T11964', '-v0'])
test('NativeOpenPairCoercion',
     [req_process, req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeOpenPairCoercionUsage.hs', 'NativeOpenPairCoercionWorker.hs']),
      extra_run_opts(f'"{config.libdir}"')],
     compile_and_run, ['-package ghc -package transformers'])
test('NativeOpenCharGhci',
     extra_files(['OpenAlias.hs', 'OpenProducer.hs', 'OpenSource.hs']),
     ghci_script, ['NativeOpenCharGhci.script'])
test('NativeOpenRetry',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeOpenRetryLib.hs', 'RetryCharacters.hs'])],
     multimod_compile_and_run,
     ['NativeOpenRetry', '-Wno-partial-type-signatures ' + config.ghc_th_way_flags])
test('NativeClosedChar',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedCharLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedChar', '-Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedLists',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedListsLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedLists', '-Werror=unused-imports -Wno-partial-type-signatures ' + config.ghc_th_way_flags])
test('NativeClosedLazyParcel',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['LazyParcel.hs'])],
     multimod_compile_and_run, ['NativeClosedLazyParcel', config.ghc_th_way_flags])
test('NativeClosedEnums',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeClosedEnumA.hs', 'NativeClosedEnumB.hs', 'NativeClosedEnumReexport.hs', 'FiniteTree.hs'])],
     multimod_compile_and_run,
     ['NativeClosedEnums', '-Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedIndexed',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['IndexedMark.hs'])],
     multimod_compile_and_run,
     ['NativeClosedIndexed', config.ghc_th_way_flags])
test('NativeClosedIndexedVec',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['IndexedVec.hs', 'ExistentialVec.hs', 'NativeClosedVectorPair.hs',
                   'NativeClosedIndexedRoute.hs', 'VectorSplitObservation.hs', 'PackedVecLibrary.hs'])],
     multimod_compile_and_run,
     ['NativeClosedIndexedVec', config.ghc_th_way_flags])
test('NativeClosedCompoundLists',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedCompoundListsLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedCompoundLists', config.ghc_th_way_flags])
test('NativeClosedResponseNames', [req_interp, req_rts_linker, only_ways(['normal'])],
     compile, [config.ghc_th_way_flags])
test('NativeClosedChoices',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedChoiceLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedChoices', config.ghc_th_way_flags])
test('NativeSourceTable',
     [omit_ways(['optasm', 'optllvm']),
      extra_files(['ClosedTableLibrary.hs', 'PackedVecLibrary.hs'])],
     multimod_compile_and_run,
     ['NativeSourceTable', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeNumeralGhci',
     extra_files(['PackedVecLibrary.hs']),
     ghci_script, ['NativeNumeralGhci.script'])
test('NativeInferredFamilyGhci', normal, ghci_script, ['NativeInferredFamilyGhci.script'])
test('NativeLetIndependentRule',
     [omit_ways(['optasm', 'optllvm']),
      extra_files(['PackedVecLibrary.hs'])],
     multimod_compile_and_run,
     ['NativeLetIndependentRule', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeListBoundary',
     extra_files(['PackedVecLibrary.hs', 'OrdinaryVecClient.hs']),
     multimod_compile_and_run,
     ['NativeListBoundary', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeClosedSum',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedSumLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedSum', '-Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedNestedExpected',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedSumLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedNestedExpected', config.ghc_th_way_flags])
test('NativeTypeLambdaReify',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['TypeLambda.hs', 'NativeTypeLambdaReifyClient.hs'])],
     multimod_compile, ['NativeTypeLambdaReifyClient', '-v0 ' + config.ghc_th_way_flags])
test('NativeCSEProjection',
     [omit_ways(ghci_ways), extra_files(['NativeCSEProjectionLib.hs'])],
     multimod_compile_and_run, ['NativeCSEProjection', '-O1'])
test('NativeStrictNamedCase', normal, compile_and_run, [''])
test('NativeReflectionShow', normal, compile_and_run, [''])
test('NativePercentArrowQuote',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativePercentArrowProvider.hs'])],
     multimod_compile, ['NativePercentArrowQuote', '-v0 ' + config.ghc_th_way_flags])
test('NativeTypeCaseReify',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeCase.hs', 'NativeNestedCase.hs', 'OpenMapWorker.hs'])],
     multimod_compile, ['NativeTypeCaseReify', '-v0 ' + config.ghc_th_way_flags])
test('NativeExistentialBeta',
     [req_host_target_ghc, unless(have_ncg(), skip), omit_ways(ghci_ways)],
     compile_and_run, ['-O1'])
test('NativeSpecialiseZip',
     [omit_ways(ghci_ways), extra_files(['NativeSpecialiseZipProvider.hs'])],
     multimod_compile_and_run, ['NativeSpecialiseZip', '-O2'])

# Converted Group (b) (+ merged Group (a)) native tests
test('NativeMapWitnessInference', [omit_ways(ghci_ways), extra_files(['NativeMapWitnessLib.hs', 'MapRuleReplacement.hs'])], multimod_compile_and_run, ['NativeMapWitnessInference', '-O1 -Wno-inline-rule-shadowing'])
test('NativeOpenIdentity', extra_files(['OpenAlias.hs', 'OpenProducer.hs', 'OpenSource.hs', 'RuntimeOrdering.hs', 'OrderingPayload.hs', 'IndexedPrefix.hs', 'IndexedPrefixClient.hs', 'NativeOpenIdentityExtra.hs']), multimod_compile_and_run, ['NativeOpenIdentity', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeStructuralCountdown', extra_files(['NativeStructuralCountdownLib.hs']), multimod_compile_and_run, ['NativeStructuralCountdown', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeGadtImplicitSource', extra_files(['NativeGadtImplicitSourceDep.hs', 'NativeGadtImplicitSourceNoReq.hs', 'NativeGadtImplicitSourceExplicit.hs', 'NativeGadtImplicitSourceOrdinary.hs']), multimod_compile, ['NativeGadtImplicitSource', '-v0 -fomit-interface-pragmas'])
test('NativeOrdinaryGadtSource', extra_files(['NativeOrdinaryGadtSourceTree.hs', 'NativeOrdinaryGadtSourceMixed.hs']), multimod_compile_and_run, ['NativeOrdinaryGadtSource', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeCheckedClosure', extra_files(['NativeCheckedClosureProvider.hs', 'NativeCheckedClosureDoubleProvider.hs', 'NativeCheckedClosureDoubleNamed.hs', 'NativeCheckedLookupProvider.hs', 'NativeLocalLookup.hs', 'NativeCheckedPlugSubstUProvider.hs', 'NativeCheckedClosureCases.hs', 'NativeCheckedClosureNamedSubstProvider.hs', 'NativeCheckedClosureNamedSubstClient.hs', 'NativeCheckedClosureUntypedSigmaProvider.hs', 'NativeCheckedClosureUntypedSigmaClient.hs']), multimod_compile_and_run, ['NativeCheckedClosure', '-fwrite-source-equations -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeLocalBooleanResult', extra_files(['NativeLocalBooleanResultLib.hs']), multimod_compile_and_run, ['NativeLocalBooleanResult', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeKeyTree', [req_th, req_interp, req_rts_linker, omit_ways(prof_ways + ghci_ways), extra_files(['SameGroupKeyTreeProof.hs', 'NativeKeyTreeProofFirst.hs', 'NativeKeyTreeTypesSplice.hs', 'NativeKeyTreeWorkersSplice.hs', 'NativeKeyTreeUncheckedNodes.hs'])], multimod_compile_and_run, ['NativeKeyTree', '-fomit-interface-pragmas -fno-write-if-simplified-core ' + config.ghc_th_way_flags])
test('NativeImportedFamily', extra_files(['ImportedFamilyProvider.hs', 'ImportedFamily.hs', 'MixedFamily.hs', 'FamilySignature.hs', 'NativeImportedFamilyDictWithout.hs', 'NativeImportedFamilyAssocWorker.hs', 'NativeImportedFamilyAssocEq.hs']), multimod_compile_and_run, ['NativeImportedFamily', ''])
test('NativeOpenConstructor', extra_files(['OpenConstructorWorker.hs', 'OpenConstructorSource.hs', 'SpineWorker.hs', 'NativeOpenConstructorLargerWorker.hs', 'NativeOpenConstructorResiduals.hs', 'NativeOpenConstructorDisjointWorker.hs', 'NativeOpenConstructorDisjointRules.hs', 'NativeOpenConstructorExtraProofs.hs']), multimod_compile_and_run, ['NativeOpenConstructor', ''])
test('NativeOpenConstructorHelpers', extra_files(['SpineWorker.hs', 'NativeOpenConstructorHelpersResidual.hs', 'NativeOpenConstructorHelpersProvider.hs', 'NativeOpenConstructorHelpersBridge.hs', 'NativeOpenIndexedSubstSyntax.hs']), multimod_compile, ['NativeOpenConstructorHelpers', '-v0 -fno-write-if-simplified-core'])
test('NativeOpenBuild', extra_files(['OpenBuildWorker.hs', 'OpenBuildSource.hs', 'NativeOpenBuildControls.hs', 'NativeOpenBuildVisible.hs', 'NativeOpenBuildDisjointWorker.hs', 'NativeOpenBuildExtra.hs']), multimod_compile_and_run, ['NativeOpenBuild', '-freduction-depth=300'])
test('NativeOpenMap', [req_interp, req_rts_linker, omit_ways(prof_ways), extra_files(['OpenMapWorker.hs', 'OpenMapSource.hs', 'OpenMapProof.hs', 'SpineWorker.hs', 'ClosedTableLibrary.hs', 'NativeOpenMapEmptyRule.hs'])], multimod_compile, ['NativeOpenMap', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core ' + config.ghc_th_way_flags])
test('NativeBinaryEnum', [req_interp, req_rts_linker, omit_ways(prof_ways + ghci_ways), extra_files(['NativeBinaryEnumCert.hs', 'NativeBinaryEnumPrefix.hs', 'NativeBinaryEnumProof.hs'])], multimod_compile_and_run, ['NativeBinaryEnum', '-fomit-interface-pragmas -fno-write-if-simplified-core ' + config.ghc_th_way_flags])
test('NativeClosedMethodApartness', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedMethodApartnessOperand.hs', 'NativeClosedMethodApartnessOrigin.hs', 'NativeClosedMethodApartnessObserved.hs', 'NativeClosedMethodApartnessDead.hs', 'NativeClosedMethodApartnessEmpty.hs', 'NativeClosedMethodApartnessInactive.hs', 'NativeClosedMethodApartnessOpaque.hs'])], multimod_compile_and_run, ['NativeClosedMethodApartness', config.ghc_th_way_flags])
test('NativeOpenInstalledDropWhile', [req_interp, req_rts_linker, extra_files(['NativeOpenSpanShapes.hs', 'PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs', 'PreludeAppendLengthSource.hs', 'PreludeFilterSource.hs', 'PreludeFilterLengthProof.hs', 'PreludeFilterRuntime.hs', 'PreludeFilterAppendProof.hs', 'PreludeTakeWhile.hs', 'PreludeDropWhileSource.hs', 'PreludeDropWhileProof.hs', 'PreludeSpanSource.hs', 'PreludeSpanProof.hs', 'PreludeFilterCutoff.hs', 'NativeOpenDropWhileWalkShapes.hs', 'NativeOpenDropWhileSpanClient.hs'])], multimod_compile_and_run, ['NativeOpenInstalledDropWhile', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeOpenUserGuardedTail', extra_files(['NativeOpenUserGuardedTailLib.hs', 'NativeOpenUserGuardedTailAlias.hs']), multimod_compile_and_run, ['NativeOpenUserGuardedTail', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeOpenConcreteGuardedList', extra_files(['ConcreteGuardedList.hs', 'ConcreteGuardedListSource.hs', 'ConcreteGuardedListProof.hs']), multimod_compile_and_run, ['NativeOpenConcreteGuardedList', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeOpenInstalledAppend', extra_files(['PreludeAppendLengthSource.hs', 'PreludeAppendLengthProof.hs', 'PreludeMapAppendProof.hs', 'PreludeMapCompositionSource.hs', 'PreludeMapCompositionProof.hs', 'PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs', 'PreludeMapLengthProof.hs']), multimod_compile_and_run, ['NativeOpenInstalledAppend', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeOpenInstalledReverse', [req_interp, req_rts_linker, extra_files(['PreludeReverseLengthSource.hs', 'PreludeReverseLengthProof.hs', 'PreludeAppendLengthSource.hs', 'PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs', 'PreludeMapLengthProof.hs', 'NativeOpenInstalledReverseProducer.hs', 'NativeOpenInstalledReverseExtra.hs'])], multimod_compile_and_run, ['NativeOpenInstalledReverse', '-fwrite-source-equations -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeLocalListAccumulator', extra_files(['NativeLocalListAccumulatorLib.hs', 'NativeLocalListAccumulatorRevSource.hs', 'NativeLocalListAccumulatorRevProof.hs']), multimod_compile_and_run, ['NativeLocalListAccumulator', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeRevExplicitListO2', [omit_ways(ghci_ways), extra_files(['NativeRevExplicitListO2Ops.hs', 'NativeRevExplicitListO2RetainedOps.hs', 'NativeRevExplicitListO2BoxMod.hs', 'NativeRevExplicitListO2Shunt.hs', 'NativeRevExplicitListO2MapGuard.hs'])], multimod_compile_and_run, ['NativeRevExplicitListO2', '-O2 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeOpenIndexedAppend', [req_interp, req_rts_linker, omit_ways(prof_ways + ghci_ways), extra_files(['PackedVecLibrary.hs', 'OpenVectorAppend.hs', 'OrdinaryOpenVectorAppend.hs', 'NativeOpenIndexedAppendExtra.hs'])], multimod_compile_and_run, ['NativeOpenIndexedAppend', '-fwrite-source-equations -Werror=inaccessible-code ' + config.ghc_th_way_flags])
test('NativeOpenZipRows', extra_files(['PackedVecLibrary.hs', 'ZipLengthSource.hs', 'ZipRows.hs', 'OrdinaryZipRows.hs', 'NativeOpenZipRowsVariants.hs', 'NativeOpenZipRowsLaws.hs']), multimod_compile_and_run, ['NativeOpenZipRows', '-fwrite-source-equations -fomit-interface-pragmas -fno-write-if-simplified-core -Werror=inaccessible-code'])
test('NativeOpenPreludeZipWith', [omit_ways(ghci_ways), extra_files(['PreludeZipWithSource.hs', 'PreludeZipWithProof.hs', 'PreludeZipWithLengthProof.hs', 'PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs', 'PreludeMapLengthProof.hs', 'PreludeZipSource.hs', 'PreludeZipProof.hs', 'PreludeZipRuntime.hs', 'PreludeZipMapRuntime.hs', 'ZipMinimum.hs', 'NativeOpenZipDemandControls.hs', 'NativeOpenPreludeZipWithFoldProvider.hs', 'NativeOpenPreludeZipWithFoldRule.hs', 'NativeOpenPreludeZipWithFoldProof.hs'])], multimod_compile_and_run, ['NativeOpenPreludeZipWith', '-fwrite-source-equations'])
test('NativeInferredSourceOperands', extra_files(['PackedVecLibrary.hs', 'NativeInferredSourceOperandsLib.hs']), multimod_compile_and_run, ['NativeInferredSourceOperands', '-fwrite-source-equations -fno-enable-rewrite-rules -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeOpenMinimum', [req_th, req_interp, req_rts_linker, omit_ways(prof_ways + ghci_ways), extra_files(['PreludeMapLengthSize.hs', 'PreludeZipMapRuntime.hs', 'PreludeZipRuntime.hs', 'ZipMinimum.hs', 'NativeOpenMinimumDormantRule.hs'])], multimod_compile_and_run, ['NativeOpenMinimum', config.ghc_th_way_flags])
test('NativeStructuralCarriedArgs', extra_files(['NativeStructuralCarriedArgsWorker.hs']), multimod_compile_and_run, ['NativeStructuralCarriedArgs', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeIndependentConstructorRows', extra_files(['NativeIndependentConstructorRowsIndep.hs', 'NativeIndependentConstructorRowsGuarded.hs']), multimod_compile_and_run, ['NativeIndependentConstructorRows', '-fomit-interface-pragmas'])
test('NativeStructuralSizePredicate', extra_files(['NativeStructuralSizePredicateCount.hs']), multimod_compile_and_run, ['NativeStructuralSizePredicate', '-fomit-interface-pragmas'])
test('NativeOpenMonus', extra_files(['PreludeMapLengthSize.hs', 'NativeOpenMonusSource.hs', 'NativeOpenMonusVariants.hs']), multimod_compile_and_run, ['NativeOpenMonus', '-fwrite-source-equations -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeSizeListNoDescent', extra_files(['PreludeMapLengthSize.hs', 'NativeSizeListNoDescentSource.hs']), multimod_compile, ['NativeSizeListNoDescentSource', '-v0'])
test('NativeRetainedFamilyGiven', extra_files(['PackedVecLibrary.hs', 'NativeRetainedFamilyGiven.hs', 'NativeRetainedTailGiven.hs', 'NativeRetainedFamilyGivenAll.hs']), multimod_compile, ['NativeRetainedFamilyGivenAll', '-v0'])
test('NativeOpenListFoldr', extra_files(['ListFoldrSource.hs', 'ListFoldrProof.hs', 'PreludeFoldrProof.hs', 'PreludeAppendLengthSource.hs', 'PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs', 'PreludeMapLengthProof.hs', 'NativeOpenListFoldrProducer.hs', 'NativeOpenListFoldrConcrete.hs', 'NativeOpenListFoldrExtra.hs']), multimod_compile_and_run, ['NativeOpenListFoldr', '-fwrite-source-equations -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeOpenEitherFmap', extra_files(['PreludeFmapEitherProof.hs', 'OrdinaryEitherMapping.hs', 'CheckedEitherInput.hs', 'NativeOpenEitherFmapProducer.hs', 'NativeOpenEitherFmapBranch.hs', 'either-fmap-overlap-source']), multimod_compile_and_run, ['NativeOpenEitherFmap', '-ieither-fmap-overlap-source'])
test('NativeOpenTheseFmap', extra_files(['These.hs', 'TheseMapping.hs', 'OrdinaryTheseMapping.hs', 'Row.hs', 'RowProof.hs', 'NativeOpenTheseFmapUniformRow.hs', 'NativeOpenTheseFmapUniform.hs']), multimod_compile_and_run, ['NativeOpenTheseFmap', ''])
test('NativePolyNestedDomainOpenEq', extra_files(['NativePolyNestedDomainOpenEqProvider.hs', 'NativePolyNestedDomainOpenEqReverse.hs']), multimod_compile_and_run, ['NativePolyNestedDomainOpenEq', '-Wall -Wno-name-shadowing -Werror'])
test('NativeMultiParamMethodOpenEq', extra_files(['NativeMultiParamMethodOpenEqProvider.hs']), multimod_compile_and_run, ['NativeMultiParamMethodOpenEq', '-Wall -Wno-name-shadowing -Werror'])
test('NativeOpenPairLinkable', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeOpenPairLinkableAux.hs', 'NativeOpenPairLinkableWorker.hs', 'NativeOpenPairLinkableMixed.hs'])], multimod_compile, ['NativeOpenPairLinkable', '-v0 -fenable-rewrite-rules -fno-omit-interface-pragmas -fno-write-if-simplified-core ' + config.ghc_th_way_flags])
test('NativeRetainedNatural', extra_files(['RetainedNaturals.hs', 'RetainedNaturalClient.hs', 'RetainedCharacters.hs', 'RetainedCharacterClient.hs', 'NativeRetainedCharChecked.hs']), multimod_compile_and_run, ['NativeRetainedNatural', ''])
test('NativeOpenRetryInterface', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['RetryCharacters.hs', 'NativeOpenRetryInterfaceProducer.hs'])], multimod_compile_and_run, ['NativeOpenRetryInterface', '-Wno-partial-type-signatures ' + config.ghc_th_way_flags])
test('NativeClosedLength', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['ClosedLengthProducer.hs'])], multimod_compile_and_run, ['NativeClosedLength', '-Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedComputedPair', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['ComputedPair.hs', 'ComputedPairProof.hs', 'PairProjection.hs', 'NativeClosedComputedPairAlias.hs'])], multimod_compile, ['NativeClosedComputedPair', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core -fenable-rewrite-rules -Wno-inline-rule-shadowing ' + config.ghc_th_way_flags])
test('NativeClosedWitnessedBatch', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['WitnessedBatch.hs', 'RectangularTable.hs', 'IndexedVec.hs', 'ExistentialVec.hs', 'NativeClosedWitnessedBatchExtra.hs'])], multimod_compile_and_run, ['NativeClosedWitnessedBatch', '-fomit-interface-pragmas -fno-write-if-simplified-core ' + config.ghc_th_way_flags])
test('NativeClosedFusedRule', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedFusedRuleLib.hs'])], multi_compile, [None, [('NativeClosedFusedRuleLib.hs', '')], '-O1 -fenable-rewrite-rules -fno-omit-interface-pragmas -fno-ignore-interface-pragmas -fno-worker-wrapper -fno-write-if-simplified-core ' + config.ghc_th_way_flags])
test('NativeKnownPair', extra_files(['NativeKnownPairProvider.hs']), multimod_compile_and_run, ['NativeKnownPair', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeRetainedTypeWitness', extra_files(['NativeRetainedTypeWitnessLib.hs', 'NativeRetainedTypeOperand.hs']), multimod_compile_and_run, ['NativeRetainedTypeWitness', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeClosedCallback', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedCallbackLib.hs', 'NativeClosedCallbackForward.hs'])], multimod_compile_and_run, ['NativeClosedCallback', '-Werror=unused-imports -fomit-interface-pragmas -fno-write-if-simplified-core ' + config.ghc_th_way_flags])
test('NativeUnsaturatedFamily', extra_files(['NativeUnsaturatedProvider.hs', 'NativeUnsaturatedClient.hs', 'NativePartialSynonym.hs', 'NativeUnsaturatedEta.hs', 'NativeBuiltinTypeFunctions.hs', 'NativeUnsaturatedPattern.hs', 'NativeUnsaturatedReexport.hs', 'NativeStandaloneUnsaturationProvider.hs', 'NativeStandaloneUnsaturationClient.hs']), multimod_compile_and_run, ['NativeUnsaturatedFamily', '-XUnsaturatedTypeFamilies -freduction-depth=8'])
test('NativeMatchableProjection', extra_files(['MatchableEquality.hs', 'MatchableView.hs', 'RetainedCharacters.hs', 'NativeMatchableKindProjection.hs', 'NativeNestedMatchableProjection.hs', 'NativeMatchableMixed.hs']), multimod_compile_and_run, ['NativeMatchableProjection', '-Werror=incomplete-patterns'])
test('NativeLaterMatchability', extra_files(['AnchorReceiver.hs', 'LaterMatchability.hs', 'LaterMatchabilityClient.hs', 'PromotedEndo.hs', 'NativeLaterMatchabilityExtras.hs']), multimod_compile_and_run, ['NativeLaterMatchability', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeSynonymReify', [req_th, req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeSynonymReifyProvider.hs', 'NativeSynonymReifyClient.hs'])], multimod_compile, ['NativeSynonymReify', '-v0 -fomit-interface-pragmas ' + config.ghc_th_way_flags])
test('NativeIndexedClassReceiver', extra_files(['IndexedMap.hs', 'IndexedMapClient.hs', 'NativeIndexedClassReceiverLate.hs']), multimod_compile_and_run, ['NativeIndexedClassReceiver', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeUnsaturatedInvisible', extra_files(['InvisibleFamily.hs', 'NativeUnsaturatedInvisibleConsumer.hs']), multimod_compile, ['NativeUnsaturatedInvisible', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeTypeLambdaEta', extra_files(['NativeTypeLambdaEtaLib.hs', 'NativeTypeLambdaEtaProofs.hs']), multimod_compile_and_run, ['NativeTypeLambdaEta', ''])
test('NativeTypeLambdaQuote', [req_th, req_interp, req_rts_linker, only_ways(['normal']), extra_files(['TypeLambda.hs', 'TypeLambdaQuote.hs', 'NativeTypeLambdaQuoteHelpers.hs', 'NativeTypeLambdaTypedCallbacks.hs', 'NativeTypeLambdaTypedRuntime.hs'])], multimod_compile_and_run, ['NativeTypeLambdaQuote', config.ghc_th_way_flags])
test('NativeTypeLambdaTypeApp', [req_th, req_interp, req_rts_linker, only_ways(['normal']), extra_files(['TypeLambda.hs', 'TypeLambdaArgument.hs', 'NativeTypeLambdaTypeAppCalls.hs'])], multimod_compile_and_run, ['NativeTypeLambdaTypeApp', config.ghc_th_way_flags])
test('NativeScopedSumMatch', extra_files(['NativeScopedSumMatchLeft.hs', 'NativeScopedSumMatchRight.hs', 'NativeScopedSumMatchClient.hs']), multimod_compile, ['NativeScopedSumMatch', '-v0'])
test('NativeScopedSumSignatureReify', [req_th, req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeScopedSumSignatureReifyProvider.hs', 'NativeScopedSumSignatureReifyClient.hs'])], multimod_compile_and_run, ['NativeScopedSumSignatureReify', '-fomit-interface-pragmas -fno-write-if-simplified-core ' + config.ghc_th_way_flags])
test('NativeGadtRetainedSum', extra_files(['NativeGadtRetainedSumProvider.hs']), multimod_compile_and_run, ['NativeGadtRetainedSum', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeClosedFamilySum', extra_files(['NativeClosedFamilySumProvider.hs', 'NativeClosedFamilySumChain.hs']), multimod_compile_and_run, ['NativeClosedFamilySum', ''])
test('NativeConstrainedSum', extra_files(['NativeAssociatedSumClass.hs', 'NativeAssociatedSumProvider.hs', 'NativeConstrainedSumProvider.hs']), multimod_compile_and_run, ['NativeConstrainedSum', ''])
test('NativeAssociatedPayloadSum', extra_files(['AssociatedPayloadSum.hs', 'AssociatedPayloadSumClient.hs', 'ComposedPayloadSum.hs', 'ComposedPayloadSumClient.hs']), multimod_compile_and_run, ['NativeAssociatedPayloadSum', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeMonomorphicRetainedProjection', extra_files(['PackedVecLibrary.hs', 'RetainedCharVector.hs', 'RetainedCharLength.hs', 'RetainedCharProjection.hs', 'RetainedCharLocal.hs', 'RetainedCharClient.hs']), multimod_compile_and_run, ['NativeMonomorphicRetainedProjection', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeRetainedSiblingFields', extra_files(['PackedVecLibrary.hs', 'SiblingColumns.hs', 'SiblingColumnsClient.hs', 'SiblingColumnRow.hs', 'TypeLambda.hs']), multimod_compile_and_run, ['NativeRetainedSiblingFields', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeNamedRetainedSiblingFields', extra_files(['PackedVecLibrary.hs', 'NamedSiblingColumns.hs', 'ParamColumns.hs', 'NativeNamedSiblingHelpers.hs', 'NativeNamedSiblingStrict.hs']), multimod_compile_and_run, ['NativeNamedRetainedSiblingFields', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeNamedRetainedRecordReify', req_th, compile, [''])
test('NativeAlternatingBinders', extra_files(['NativeAlternatingBindersProvider.hs', 'NativeAlternatingBindersArrow.hs', 'NativeAlternatingBindersRank.hs', 'NativeAlternatingBindersDomain.hs', 'NativeAlternatingBindersStored.hs']), multimod_compile_and_run, ['NativeAlternatingBinders', ''])
test('NativeLiftedCallback', extra_files(['NativeLiftedCallbackProvider.hs', 'curried-callback/FamilyCallback.hs', 'NativeLiftedCallbackExtra.hs', 'RuntimeOrdering.hs', 'CompareAt.hs', 'CompareOptional.hs']), multimod_compile_and_run, ['NativeLiftedCallback', ''])
test('NativeNamedCallback', extra_files(['PreludeMapLengthSize.hs', 'PreludeZipMapRuntime.hs', 'PreludeZipRuntime.hs', 'NativeNamedCallbackOtherSizes.hs', 'NativeNamedCallbackMeasuredZip.hs']), multimod_compile_and_run, ['NativeNamedCallback', ''])
test('NativeOpaqueCallbackAlias', extra_files(['NativeOpaqueCallbackAliasBridge.hs', 'NativeOpaqueCallbackAliasProvider.hs', 'NativeOpaqueCallbackAliasProof.hs', 'NativeNamedOpaqueCallbackProvider.hs', 'NativeNamedOpaqueCallbackAlias.hs', 'NativeNamedOpaqueCallbackPublic.hs']), multimod_compile_and_run, ['NativeOpaqueCallbackAlias', ''])
test('NativeNamedOpaqueCharacter', extra_files(['CharacterDecision.hs']), multimod_compile, ['NativeNamedOpaqueCharacter', '-v0'])
test('NativeRetainedMatchability', extra_files(['NativeRetainedMatchabilityOrdinary.hs', 'NativeRetainedMatchabilityRetained.hs', 'NativeRetainedMatchabilityDefer.hs']), multimod_compile_and_run, ['NativeRetainedMatchability', ''])
test('NativeNestedPackagePat', [req_th, extra_files(['NativeGadtRetainedSumProvider.hs', 'NativeNestedPackagePatProvider.hs', 'PackedVecLibrary.hs'])], multimod_compile_and_run, ['NativeNestedPackagePat', '-Werror=incomplete-patterns -Werror=overlapping-patterns -package template-haskell'])
test('NativeRetainedIndexHpc', [omit_ways(ghci_ways), extra_files(['NativeRetainedIndexHpcProvider.hs'])], multimod_compile_and_run, ['NativeRetainedIndexHpc', '-fhpc -package hpc'])
test('NativeTypeLetQuote', [req_th, req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeTypeLetQuoteLib.hs'])], multimod_compile_and_run, ['NativeTypeLetQuote', config.ghc_th_way_flags])
test('NativeTypeLet', extra_files(['NativeTypeLetContexts.hs', 'TypeLet.hs', 'PackedVecLibrary.hs', 'NativeTypeLetSignature.hs', 'NativeTypeLetSigned.hs', 'NativeTypeLetPairBindings.hs', 'NativeTypeLetOrdinary.hs', 'NativeTypeLetOperator.hs']), multimod_compile_and_run, ['NativeTypeLet', ''])
test('NativeClosedMatchability', extra_files(['NativeClosedMatchabilityProvider.hs', 'NativeClosedMatchabilityClient.hs']), multimod_compile, ['NativeClosedMatchability', '-v0'])
test('NativeOpenMatchability', extra_files(['../should_compile/T14038.hs', '../../polykinds/T14174a.hs', 'NestedFunctionKind.hs', 'NestedFunctionClient.hs', 'NativeOpenMatchabilityClient.hs', 'NativeOpenMatchabilityCongNat.hs', 'NativeOpenMatchabilityExtra.hs']), multimod_compile, ['NativeOpenMatchability', '-v0'])
test('NativePercentArrow', extra_files(['NativePercentArrowProvider.hs', 'NativePercentArrowOnlyModifiers.hs']), multimod_compile, ['NativePercentArrow', '-v0 -Werror=unrecognised-modifiers'])
test('NativePercentArrowPpr', normal, compile, [''])
test('NativeTypeCase', extra_files(['OpenMapWorker.hs', 'NativeCase.hs', 'NativeNestedCase.hs', 'FamilyCasePayload.hs', 'NativeTypeCaseFallback.hs', 'NativeTypeCaseClient.hs', 'NativeTypeCaseHeadProvider.hs', 'NativeTypeCaseHeads.hs', 'NativeRetainedJoinProvider.hs', 'NativeTypeCaseFieldCompoundKind.hs', 'NativeTypeCaseCompoundKindClient.hs', 'NativeTypeCaseLocalComputedField.hs', 'NativeTypeCaseRetainedField.hs']), multimod_compile_and_run, ['NativeTypeCase', ''])
test('NativeTypeCaseScopedResult', extra_files(['NativeTypeCaseScopedResultDomain.hs', 'NativeTypeCaseScopedResultProvide.hs', 'NativeTypeCaseScopedResultConsume.hs', 'NativeTypeCaseOrderedScopeDomain.hs', 'NativeTypeCaseOrderedScopeProvide.hs', 'NativeTypeCaseOrderedScopeConsume.hs']), multimod_compile_and_run, ['NativeTypeCaseScopedResult', ''])
test('NativeTypeCaseRetainedTh', [req_th, req_interp, req_rts_linker, req_host_target_ghc, only_ways(['normal']), extra_files(['NativeRetainedTH.hs', 'NativeTypeCaseRetainedThProvider.hs', 'NativeTypeCaseRetainedThQuote.hs', 'NativeTypeCaseRetainedThRaw.hs', 'NativeTypeCaseRetainedThSeparate.hs', 'NativeTypeCaseRetainedThChooseAcross.hs', 'NativeTypeCaseRetainedThRenderAcross.hs', 'NativeTypeCaseRetainedConThProvider.hs', 'NativeTypeCaseRetainedConTh.hs'])], multimod_compile_and_run, ['NativeTypeCaseRetainedTh', '-package ghci -package template-haskell -package binary -package bytestring ' + config.ghc_th_way_flags])
test('NativeRetainedConstructorContext', [req_th, req_interp, req_rts_linker, only_ways(['normal']), extra_files(['retained-constructor-context/Provider.hs', 'NativeRetainedConstructorContextPlain.hs'])], multimod_compile_and_run, ['NativeRetainedConstructorContext', '-Werror=incomplete-patterns ' + config.ghc_th_way_flags])
test('NativeNestedRetainedCase', extra_files(['nested-retained-case/Provider.hs', 'NativeNestedRetainedCaseDiagonal.hs']), multimod_compile_and_run, ['NativeNestedRetainedCase', ''])
test('NativeRetainedFunctionRules', extra_files(['RetainedRules.hs', 'RetainedRulesClient.hs', 'CallbackRules.hs', 'VisibleRuleArguments.hs', 'VisibleRuleClient.hs', 'NativeRetainedFunctionRulesCallback.hs', 'NativeRetainedFunctionRulesScopes.hs', 'NativeRetainedFunctionRulesOwner.hs', 'NativeRetainedFunctionRulesOrphan.hs', 'NativeRetainedFunctionRulesAnchored.hs', 'NativeRetainedFunctionRulesUse.hs']), multimod_compile_and_run, ['NativeRetainedFunctionRules', ''])
test('NativeSimplifier', extra_files(['NativeSimplifierProvider.hs', 'NativeSimplifierLocalRec.hs', 'NativeSimplifierLocalRule.hs', 'NativeSimplifierClients.hs', 'NativeSimplifierPackageCase.hs', 'implicit-existential-source/WitnessSchema.hs', 'PackageRules.hs', 'LexicalPackageRules.hs', 'OrphanPackageRules.hs', 'ErasedStream.hs', 'StreamFold.hs']), multimod_compile_and_run, ['NativeSimplifier', ''])
test('NativeFunctionIndex', [omit_ways(ghci_ways), extra_files(['BookIsomorphism.hs', 'FunctionFieldLibrary.hs', 'FunctionFieldClient.hs', 'ConstructorCallClient.hs', 'PackedVecLibrary.hs', 'NativeFunctionIndexIsoIdentity.hs', 'NativeFunctionIndexIso.hs'])], multimod_compile_and_run, ['NativeFunctionIndex', ''])
test('NativeRetainedPackage', extra_files(['NativePackageOperand.hs', 'NativeDefaultO1InterfaceProducer.hs', 'QuantifiedGadtIndex.hs', 'ListVectorClient.hs', 'StoredVector.hs', 'ConservedPartition.hs', 'DependentPair.hs', 'DependentProjection.hs', 'PackedVecLibrary.hs', 'RetainedNat.hs', 'PackedLength.hs', 'PackedProjection.hs', 'RetainedWitness.hs', 'RetainedProjection.hs', 'NativeRetainedPackageProviders.hs', 'NativeRetainedPackageClients.hs']), multimod_compile_and_run, ['NativeRetainedPackage', '-Wno-inaccessible-code -Wno-overlapping-patterns'])
test('NativeCertifiedSplit', extra_files(['PackedVecLibrary.hs', 'CertifiedSplit.hs', 'CertifiedSplitClient.hs']), multimod_compile_and_run, ['NativeCertifiedSplit', '-fwrite-source-equations'])
test('NativeAppUnderRetainedEquality', extra_files(['app-under-retained-equality/Provider.hs', 'NativeAppUnderRetainedEqualityImpred.hs']), multimod_compile_and_run, ['NativeAppUnderRetainedEquality', ''])
test('NativeLinearRetainedPackage', extra_files(['NativeLinearRetainedPackageProvider.hs']), multimod_compile_and_run, ['NativeLinearRetainedPackage', '-Werror=incomplete-patterns'])
test('NativeNamedSplit', extra_files(['NamedSplitClient.hs', 'PackedVecLibrary.hs', 'NativeNamedSplitOpaque.hs', 'NativeNamedSplitAppend.hs', 'NativeNamedSplitAppendQL.hs']), multimod_compile_and_run, ['NativeNamedSplit', ''])
test('NativeIndexedFin', extra_files(['NativeIndexedFinGate.hs', 'NativeLinearIndexColumns.hs', 'NativeGeneralGadtOperandProvider.hs', 'NativeIndexedOperandWire.hs', 'NativeRetainedAsPattern.hs', 'NativeIndexedConsProof.hs', 'NativeIndexedFinExtra.hs', 'PackedVecLibrary.hs', 'ParameterizedIndexLibrary.hs', 'FinitePosition.hs', 'FinitePositionClient.hs', 'ListCursor.hs', 'AlignedList.hs', 'CursorClient.hs']), multimod_compile_and_run, ['NativeIndexedFin', '-fwrite-source-equations -Werror=incomplete-patterns'])
test('NativeOrdinaryInline', extra_files(['PackedVecLibrary.hs', 'ParameterizedIndexLibrary.hs', 'FinitePosition.hs', 'RecursiveJobs.hs', 'NativeOrdinaryInlineProvider.hs', 'NativeOrdinaryInlineLocalProducer.hs', 'NativeOrdinaryInlinePackages.hs', 'NativeOrdinaryInlineFin.hs', 'NativeOrdinaryInlinableWork.hs']), multimod_compile_and_run, ['NativeOrdinaryInline', '-fwrite-source-equations -fpedantic-bottoms'])
test('NativeRetainedCast', extra_files(['ParameterizedIndexLibrary.hs', 'PackedVecLibrary.hs', 'NativeRetainedCastDomainWorker.hs', 'NativeRetainedCastHReflLib.hs']), multimod_compile_and_run, ['NativeRetainedCast', ''])
test('NativeDefaultVector', extra_files(['DefaultVector.hs', 'PackedVecLibrary.hs']), multimod_compile_and_run, ['NativeDefaultVector', ''])
test('NativeMixedPackageInterface', extra_files(['ForgetWitness.hs']), multimod_compile_and_run, ['NativeMixedPackageInterface', ''])
test('NativeExistentialInstances', extra_files(['ErasedStream.hs', 'StreamPreview.hs']), multimod_compile_and_run, ['NativeExistentialInstances', ''])
test('NativeExistentialSource', [req_th, req_interp, req_rts_linker, only_ways(['normal']), extra_files(['erased-existential-source/Provider.hs', 'erased-existential-source/OrdinaryPack.hs', 'erased-existential-source/Eliminate.hs', 'erased-existential-source/QuotePack.hs', 'erased-existential-source/TypedQuotePack.hs', 'erased-existential-source/ReifyPack.hs', 'NativeExistentialSourceRuleOwner.hs', 'NativeExistentialSourceNamesake.hs'])], multimod_compile_and_run, ['NativeExistentialSource', config.ghc_th_way_flags])
test('NativeConstrainedFamilyRules', extra_files(['NativeConstrainedFamilyRulesProvider.hs', 'PackedVecLibrary.hs', 'ParameterizedIndexLibrary.hs', 'FinitePosition.hs']), multimod_compile_and_run, ['NativeConstrainedFamilyRules', '-fwrite-source-equations'])
test('NativeDataFamilyPromotion', extra_files(['data-family-promotion/TupleProvider.hs', 'data-family-promotion/RetainedTuple.hs', 'data-family-promotion/RetainedConstructor.hs', 'NativeDataFamilyTuplePromotion.hs', 'NativeDataFamilyStrictCheck.hs']), multimod_compile_and_run, ['NativeDataFamilyPromotion', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativePromotedClassBoundary', [omit_ways(ghci_ways), extra_files(['MappingWorker.hs', 'PreludeMappingProof.hs', 'ImplicitVector.hs', 'ScopedClassMethod.hs', 'RetainedNaturals.hs', 'PackedVecLibrary.hs', 'promoted-class/ChooseTarget.hs', 'promoted-class/ChooseControl.hs', 'promoted-class/MappingTags.hs', 'promoted-class/MappingScope.hs', 'promoted-class/DirectMappingProvider.hs', 'promoted-class/DirectMappingControl.hs', 'promoted-class/ChooseEvidence.hs', 'promoted-class/stored-dictionary/StoredEvidence.hs', 'promoted-class/stored-dictionary/Selector.hs', 'promoted-class/stored-dictionary/FoldCell.hs', 'NativePromotedClassAltProv.hs', 'NativePromotedClassAltControl.hs', 'NativePromotedClassEvidencePromoted.hs', 'NativePromotedClassSpecialise.hs'])], multimod_compile_and_run, ['NativePromotedClassBoundary', ''])
test('NativeScopedDictionaryWire', extra_files(['promoted-class/ScopedFamilyMethodProvider.hs', 'NativeScopedDictExportProvider.hs', 'NativeScopedDictImportCheck.hs', 'NativeScopedDictAmbigOk.hs']), multimod_compile_and_run, ['NativeScopedDictionaryWire', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeRetainedFamilyDomain', extra_files(['ConstrainedElements.hs', 'ConstrainedElementsClient.hs', 'ConstrainedFormatClient.hs', 'NativeRetainedFamilyDomainChecks.hs']), multimod_compile_and_run, ['NativeRetainedFamilyDomain', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeConstrainedFamily', [req_th, extra_files(['NativeConstrainedFamilyProvider.hs', 'NativeConstrainedFamilyOrdProvider.hs', 'NativeConstrainedFamilyClient.hs', 'NativePartialFamilyScope.hs', 'NativeImprovedFamilyScope.hs', 'NativeFamilyEquationScope.hs', 'NativeDerivedFamilyScope.hs'])], multimod_compile_and_run, ['NativeConstrainedFamily', '-fomit-interface-pragmas -fno-write-if-simplified-core -Wno-partial-type-signatures'])
test('NativeIndexedForEachCtor', extra_files(['NativeIndexedForEachCtorProvider.hs']), multimod_compile_and_run, ['NativeIndexedForEachCtor', '-Werror=inaccessible-code -Werror=incomplete-patterns'])
test('NativeComposedLetWhere', extra_files(['NativeComposedIsoFields.hs', 'NativeComposedStrictLocal.hs']), multimod_compile_and_run, ['NativeComposedLetWhere', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeComposedIndependent', normal, compile, ['-O2 -Wno-inline-rule-shadowing'])
test('NativeProposal378SyntaxAndFlags', extra_files(['NativeProposal378SyntaxAndFlagsProvider.hs', 'NativeProposal378AliasChecks.hs', 'NativeProposal378NonUnicodeProd.hs', 'NativeProposal378SomeInfix.hs']), multimod_compile_and_run, ['NativeProposal378SyntaxAndFlags', ''])
test('NativeTypeLambdaCaptureU', extra_files(['NativeTypeLambdaCaptureUProvider.hs']), multimod_compile_and_run, ['NativeTypeLambdaCaptureU', ''])
test('NativeSameModuleClosurePlug', [req_th, extra_files(['NativeSameModuleClosurePlugProvider.hs', 'NativeSameModuleClosurePlugRev.hs', 'NativeSameModuleClosurePlugPrenex.hs', 'NativeSameModulePrunedSplice.hs', 'NativeSameModuleNamedAndSigma.hs'])], multimod_compile_and_run, ['NativeSameModuleClosurePlug', ''])
test('NativeRetainedPolyAndInlineGadt', extra_files(['NativeRetainedPolyAndInlineGadtProvider.hs']), multimod_compile_and_run, ['NativeRetainedPolyAndInlineGadt', '-Werror=inaccessible-code -Werror=incomplete-patterns'])
test('NativeRetainedCastSourceCall', extra_files(['NativeRetainedCastSourceCallProvider.hs']), multimod_compile_and_run, ['NativeRetainedCastSourceCall', '-Werror=inaccessible-code -Werror=incomplete-patterns'])
test('NativeLargeElimType', extra_files(['NativeLargeElimTypeProvider.hs', 'NativeLargeElimIndexedProv.hs', 'NativeLargeElimIndexedCli.hs', 'NativeLargeElimIndexedSame.hs']), multimod_compile_and_run, ['NativeLargeElimType', '-Werror=inaccessible-code -Werror=incomplete-patterns'])
test('NativeProposal378Quantifiers', extra_files(['NativeProposal378QuantifiersProvider.hs']), multimod_compile_and_run, ['NativeProposal378Quantifiers', ''])
test('NativeBootSigmaExists', extra_files(['NativeBootSigmaExistsProvider.hs', 'NativeBootSigmaExistsOrdinary.hs']), multimod_compile_and_run, ['NativeBootSigmaExists', ''])
test('NativeAutoNumLiterals', extra_files(['NativeAutoNumLiteralsProvider.hs', 'NativeAutoNumExplicitFam.hs', 'NativeAutoNumMTransProvider.hs', 'NativeAutoNumMTransClient.hs']), multimod_compile_and_run, ['NativeAutoNumLiterals', ''])
test('NativeProposal378PatternsAndT2T', extra_files(['NativeProposal378PatternsAndT2TProvider.hs']), multimod_compile_and_run, ['NativeProposal378PatternsAndT2T', ''])
test('NativeHaskLibInterop', [omit_ways(ghci_ways), extra_files(['HaskLibCore.hs', 'HaskLibProps.hs', 'HaskLibOrdinaryClient.hs', 'HaskLibDependentClient.hs'])], multimod_compile_and_run, ['NativeHaskLibInterop', '-fobject-code'])
test('NativeTotalityReport', omit_ways(ghci_ways), warn_and_run, ['-v0 -freport-totality -Wno-incomplete-patterns -Werror'])
test('NativeRetainedScrutineePoly', extra_files(['NativeRetainedScrutineePolyProvider.hs', 'NativeRetainedScrutineePitaSurface.hs']), multimod_compile_and_run, ['NativeRetainedScrutineePoly', ''])
test('NativeDataHeraldInTypes', extra_files(['NativeDataHeraldInTypesProvider.hs', 'NativeDataHeraldPunsOn.hs', 'NativeDataHeraldNoPunWarnings.hs']), multimod_compile_and_run, ['NativeDataHeraldInTypes', ''])
test('NativeMutualLargeElimData', extra_files(['NativeMutualLargeElimDataProvider.hs']), multimod_compile_and_run, ['NativeMutualLargeElimData', '-Werror=inaccessible-code -Werror=incomplete-patterns'])
test('NativeParamDependentRecordUpdate', extra_files(['NativeParamDependentRecordUpdateProvider.hs', 'NativeParamDependentRecordUpdateOrd.hs']), multimod_compile_and_run, ['NativeParamDependentRecordUpdate', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeDepFunUnfolding', extra_files(['NativeDepFunUnfoldingProvider.hs', 'NativeDepFunUnfoldingDepClient.hs']), multimod_compile_and_run, ['NativeDepFunUnfolding', '-funfolding-use-threshold=1500'])
test('NativeHomogeneousGadtBinders', [req_host_target_ghc, extra_run_opts(f'"{config.libdir}"'), only_ways(['normal']), extra_files(['NativeHomogeneousGadtBindersTarget.hs'])], multimod_compile_and_run, ['NativeHomogeneousGadtBinders', '-package ghc'])
test('NativeUnboxedTupleAndCharPrim', extra_files(['NativeUnboxedTupleAndCharPrimProvider.hs', 'NativeUnboxedTupleNoPuns.hs']), multimod_compile_and_run, ['NativeUnboxedTupleAndCharPrim', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeDataFamilyNewtypeExistential', extra_files(['NativeDataFamilyNewtypeExistentialProvider.hs']), multimod_compile_and_run, ['NativeDataFamilyNewtypeExistential', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeStandaloneConstrainedFamily', extra_files(['NativeStandaloneConstrainedFamilyProvider.hs']), multimod_compile_and_run, ['NativeStandaloneConstrainedFamily', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeTypeCaseDotSections', extra_files(['NativeTypeCaseDotSectionsProvider.hs']), multimod_compile_and_run, ['NativeTypeCaseDotSections', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeAbsurdForEachCoverage', normal, compile_and_run, ['-Wall -Werror'])
test('NativeLinearForEachAndExtendedLet', extra_files(['NativeLinearForEachAndExtendedLetProvider.hs', 'NativeStandaloneExtendedLet.hs']), multimod_compile_and_run, ['NativeLinearForEachAndExtendedLet', ''])
test('NativeDependentCompose', [omit_ways(ghci_ways), extra_files(['NativeDepComposeLib.hs', 'NativeDepComposeH2010.hs', 'NativeDepComposeImpredicative.hs', 'NativeDepComposeListDefs.hs', 'NativeDepComposeSymUnsatOk.hs'])], multimod_compile_and_run, ['NativeDependentCompose', '-fobject-code'])
test('NativeVoidAbsurdOpenEq', extra_files(['NativeVoidAbsurdOpenEqProvider.hs']), multimod_compile_and_run, ['NativeVoidAbsurdOpenEq', '-Wall -Werror'])
test('NativeTypeLambda', extra_files(['TypeLambda.hs', 'TypeLambdaClient.hs', 'SchemaColumns.hs', 'NativeTypeLambdaScope.hs', 'NativeTypeLambdaFormation.hs', 'NativeTypeLambdaOrdinary.hs']), multimod_compile_and_run, ['NativeTypeLambda', '-DANNOTATION'])
test('NativeLocalOpenRun', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeLocalOpenProducer.hs', 'NativeLocalOpenDerived.hs', 'LocalInstance.hs', 'LocalAssociated.hs', 'LocalDataFamily.hs', 'IndependentFamily.hs', 'LocalAliasField.hs'])], multimod_compile_and_run, ['NativeLocalOpenRun', config.ghc_th_way_flags])
test('NativeOpenInstalledMapRun', [omit_ways(ghci_ways), extra_files(['PreludeMapSource.hs', 'PreludeMapProof.hs', 'PreludeFilterSource.hs', 'PreludeFilterProof.hs', 'PreludeFilterLengthProof.hs', 'PreludeFilterRuntime.hs', 'PreludeFilterDecider.hs', 'PreludeFilterAppendProof.hs', 'PreludeFilterAppendRuntime.hs', 'PreludeAppendLengthSource.hs', 'PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs', 'PreludeMapLengthProof.hs', 'NativeRetainedSourceOperand.hs'])], multimod_compile_and_run, ['NativeOpenInstalledMapRun', '-O1'])
test('NativeIndexedGadtFieldsRun', extra_files(['PackedVecLibrary.hs', 'NativeIndexedGadtFieldsProvider.hs', 'NativeNonuniformGadtFieldsProvider.hs', 'NativeIndexedGadtFields.hs']), multimod_compile_and_run, ['NativeIndexedGadtFields', ''])
test('NativeSameModuleClosedEvalRun', [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeSameModuleClosedEval.hs'])], multimod_compile_and_run, ['NativeSameModuleClosedEval', config.ghc_th_way_flags])

# Group (c): Stateful multi-step recompilation, binary interface mutation, and package DB targets
test('DepRecompHomeRules',
     [req_host_target_ghc, req_th, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['recomp_home_rules', 'TypeLambda.hs', 'NativeTypeLambdaScope.hs', 'SchemaColumns.hs'])],
     makefile_test, ['DepRecompHomeRules'])

test('DepRecompSourceUpgrade',
     [req_host_target_ghc, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['recomp_source_upgrade', 'CharacterCase.hs', 'CharacterExport.hs',
                   'NativeOpenCharFamily.hs', 'PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs',
                   'PreludeMapSource.hs', 'NativeOpenInstalledMapProducer.hs',
                   'NativeLocalOpenProducer.hs', 'NativeLocalOpen.hs',
                   'implicit-existential-source'])],
     makefile_test, ['DepRecompSourceUpgrade'])

test('DepRecompClosedEval',
     [req_host_target_ghc, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['recomp_closed_eval', 'NativeClosedReturnedPair.hs', 'NativeClosedReturnedPairLib.hs',
                   'NativeClosedFamilyProjection.hs', 'NativeClosedListsLib.hs', 'NativeOpenRetryLib.hs',
                   'NativeClosedNestedDependencies.hs', 'NativeClosedSumDependencies.hs',
                   'NativeClosedSumDependenciesHelper.hs', 'NativeClosedSumDependenciesInstance.hs'])],
     makefile_test, ['DepRecompClosedEval'])

test('DepIfaceBinaryMutate',
     [req_process, req_host_target_ghc, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['iface_binary_mutate', 'NativeDepFunCoProvider.hs', 'NativeDepFunCoMutate.hs',
                   'NativeDepFunCoClient.hs', 'NativeDepFunCoWire.hs', 'PackedVecLibrary.hs',
                   'NativeIndexedGadtFieldsProvider.hs', 'NativeNonuniformGadtFieldsProvider.hs',
                   'NativeLegacyGadtFieldsMutate.hs', 'NativeIndexedGadtFields.hs'])],
     makefile_test, ['DepIfaceBinaryMutate'])

test('DepInstalledBootIfaceDb',
     [req_host_target_ghc, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['installed_boot_db', 'UnicodeExport.hs', 'NativeOpenInstalledUpper.hs',
                   'NativeOpenInstalledUpper.stdout', 'OpenMapWorker.hs', 'InstalledNotSource.hs',
                   'InstalledNotProof.hs', 'PreludePairExport.hs', 'NativeOpenInstalledPair.hs',
                   'NativeOpenInstalledPair.stdout', 'InstalledStagedPair.hs',
                   'WrongRigidPair.hs', 'WrongGroundPair.hs', 'WrongPairNamespace.hs',
                   'WrongPairRule.hs'])],
     makefile_test, ['DepInstalledBootIfaceDb'])

test('DepPackageFacadeDb',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['package_facade_db', 'PackedVecLibrary.hs', 'RetainedCharVector.hs',
                   'RetainedCharLength.hs', 'RetainedCharProjection.hs'])],
     makefile_test, ['DepPackageFacadeDb'])

test('DepPluginsPackageDb',
     [req_host_target_ghc, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['plugins_package_db', 'NativePluginsGeneral.hs', 'NativeObsEqPlugin.hs'])],
     makefile_test, ['DepPluginsPackageDb'])
test('PromotedConCallbackTypeApp', normal, compile_and_run, [''])
