test('FamilyArrowArity', normal, compile_fail, [''])
test('StuckFamilyArrowArity', normal, compile_fail, [''])
test('FamilyArrowMatchability', normal, compile_fail, [''])
test('ForallMatchabilityEmpty', normal, compile_fail, [''])
test('ForallMatchabilityTHWrapper', req_th, compile_fail, [''])
test('PartialFamilyArityAfterErasure', req_th, compile_fail, [''])
test('PartialFamilyArityWithSplice', req_th, compile_fail, [''])
test('PatSynMatchableArrow', normal, compile_fail, [''])
test('RetainedQuantificationInKind', normal, compile_fail, [''])
test('DependentArrowMatchability',
     [extra_files(['DependentArrowProvider.hs'])],
     multi_compile,
     ['DependentArrowMatchability',
      [('DependentArrowProvider.hs', '-fomit-interface-pragmas -fno-write-if-simplified-core')],
      '-v0 -XNoDependentTypes'])
test('DependentArrowMatchabilityDep',
     [extra_files(['DependentArrowProvider.hs', 'DependentArrowMatchability.hs'])],
     multi_compile,
     ['DependentArrowMatchabilityDep',
      [('DependentArrowProvider.hs', '-fomit-interface-pragmas -fno-write-if-simplified-core'),
       ('DependentArrowMatchability.hs', '-XDependentTypes -XListTuplePuns')],
      '-v0'])
test('DependentArrowMatchabilityFail',
     [extra_files(['DependentArrowProvider.hs', 'DependentArrowMatchability.hs'])],
     multimod_compile_fail,
     ['DependentArrowMatchabilityFail', '-v0 -XNoDependentTypes'])
test('DependentArrowMatchabilityFailDep',
     [extra_files(['DependentArrowProvider.hs', 'DependentArrowMatchability.hs'])],
     multimod_compile_fail,
     ['DependentArrowMatchabilityFailDep', '-v0'])
test('DependentArrowMatchabilitySigFail',
     [extra_files(['DependentArrowProvider.hs'])],
     multimod_compile_fail,
     ['DependentArrowMatchabilitySigFail', '-v0 -XNoDependentTypes'])
test('DependentArrowMatchabilitySigFailDep',
     [extra_files(['DependentArrowProvider.hs'])],
     multimod_compile_fail,
     ['DependentArrowMatchabilitySigFailDep', '-v0'])
test('DependentArrowMatchabilityValFail',
     [extra_files(['DependentArrowProvider.hs', 'DependentArrowMatchability.hs'])],
     multimod_compile_fail,
     ['DependentArrowMatchabilityValFail', '-v0 -XNoDependentTypes'])
test('DependentArrowMatchabilityValFailDep',
     [extra_files(['DependentArrowProvider.hs', 'DependentArrowMatchability.hs'])],
     multimod_compile_fail,
     ['DependentArrowMatchabilityValFailDep', '-v0'])
