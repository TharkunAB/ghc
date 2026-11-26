TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T13938:
	$(RM) T13938a.hi T13938a.o T13938.hi T13938.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T13938a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T13938.hs

source_call_hc = '$(TEST_HC)' $(TEST_HC_OPTS) -v0 -fno-force-recomp -fomit-interface-pragmas -fno-write-if-simplified-core -i -iinterfaces -outputdir interfaces

NativeSourceCallIface:
	mkdir -p interfaces changed
	$(source_call_hc) -fno-code -fwrite-interface -c ClosedTableLibrary.hs
	$(source_call_hc) -c NativeSourceCallIface.hs
	sed 's/flipBit False = True/flipBit False = False/' ClosedTableLibrary.hs > changed/ClosedTableLibrary.hs
	$(source_call_hc) -fno-code -fwrite-interface -fforce-recomp -c changed/ClosedTableLibrary.hs
	! $(source_call_hc) -c NativeSourceCallIface.hs 2>changed.stderr
	grep -q 'GHC-83865' changed.stderr
	$(source_call_hc) -fno-code -fwrite-interface -c NativeMethodAliases.hs
	$(source_call_hc) -c NativeMethodAliasClient.hs
	sed 's/forward = turn/forward = stay/' NativeMethodAliases.hs > changed/NativeMethodAliases.hs
	$(source_call_hc) -fno-code -fwrite-interface -fforce-recomp -c changed/NativeMethodAliases.hs
	! $(source_call_hc) -c NativeMethodAliasClient.hs 2>changed-method.stderr
	grep -q 'GHC-83865' changed-method.stderr
	grep -q 'forwardProof' changed-method.stderr
	$(source_call_hc) -fno-code -fwrite-interface -c PackedVecLibrary.hs
	$(source_call_hc) -c NativeLiteralClient.hs
	$(source_call_hc) -c RetainedSumOperator.hs
	sed 's/foreach (y :: Nat) -> Vec Bool (x P.+ y)/foreach (y :: Bool) -> Vec Bool (x P.+ y)/' RetainedSumOperator.hs > changed/RetainedSumOperator.hs
	! $(source_call_hc) -fforce-recomp -c changed/RetainedSumOperator.hs 2>wrong-source-operator.stderr
	grep -q 'GHC-83865' wrong-source-operator.stderr
	sed 's/(2 :: Nat)/(2 :: Integer)/' NativeLiteralClient.hs > changed/NativeLiteralClient.hs
	! $(source_call_hc) -fforce-recomp -c changed/NativeLiteralClient.hs 2>wrong-annotation.stderr
	grep -q 'cannot use the cast required for this argument as a retained index' wrong-annotation.stderr
	sed 's/fromInteger 3/fromInteger 2/' NativeLiteralClient.hs > changed/NativeLiteralClient.hs
	! $(source_call_hc) -fforce-recomp -c changed/NativeLiteralClient.hs 2>wrong-conversion.stderr
	grep -q 'GHC-83865' wrong-conversion.stderr
	grep -q 'threeConverted' wrong-conversion.stderr
	sed 's/n == 0 = Z/n == 0 = S Z/' PackedVecLibrary.hs > changed/PackedVecLibrary.hs
	$(source_call_hc) -fno-code -fwrite-interface -fforce-recomp -c changed/PackedVecLibrary.hs
	! $(source_call_hc) -c NativeLiteralClient.hs 2>changed-literal.stderr
	grep -q 'has no checked zero and successor equations' changed-literal.stderr
	grep -q 'twoAnnotated' changed-literal.stderr

dep_iface_hc = '$(TEST_HC)' $(TEST_HC_OPTS) -v0 -fomit-interface-pragmas -fno-write-if-simplified-core

DepIfaceSerialization:
	mkdir -p with_eqns no_closures without_eqns
	$(dep_iface_hc) -XDependentTypes -fwrite-source-equations -fexpose-all-unfoldings -outputdir with_eqns -c DepIfaceSerialization.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface with_eqns/DepIfaceSerialization.hi > with_eqns.iface
	grep -Fq 'SourceTable T:flipBit' with_eqns.iface
	grep -Fq 'FinalCoreSourceTable T:monus' with_eqns.iface
	grep -Fq 'CheckedClosureSourceTable T:renameExpr [extendName]' with_eqns.iface
	grep -Fq 'CheckedClosureSourceTable T:replace' with_eqns.iface
	$(dep_iface_hc) -i -iwith_eqns -outputdir with_eqns -fno-code -c DepIfaceSerializationClient.hs
	$(dep_iface_hc) -XDependentTypes -fno-write-source-equations -fexpose-all-unfoldings -outputdir no_closures -c DepIfaceSerialization.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface no_closures/DepIfaceSerialization.hi > no_closures.iface
	! grep -Fq 'CheckedClosureSourceTable' no_closures.iface
	$(dep_iface_hc) -XNoDependentTypes -fno-write-source-equations -outputdir without_eqns -c DepIfaceSerialization.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface without_eqns/DepIfaceSerialization.hi > without_eqns.iface
	! grep -Eq '  SourceTable |CheckedClosureSourceTable' without_eqns.iface
	! $(dep_iface_hc) -i -iwithout_eqns -outputdir without_eqns -fno-code -c DepIfaceSerializationClient.hs 2>without_eqns.stderr
	grep -q 'GHC-70565' without_eqns.stderr
	mkdir -p nocode_eqns
	$(dep_iface_hc) -XDependentTypes -fwrite-source-equations -fno-code -fwrite-interface -outputdir nocode_eqns -c DepIfaceSerialization.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface nocode_eqns/DepIfaceSerialization.hi > nocode_eqns.iface
	grep -Fq 'SourceTable T:flipBit' nocode_eqns.iface
	grep -Fq 'FinalCoreSourceTable T:monus' nocode_eqns.iface
	grep -Fq 'CheckedClosureSourceTable T:renameExpr [extendName]' nocode_eqns.iface
	grep -Fq 'CheckedClosureSourceTable T:replace' nocode_eqns.iface
	$(dep_iface_hc) -i -inocode_eqns -outputdir nocode_eqns -fno-code -c DepIfaceSerializationClient.hs
