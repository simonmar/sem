GHC=ghc
GHC_OPTS=-O2 -Wall -threaded -rtsopts

all :

sembench-oldq: sembench.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DOLDQ $< -o $@

sembench-newq: sembench.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DNEWQ $< -o $@

sembench-newqs: sembench.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DNEWQS $< -o $@

sembench-ssem: sembench.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DSSEM $< -o $@

sembench-msem: sembench.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DMSEM $< -o $@

.PHONY: runbench
runbench : sembench-oldq sembench-newq sembench-newqs sembench-ssem sembench-msem
	@echo ========== test case 0 ===========
	@echo
	time ./sembench-oldq  0 5000000
	time ./sembench-newq  0 5000000
	time ./sembench-newqs 0 5000000
	time ./sembench-ssem  0 5000000
	time ./sembench-msem  0 5000000
	@echo
	@echo ========== test case 1 ===========
	@echo
	time ./sembench-oldq  1 1000000
	time ./sembench-newq  1 1000000
	time ./sembench-newqs 1 1000000
	time ./sembench-ssem  1 1000000
	time ./sembench-msem  1 1000000
	@echo
	@echo ========== test case 2 ===========
	@echo
	# OMITTED (TAKES TOO LONG): time ./sembench-oldq  2 100000
	time ./sembench-newq  2 100000
	time ./sembench-newqs 2 100000
	# OMITTED (TAKES TOO LONG): time ./sembench-ssem  2 100000
	time ./sembench-msem 2 100000

clean :
	rm -f *.o *.hi sembench-*


semtest-oldq: semtest.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DOLDQ $< -o $@

semtest-newq: semtest.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DNEWQ $< -o $@

semtest-newqs: semtest.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DNEWQS $< -o $@

semtest-ssem: semtest.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DSSEM $< -o $@

semtest-msem: semtest.hs QSemSTM.hs QSem.hs
	$(GHC) $(GHC_OPTS) -DMSEM $< -o $@


.PHONY: runtest
runtest : semtest-oldq semtest-newq semtest-newqs semtest-ssem semtest-msem
	@echo ============== tests =============
	-./semtest-oldq
	-./semtest-newq
	-./semtest-newqs
	-./semtest-ssem
	-./semtest-msem
	@echo


semntest-qsemn: semntest.hs QSemN.hs
	$(GHC) -fhpc $(GHC_OPTS) -DQSEMN $< -o $@

semntest-newqn: semntest.hs QSemN.hs
	$(GHC) -fhpc $(GHC_OPTS) -DNEWQSEMN $< -o $@

semntest-msemn: semntest.hs QSemN.hs
	$(GHC) -fhpc $(GHC_OPTS) -DMSEMN $< -o $@

.PHONY: runtest-qsemn
runtest-qsemn : semntest-qsemn semntest-newqn semntest-msemn
	@echo ============== tests =============
	-./semntest-qsemn
	-./semntest-newqn
	-./semntest-msemn
	@echo



semnbench-newqn: semnbench.hs QSemN.hs
	$(GHC) $(GHC_OPTS) -DNEWQSEMN $< -o $@

semnbench-qsemn: semnbench.hs QSemN.hs
	$(GHC) $(GHC_OPTS) -DQSEMN $< -o $@

semnbench-msemn: semnbench.hs QSemN.hs
	$(GHC) $(GHC_OPTS) -DMSEMN $< -o $@

.PHONY: runbench
runbench-qsemn : semnbench-newqn semnbench-qsemn semnbench-msemn
	@echo ========== test case 0 ===========
	@echo
	time ./semnbench-qsemn    0 5000000
	time ./semnbench-newqn 0 5000000
	time ./semnbench-msemn    0 5000000
	@echo
	@echo ========== test case 1 ===========
	@echo
	time ./semnbench-qsemn    1 1000000
	time ./semnbench-newqn 1 1000000
	time ./semnbench-msemn    1 1000000
	@echo
	@echo ========== test case 2 ===========
	@echo
	# OMITTED (takes too long): time ./semnbench-qsemn    2 100000
	time ./semnbench-newqn 2 100000
	time ./semnbench-msemn    2 100000
