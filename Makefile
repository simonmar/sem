GHC=ghc
GHC_OPTS=-O2 -Wall -threaded -rtsopts

all :

sembench-sem: sembench.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) $< -o $@

sembench-ssem: sembench.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) -DSSEM $< -o $@

sembench-qsem: sembench.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) -DQSEM $< -o $@

sembench-msem: sembench.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) -DMSEM $< -o $@

.PHONY: runbench
runbench : sembench-sem sembench-ssem sembench-qsem sembench-msem
	@echo ========== test case 0 ===========
	@echo
	time ./sembench-sem  0 5000000
	time ./sembench-ssem 0 5000000
	time ./sembench-qsem 0 5000000
	time ./sembench-msem 0 5000000
	@echo
	@echo ========== test case 1 ===========
	@echo
	time ./sembench-sem  1 1000000
	time ./sembench-ssem 1 1000000
	time ./sembench-qsem 1 1000000
	time ./sembench-msem 1 1000000
	@echo
	@echo ========== test case 2 ===========
	@echo
	time ./sembench-sem  2 100000
	# OMITTED (TAKES TOO LONG): time ./sembench-ssem  2 100000
	# OMITTED (TAKES TOO LONG): time ./sembench-qsem 2 100000
	time ./sembench-msem 2 100000

clean :
	rm -f *.o *.hi sembench-*


semtest-sem: semtest.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) $< -o $@

semtest-ssem: semtest.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) -DSSEM $< -o $@

semtest-qsem: semtest.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) -DQSEM $< -o $@

semtest-msem: semtest.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) -DMSEM $< -o $@


.PHONY: runtest
runtest : semtest-sem semtest-ssem semtest-qsem semtest-msem
	@echo ============== tests =============
	-./semtest-sem
	-./semtest-ssem
	-./semtest-qsem
	-./semtest-msem
	@echo
