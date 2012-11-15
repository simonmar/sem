GHC=ghc
GHC_OPTS=-O2 -Wall

sembench-sem: sembench.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) $< -o $@

sembench-qsem: sembench.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) -DQSEM $< -o $@

sembench-msem: sembench.hs Semaphore.hs
	$(GHC) $(GHC_OPTS) -DMSEM $< -o $@

.PHONY: run
run : sembench-sem sembench-qsem sembench-msem
	@echo ========== 0 ===========
	@echo
	time ./sembench-sem  0 5000000
	time ./sembench-qsem 0 5000000
	time ./sembench-msem 0 5000000
	@echo
	@echo ========== 1 ===========
	@echo
	time ./sembench-sem  1 1000000
	time ./sembench-qsem 1 1000000
	time ./sembench-msem 1 1000000
	@echo
	@echo ========== 2 ===========
	@echo
	time ./sembench-sem  2 100000
	# time ./sembench-qsem 2 100000
	time ./sembench-msem 2 100000

clean :
	rm -f *.o *.hi sembench-*
