
GHC = ghc -O2 -i../lib

.PHONY : all clean test clean-all

all : autorotate

Autorotate.o : Autorotate.hs
	$(GHC) -c Autorotate.hs

autorotate : autorotate.hs Autorotate.o
	$(GHC) --make autorotate.hs

# if stack overflows occur
# $(GHC) -with-rtsopts "-K92M" --make autorotate.hs

clean :
	rm -f *.{hi,o}

clean-all : clean
	rm -f autorotate
