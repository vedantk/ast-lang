include Make.common

CXX = clang++
CXXFLAGS = -std=c++0x -Wall -Wextra `llvm-config --cxxflags` 

%.so:
	$(CXX) `llvm-config --cxxflags --ldflags --libs support mc` \
		-lclangFrontend -lclangAST -shared -Wl,-soname,$@ -o \
		$@ $^

ReallocVerifier.so: ReallocVerifier.o
ReallocVerifier.o: ReallocVerifier.cpp

test:
	clang -cc1 -load ./ReallocVerifier.so -plugin realloc-verifier \
		$(LLVMCC1) mem-corrupt.c

clean:
	rm -f *.so *.o
