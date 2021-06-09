.PHONY: clean distclean default

LLVMCONFIG=llvm-config-9

CXX=clang++
CXXFLAGS=-g -Wall -Wno-delete-non-virtual-dtor -std=c++11 `$(LLVMCONFIG) --cxxflags`
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

default: tony lib.a

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp ast.hpp

tony: lexer.o parser.o ast.o
	$(CXX) $(CXXFLAGS) -o tony $^ $(LDFLAGS)

lib.a: lib.c
	gcc -Wno-unused-value -c -Wall -o lib.o lib.c
	ar rcs lib.a lib.o
	ranlib lib.a

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o *.out *.ll *.s

distclean: clean
	$(RM) tony lib.a