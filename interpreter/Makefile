BNFC = /home/students/inf/PUBLIC/MRJP/bin/bnfc
GHC = ghc
LANG_NAME = Lattepp
PARSER_BUILD = parser_build

.PHONY: all

all: clean parser interpreter

parser: ${LANG_NAME}.cf
	mkdir ${PARSER_BUILD} && \
	cd ${PARSER_BUILD} && \
	${BNFC} --haskell --functor -d -m ../${LANG_NAME}.cf && \
	make && \
	mv ${LANG_NAME} ../ && \
	cd .. && \
	rm -r -f ${PARSER_BUILD}

interpreter: Main.hs Interpreter.hs Typechecker.hs
	${GHC} Main.hs -o interpreter

clean:
	rm -r -f ${LANG_NAME} ${PARSER_BUILD} interpreter *.o *.hi
