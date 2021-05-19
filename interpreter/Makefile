BNFC = bnfc
GHC = ghc
LANG_NAME = Lattepp
PARSER_BUILD = parser_build

.PHONY: all

all: parser interpreter

parser: ${LANG_NAME}.cf
	@echo "Creating a parser..."
	mkdir ${PARSER_BUILD} && \
	cd ${PARSER_BUILD} && \
	${BNFC} --haskell --functor -d -m ../${LANG_NAME}.cf && \
	make && \
	mv ${LANG_NAME} ../ && \
	cd .. && \
	rm -r -f ${PARSER_BUILD}

interpreter: Main.hs Interpreter.hs Typechecker.hs
	@echo "Compiling an interpreter..."
	${GHC} Main.hs -o interpreter

clean:
	@echo "Cleaning..."
	rm -r -f ${LANG_NAME} ${PARSER_BUILD} interpreter *.o *.hi
