.PHONY : all clean hindent test

HINDENT = hindent --line-length 76 --sort-imports

test : all
	stack test

all :
	stack build

hindent :
	find src test -name '*.hs' -exec $(HINDENT) \{} \;

clean : hindent
	stack clean
	find . -name '*~' -delete
	find . -name '#*' -delete

