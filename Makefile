
.PHONY : test
test : all
	stack test

.PHONY : all
all :
	stack build

.PHONY : lint
lint :
	hlint src/ test/

.PHONY : docs
docs :
	stack haddock && open `stack path --local-doc-root`/index.html

.PHONY : clean
clean : hindent
	stack clean
	find . -name '*~' -delete
	find . -name '#*' -delete

