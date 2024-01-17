PANDOC_OPTS=-t beamer -H templates/slides-header.tex

.PHONY: presentation
presentation: introduction.pdf

%.pdf: %.md
	@pandoc $(PANDOC_OPTS) -o $@ $<

.PHONY: clean
clean:
	@rm -f introduction.pdf

.PHONY: watch
watch:
	@echo introduction.md | entr -c 'make'
