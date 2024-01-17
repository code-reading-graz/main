# -H templates/slides-header.tex
PANDOC_OPTS=-t beamer

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
