####################
# Makefile for automated aprof testing
####################

## Setup 
DATE= $(shell date +"%m-%d-%Y")
CV_DIR = .
BASENAME = testaprof
MASTER = $(CV_DIR)/aprof.Rmd
MD = $(CV_DIR)/$(BASENAME)_$(DATE).md
PDF = $(CV_DIR)/PDF/$(BASENAME)_$(DATE).pdf

## functions
RNAME = cp '$<' '$@' 
PDOC = pandoc -s --smart '$<' -o '$@'
KNIT = Rscript --vanilla -e "library(markdown,lib='~/R/lib');require(knitr,lib='~/R/lib'); knit('$<','$@')"

PDFLATEX = pdflatex -synctex=1 -interaction=nonstopmode '$<' '$@'

all: $(MD) 

#########################
## main markdownx
## $(OUTPUT):$(INPUT)
## 	: RULES

$(MD):$(MASTER)
	$(KNIT)

$(PDF):$(MD)
	$(PDOC)

