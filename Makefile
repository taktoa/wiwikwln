NAME = tutorial

CPPHS = cpphs
PANDOC = pandoc
IFORMAT = markdown
CPPHS_FLAGS = --noline --text
PAN_FLAGS = --standalone --toc --toc-depth=2 --highlight-style pygments
TEMPLATE = page.tmpl
STYLE = css/style.css
HESC = LITERAL_HASH
HLIT = define\|if\|ifdef\|ifndef\|elif\|endif
TMP = tmp.txt
GHC_CLEAN = misc/includes.hi misc/includes.o

MACRO = src

TEMPLATE = src/page.tmpl

HTML = public/${NAME}.html
MARKDOWN = public/${NAME}.md
PDF = public/${NAME}.pdf
EPUB = public/${NAME}.epub

UNAVAILABLE = "$$PROG was not found, quitting"
CHECK_AVAILABLE = which $$PROG &>/dev/null || { echo ${UNAVAILABLE}; exit -1; }

all: build

build:
	@echo "Checking if you are in a nix-shell"
	@[ "$$IN_NIX_SHELL" = "1" ] || { make nix; exit 0; }
	@echo "Good, we are in a nix-shell"
	@echo "Checking if all the requisite programs are available"
	@make check-available
	make html

html:
	echo "#define ~HTML" > macro.cfg
	make ${HTML}

pdf:
	echo "#define ~PDF" > macro.cfg
	make ${PDF}

epub:
	echo "#define ~EPUB" > macro.cfg
	make ${EPUB}

check-available:
	@export PROG=nix-shell; ${CHECK_AVAILABLE}
	@export PROG=pandoc;    ${CHECK_AVAILABLE}
	@export PROG=ghc;       ${CHECK_AVAILABLE}
	@export PROG=alex;      ${CHECK_AVAILABLE}
	@export PROG=happy;     ${CHECK_AVAILABLE}
	@export PROG=cpphs;     ${CHECK_AVAILABLE}
	@export PROG=sed;       ${CHECK_AVAILABLE}
	@export PROG=rm;        ${CHECK_AVAILABLE}
	@export PROG=echo;      ${CHECK_AVAILABLE}
	@export PROG=bash;      ${CHECK_AVAILABLE}
	@export PROG=which;     ${CHECK_AVAILABLE}
	@export PROG=make;      ${CHECK_AVAILABLE}
	@echo "All required programs are available"

nix:
	@echo "You were not in a nix-shell, entering one..."
	@export PROG=nix-shell; ${CHECK_AVAILABLE}
	nix-shell --run 'make build'

includes: misc/includes.hs
	ghc --make $<
	rm ${GHC_CLEAN}

public/%.html: public/%.md misc/includes
	./includes < $< | ${PANDOC} -c ${STYLE} --template ${TEMPLATE} -s -f ${IFORMAT} -t html ${PAN_FLAGS} -o $@

public/%.epub: public/%.md misc/includes
	./includes < $< | ${PANDOC} -f ${IFORMAT} -t epub ${PAN_FLAGS} -o $@

public/%.pdf: public/%.md misc/includes
	./includes < $< | ${PANDOC} -c -s -f ${IFORMAT} --latex-engine=xelatex ${PAN_FLAGS} -o $@

public/%.md: src/%.macro macro.cfg
	sed 's:#:${HESC}:g' $< | sed 's:${HESC}\(${HLIT}\):#\1:g' > ${TMP}
	${CPPHS} ${CPPHS_FLAGS} ${TMP} | sed 's:${HESC}:#:g' > $@
	-rm ${TMP}

clean:
	-{ rm ${CHAPTERS} ${HTML} ${MARKDOWN} ${TMP} || true; } &>/dev/null
	-{ rm ${GHC_CLEAN} misc/includes || true; } &>/dev/null
