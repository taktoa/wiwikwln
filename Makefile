# -*- mode: makefile -*-
# What I Wish I Knew When Learning Nix - Makefile
#
# The build script that compiles the Markdown guide to a website
#
# Written in 2015 by Remy Goldschmidt <taktoa@gmail.com>
# Copyright © 2015 Remy Goldschmidt
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
# 
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
# 
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.


#-------------------------------------------------------------------------------
#------------------------------------ README -----------------------------------
#-------------------------------------------------------------------------------


# This Makefile makes the following assumptions:
# You can read English
# 
# Your directory separator is '/'
# Your MKDIR executable can take a -p option
# Your RM executable can take -r and -f options
# You haven't moved this script from the root of the git repository
# The semantics of Makefiles and shell are roughly the same as they were in 2015

# Everything else should be configurable through these variables


#-------------------------------------------------------------------------------
#--------------------------- CONFIGURATION VARIABLES ---------------------------
#-------------------------------------------------------------------------------

MDIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

# Project name

NAME = tutorial

# Directories

PUBLIC = ${MDIR}/public
MISC = ${MDIR}/misc
SRC = ${MDIR}/src

ASSETS = ${PUBLIC}/assets

CSS = ${ASSETS}/css
IMAGES = ${ASSETS}/images
SCRIPTS = ${ASSETS}/scripts

# Files

STYLE = ${CSS}/style.css

INPUT = ${SRC}/${NAME}.macro
TEMPLATE = ${SRC}/page.tmpl

SHELL_NIX = ${MISC}/shell.nix
INCLUDES_BIN = ${MISC}/includes
INCLUDES_INT = ${MISC}/includes.hi
INCLUDES_OBJ = ${MISC}/includes.o
INCLUDES_SRC = ${MISC}/includes.hs

HTML = ${PUBLIC}/${NAME}.html
MARKDOWN = ${PUBLIC}/${NAME}.md
PDF = ${PUBLIC}/${NAME}.pdf
EPUB = ${PUBLIC}/${NAME}.epub

# Flags

HTML_ENABLED = true
EPUB_ENABLED = true
PDF_ENABLED = true

MDFMT = markdown+mmd_title_block

PANDOC_HTML_FLAGS = -t html -c "assets/css/style.css" --template ${TEMPLATE}
PANDOC_EPUB_FLAGS = -t epub
PANDOC_PDF_FLAGS = -t latex --latex-engine=xelatex 

GHC_FLAGS =
PANDOC_FLAGS = --toc --toc-depth=2 --highlight-style pygments --ascii --katex -f ${MDFMT} --smart

# Misc

HESC = LITERAL_HASH # All instances of this string in
                    # your Markdown will be converted
                    # to octothorpes (#)


#-------------------------------------------------------------------------------
#------------------------------- OTHER VARIABLES -------------------------------
#-------------------------------------------------------------------------------


SHELL = /usr/bin/env bash -e -o pipefail

# Executable aliases

NIX-SHELL = nix-shell
PANDOC = pandoc
GHC = ghc
ALEX = alex
HAPPY = happy
CPPHS = cpphs
SED = sed
RM = rm
ECHO = echo
EXIT = exit
BASH = bash
WHICH = which
MAKE = make
MKDIR = mkdir

# Directories

TMP = ${MDIR}/tmp

# Files

NULL = /dev/null

MACRO_INT = ${TMP}/markdown-temporary.macro
MACRO_CONF = ${TMP}/macroconfig

# Flags

CPPHS_FLAGS = --noline --text --include=${MACRO_CONF}

# Misc

HLIT = define\|if\|ifdef\|ifndef\|else\|elif\|endif\|include

NONEXIST = "$$PROG was not found, quitting"
CHECK_AVAILABLE = ${WHICH} $$PROG &>${NULL} || { ${ECHO} ${NONEXIST}; ${EXIT} -1; }


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

PANDOC_HTML = ${PANDOC} ${PANDOC_HTML_FLAGS} ${PANDOC_FLAGS}
PANDOC_EPUB = ${PANDOC} ${PANDOC_EPUB_FLAGS} ${PANDOC_FLAGS}
PANDOC_PDF = ${PANDOC} ${PANDOC_PDF_FLAGS} ${PANDOC_FLAGS}


# Targets

## General

all: build

build:
	@${ECHO} "Checking if you are in a nix-shell"
	@[ "$$IN_NIX_SHELL" = "1" ] || { ${MAKE} nix; ${EXIT} 0; }
	@${ECHO} "Good, we are in a nix-shell"
	@${ECHO} "Checking if all the requisite programs are available"
	@${MAKE} check-available
	-${MKDIR} -p ${TMP}
	${ECHO} "#define PATH ${MDIR}/${CSS}" >> ${MACRO_CONF}
	${HTML_ENABLED} && ${MAKE} html
	${EPUB_ENABLED} && ${MAKE} epub
	${PDF_ENABLED}  && ${MAKE} pdf
	-${RM} -rf ${TMP}

html:
	${ECHO} "#define HTML" >> ${MACRO_CONF}
	${MAKE} ${HTML}
	rm ${MACRO_CONF}

pdf:
	${ECHO} "#define PDF" >> ${MACRO_CONF}
	${MAKE} ${PDF}
	rm ${MACRO_CONF}

epub:
	${ECHO} "#define EPUB" >> ${MACRO_CONF}
	${MAKE} ${EPUB}
	rm ${MACRO_CONF}

## Nix

check-available:
	@${EXPORT} PROG=${NIX-SHELL}; ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${PANDOC};	  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${GHC};		  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${ALEX};	  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${HAPPY};	  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${CPPHS};	  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${SED};		  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${RM};		  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${ECHO};	  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${BASH};	  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${WHICH};	  ${CHECK_AVAILABLE}
	@${EXPORT} PROG=${MAKE};	  ${CHECK_AVAILABLE}
	@${ECHO} "All required programs are available"

nix: ${SHELL_NIX}
	@${ECHO} "You were not in a nix-shell, entering one..."
	@${EXPORT} PROG=${NIX-SHELL}; ${CHECK_AVAILABLE}
	${NIX-SHELL} --arg latexEnabled ${PDF_ENABLED} \
	  --run '${MAKE} build' ${SHELL_NIX} || ${EXIT} -1

## Build

### Haskell

${INCLUDES_BIN} ${INCLUDES_INT} ${INCLUDES_OBJ}: ${INCLUDES_SRC}
	${GHC} ${GHC_FLAGS} --make $<

### Pandoc

${PUBLIC}/%.html: ${PUBLIC}/%.md ${INCLUDES_BIN}
	@echo "Compiling HTML ($@) from $<"
	cd ${PUBLIC}; ${INCLUDES_BIN} < $< | ${PANDOC_HTML} -o $@

${PUBLIC}/%.epub: ${PUBLIC}/%.md ${INCLUDES_BIN}
	@echo "Compiling EPUB ($@) from $<"
	cd ${PUBLIC}; ${INCLUDES_BIN} < $< | ${PANDOC_EPUB} -o $@

${PUBLIC}/%.pdf: ${PUBLIC}/%.md ${INCLUDES_BIN}
	@echo "Compiling PDF ($@) from $<"
	cd ${PUBLIC}; ${INCLUDES_BIN} < $< | ${PANDOC_PDF} -o $@

${PUBLIC}/%.md: ${SRC}/%.macro ${MACRO_CONF}
	@echo "Compiling Markdown ($@) from $<"
	${SED} 's:#:${HESC}:g' $< \
	  | ${SED} 's:${HESC}[ ]*\(${HLIT}\):#\1:g' > ${MACRO_INT}
	${CPPHS} ${CPPHS_FLAGS} ${MACRO_INT} | ${SED} 's:${HESC}:#:g' > $@

## Cleanup

.DELETE_ON_ERROR:

clean: clean-output clean-tmp clean-includes

clean-output: clean-html clean-pdf clean-epub clean-markdown 

clean-html:
	-${RM} ${HTML}

clean-pdf:
	-${RM} ${PDF}

clean-epub:
	-${RM} ${EPUB}

clean-markdown:
	-${RM} ${MARKDOWN}

clean-tmp:
	-${RM} -rf ${TMP}

clean-includes:
	-${RM} ${INCLUDES_BIN} ${INCLUDES_INT} ${INCLUDES_OBJ}

