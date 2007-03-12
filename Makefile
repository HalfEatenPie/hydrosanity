## Makefile based on one from Rattle v2.1

# TARGETS
#
# check:	Ask R to check that the package looks okay
# local:	Install into the local machine's R library
# build:	Generate package tar.gz archive
# zip:		Generate package zip archive

PACKAGE=hydrosanity
SRC=$(PACKAGE)/R
NAMESPACE=$(PACKAGE)/NAMESPACE
DESCRIPTION=$(PACKAGE)/DESCRIPTION
DESCRIPTIN=DESCRIPTION.in

# Canonical version information from hydrosanity.R
MAJOR=$(shell egrep '^MAJOR' $(SRC)/hydrosanity.R | cut -d\" -f 2)
MINOR=$(shell egrep '^MINOR' $(SRC)/hydrosanity.R | cut -d\" -f 2)
REVIS=$(shell egrep '^REVIS' $(SRC)/hydrosanity.R | cut -d\" -f 2)
VERSION=$(MAJOR).$(MINOR).$(REVIS)

DATE=$(shell date +%F)

default: local

check: build
	R CMD check $(PACKAGE)

build: hydrosanity_$(VERSION).tar.gz

hydrosanity_$(VERSION).tar.gz: $(SRC)
	perl -p -e "s|^Version: .*$$|Version: $(VERSION)|" < $(DESCRIPTIN) |\
	perl -p -e "s|^Date: .*$$|Date: $(DATE)|" > $(DESCRIPTION)
	R CMD build $(PACKAGE)

zip: local
	(cd /usr/local/lib/R/site-library; zip -r9 - hydrosanity) >| \
	hydrosanity_$(VERSION).zip

local: hydrosanity_$(VERSION).tar.gz
	R CMD INSTALL $^

clean:
	rm -f hydrosanity_*.tar.gz hydrosanity_*.zip
	rm -f $(DESCRIPTION)
