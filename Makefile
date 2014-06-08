###
### By Mateusz Kowalczyk and Peter Selinger
###   see http://www.haskell.org/pipermail/cabal-devel/2014-January/009645.html
###

PACKAGE=yampa-glfw
HACKAGE_USER=_deepfire

VERSION=$(shell grep ^version: ${PACKAGE}.cabal | sed 's/version://;s/[[:space:]]//g')

all: build doc

build: dist/setup/setup
	cabal build

doc: dist/setup/setup
	cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/package/$$pkg/docs'

doc-for-upload: ${PACKAGE}-${VERSION}-docs.tar.gz

${PACKAGE}-${VERSION}-docs.tar.gz: doc
	rm -rf ${PACKAGE}-${VERSION}-docs
	cp -rp dist/doc/html/${PACKAGE} ${PACKAGE}-${VERSION}-docs
	echo '<meta HTTP-EQUIV="REFRESH" content="0; url=http://hackage.haskell.org/package/${PACKAGE}-${VERSION}">' > ${PACKAGE}-${VERSION}-docs/index.html
	echo 'If your browser does not take you there automatically, please go to' >> ${PACKAGE}-${VERSION}-docs/index.html
	echo '<a href=http://hackage.haskell.org/package/${PACKAGE}-${VERSION}>http://hackage.haskell.org/package/${PACKAGE}-${VERSION}</a>.' >> ${PACKAGE}-${VERSION}-docs/index.html
	tar -Hustar -zcf ${PACKAGE}-${VERSION}-docs.tar.gz ${PACKAGE}-${VERSION}-docs

doc-upload: ${PACKAGE}-${VERSION}-docs.tar.gz
	echo -n "Hackage password for ${HACKAGE_USER}: "; read PASSWORD; curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary @${PACKAGE}-${VERSION}-docs.tar.gz http://${HACKAGE_USER}:$$PASSWORD@hackage.haskell.org/package/${PACKAGE}-${VERSION}/docs

install: dist/setup/setup
	cabal install

dist: dist/setup/setup
	cabal sdist

conf dist/setup/setup: Setup.hs
	cabal configure

clean: 
	cabal clean
	rm -rf ${PACKAGE}-${VERSION}-docs
	rm -f ${PACKAGE}-${VERSION}-docs.tar.gz
