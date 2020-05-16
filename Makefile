VERSION   := 0.1.0.0
TAR_NAME  := simsim-${VERSION}
TAR_EXT   := tar.gz
TAR_FLAGS := --format=ustar --xform="s/^/${TAR_NAME}\//g" -zcf
TAR_FILENAME := ${TAR_NAME}.${TAR_EXT}
FILES     := LICENSE package.yaml README.md Setup.hs simsim.cabal src stack.yaml test

.all : package

.PHONY: simsim build
simsim build :
	stack install --test --haddock

.PHONY: package
package ${TAR_FILENAME} :
	mkdir -p ${TAR_NAME} && \
		cp -R ${FILES} ${TAR_NAME} && \
		find ${TAR_NAME} -type f -exec chmod  644 '{}' \; && \
		find ${TAR_NAME} -type d -exec chmod  755 '{}' \; && \
		tar ${TAR_FLAGS} ${TAR_FILENAME} ${FILES}

.PHONY: clean
clean :
	rm -rf ${TAR_FILENAME} ${TAR_NAME}
	stack clean
