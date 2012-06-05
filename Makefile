########################################################################
# Makefile for ID compiler
########################################################################

# Is this a global installation (with restricted functionality)(yes/no)?
GLOBALINSTALL=yes
# The major version number:
export MAJORVERSION=0
# The minor version number:
export MINORVERSION=2
# The version date:
COMPILERDATE=01/06/12
# The Haskell installation info
INSTALLHS=runtime/Installation.hs
# The Curry installation info
INSTALLCURRY=src/Installation.curry
# Logfile for make:
MAKELOG=make.log
BOOTLOG=boot.log
# The path to the Glasgow Haskell Compiler:
GHC=`which ghc`
# The path to the package configuration file
PKGCONF=`ghc-pkg --user -v0 list | head -1 | sed "s/://"`
# the root directory
export ROOT = ${CURDIR}
# binary directory and executables
export BINDIR=${ROOT}/bin
# Directory where local executables are stored:
export LOCALBIN=${BINDIR}/.local
# Directory where the libraries are located:
export LIBDIR=${ROOT}/lib
export COMP=${LOCALBIN}/kics2c
export REPL=${LOCALBIN}/kics2i

.PHONY: all
all:
	${MAKE} installwithlogging

# bootstrap the compiler using PAKCS
.PHONY: bootstrap
bootstrap: ${INSTALLCURRY} installscripts
	@rm -f ${BOOTLOG}
	@echo "Bootstrapping started at `date`" > ${BOOTLOG}
	cd src && ${MAKE} bootstrap 2>&1 | tee -a ../${BOOTLOG}
	@echo "Bootstrapping finished at `date`" >> ${BOOTLOG}
	@echo "Bootstrap process logged in file ${BOOTLOG}"

# install the complete system and log the installation process
.PHONY: installwithlogging
installwithlogging:
	@rm -f ${MAKELOG}
	@echo "Make started at `date`" > ${MAKELOG}
	${MAKE} install 2>&1 | tee -a ${MAKELOG}
	@echo "Make finished at `date`" >> ${MAKELOG}
	@echo "Make process logged in file ${MAKELOG}"

# install the complete system if the kics2 compiler is present
.PHONY: install
install: kernel
	# compile all libraries if the installation is a global one:
	@if [ ${GLOBALINSTALL} = yes ] ; \
	 then cd runtime && ${MAKE} && \
	      cd ../lib && ${MAKE} compilelibs && \
	                   ${MAKE} installlibs && \
	                   ${MAKE} acy ; fi
	cd cpns  && ${MAKE} # Curry Port Name Server demon
	cd tools && ${MAKE} # various tools
	cd www   && ${MAKE} # scripts for dynamic web pages
	# generate manual, if necessary:
	@if [ -d docs/src ] ; then cd docs/src && ${MAKE} install ; fi
	# make everything accessible:
	chmod -R go+rX .

# install some scripts of KICS2 in the bin directory:
.PHONY: installscripts
installscripts:
	@if [ ! -d ${BINDIR} ] ; then mkdir -p ${BINDIR} ; fi
	cp src/cleancurry.sh ${BINDIR}/cleancurry
	sed "s|^KICS2HOME=.*$$|KICS2HOME=${ROOT}|" < src/cymake.sh > ${BINDIR}/cymake
	sed "s|^KICS2HOME=.*$$|KICS2HOME=${ROOT}|" < src/kics2.sh > ${BINDIR}/kics2
	sed "s|^KICS2HOME=.*$$|KICS2HOME=${ROOT}|" < src/makecurrycgi.sh > ${BINDIR}/makecurrycgi
	cd ${BINDIR} && chmod 755 cleancurry cymake kics2 makecurrycgi

# install a kernel system without all tools
.PHONY: kernel
kernel: ${INSTALLCURRY} installscripts installfrontend
	${MAKE} Compile
	${MAKE} REPL

#
# Create documentation for system libraries:
#
.PHONY: libdoc
libdoc:
	@if [ ! -r bin/currydoc ] ; then \
	  echo "Cannot create library documentation: currydoc not available!" ; exit 1 ; fi
	@rm -f ${MAKELOG}
	@echo "Make libdoc started at `date`" > ${MAKELOG}
	@cd lib && ${MAKE} doc 2>&1 | tee -a ../${MAKELOG}
	@echo "Make libdoc finished at `date`" >> ${MAKELOG}
	@echo "Make libdoc process logged in file ${MAKELOG}"

# install the front end if necessary:
.PHONY: installfrontend
installfrontend:
	@if [ ! -d ${LOCALBIN} ] ; then mkdir -p ${LOCALBIN} ; fi
	# install mcc front end if sources are present:
	@if [ -f mccparser/Makefile ] ; then cd mccparser && ${MAKE} ; fi
	# install local front end if sources are present:
	@if [ -d frontend ] ; then ${MAKE} installlocalfrontend ; fi

# install local front end:
.PHONY: installlocalfrontend
installlocalfrontend:
	cd frontend/curry-base && cabal install
	cd frontend/curry-frontend && cabal install
	# copy cabal installation of front end into local directory
	@if [ -f ${HOME}/.cabal/bin/cymake ] ; then cp -p ${HOME}/.cabal/bin/cymake ${LOCALBIN} ; fi

.PHONY: Compile
Compile: ${INSTALLCURRY}
	cd src ; ${MAKE} CompileBoot

.PHONY: REPL
REPL: ${INSTALLCURRY}
	cd src ; ${MAKE} REPLBoot

# generate module with basic installation information:
${INSTALLCURRY}: ${INSTALLHS}
	cp ${INSTALLHS} ${INSTALLCURRY}

${INSTALLHS}: Makefile
	@if [ ! -x "${GHC}" ] ; then echo "No executable 'ghc' found in path!" && exit 1; fi
	echo "-- This file is automatically generated, do not change it!" > ${INSTALLHS}
	echo "module Installation where" >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'compilerName :: String' >> ${INSTALLHS}
	echo 'compilerName = "KiCS2 Curry -> Haskell Compiler"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installDir :: String' >> ${INSTALLHS}
	echo 'installDir = "'`pwd`'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'majorVersion :: Int' >> ${INSTALLHS}
	echo 'majorVersion = ${MAJORVERSION}' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'minorVersion :: Int' >> ${INSTALLHS}
	echo 'minorVersion = ${MINORVERSION}' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'compilerDate :: String' >> ${INSTALLHS}
	echo 'compilerDate = "'${COMPILERDATE}'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installDate :: String' >> ${INSTALLHS}
	echo 'installDate = "'`date`'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'ghcExec :: String' >> ${INSTALLHS}
	echo 'ghcExec = "'${GHC}'" ++ " -package-conf '${PKGCONF}'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installGlobal :: Bool' >> ${INSTALLHS}
	@if [ ${GLOBALINSTALL} = yes ] ; \
	 then echo 'installGlobal = True' >> ${INSTALLHS} ; \
	 else echo 'installGlobal = False' >> ${INSTALLHS} ; fi

# install required cabal packages

.PHONY: installhaskell
installhaskell:
	cabal update
	cabal install network
	cabal install parallel
	cabal install tree-monad
	cabal install parallel-tree-search
	cabal install mtl

.PHONY: clean
clean:
	rm -f *.log
	rm -f ${INSTALLHS} ${INSTALLCURRY}
	cd src   ; ${MAKE} clean
	@if [ -d lib/.curry/kics2 ] ; then cd lib/.curry/kics2 && rm -f *.hi *.o ; fi
	cd cpns  ; ${MAKE} clean
	cd tools ; ${MAKE} clean
	cd www   ; ${MAKE} clean

# clean everything (including compiler binaries)
.PHONY: cleanall
cleanall: clean
	bin/cleancurry -r
	rm -rf ${LOCALBIN}


###############################################################################
# Create distribution versions of the complete system as tar file kics2.tar.gz:

# temporary directory to create distribution version
KICS2DIST=/tmp/kics2
# repository with new front-end:
FRONTENDREPO=git://git-ps.informatik.uni-kiel.de/curry

# install the sources of the front end from its repository
.PHONY: frontendsources
frontendsources:
	if [ -d frontend ] ; then \
	 cd frontend/curry-base && git pull && cd ../curry-frontend && git pull ; \
	 else mkdir frontend && cd frontend && \
	      git clone ${FRONTENDREPO}/curry-base.git && \
	      git clone ${FRONTENDREPO}/curry-frontend.git ; fi

# generate a source distribution of KICS2:
.PHONY: dist
dist:
	rm -rf kics2.tar.gz ${KICS2DIST}           # remove old distribution
	git clone . ${KICS2DIST}                   # create copy of git version
	cd ${KICS2DIST} && ${MAKE} installscripts ; \
	# copy or install front end sources in distribution:
	if [ -d frontend ] ; then \
	  cp -pr frontend ${KICS2DIST} ; \
	else \
	  cd ${KICS2DIST} && ${MAKE} frontendsources ; \
	fi
	# generate compile and REPL in order to have the bootstrapped
	# Haskell translations in the distribution:
	cd bin/.local && cp kics2c ${KICS2DIST}/bin/.local # copy bootstrap compiler
	cd ${KICS2DIST} && ${MAKE} Compile         # translate compiler
	cd ${KICS2DIST} && ${MAKE} REPL            # translate REPL
	cd ${KICS2DIST} && ${MAKE} clean           # clean object files
	cd ${KICS2DIST} && ${MAKE} cleandist       # delete unnessary files
	# copy documentation:
	@if [ -f docs/Manual.pdf ] ; then cp docs/Manual.pdf ${KICS2DIST}/docs ; fi
	cat Makefile | sed -e "/distribution/,\$$d" | sed 's|^GLOBALINSTALL=.*$$|GLOBALINSTALL=yes|' > ${KICS2DIST}/Makefile
	cd /tmp && tar cf kics2.tar kics2 && gzip kics2.tar
	mv /tmp/kics2.tar.gz .
	chmod 644 kics2.tar.gz
	rm -rf ${KICS2DIST}
	@echo "----------------------------------------------------------------"
	@echo "Distribution kics2.tar.gz generated."

#
# Clean all files that should not be included in a distribution
#
.PHONY: cleandist
cleandist:
	rm -rf .git .gitignore bin/.gitignore
	cd frontend/curry-base     && rm -rf .git .gitignore dist
	cd frontend/curry-frontend && rm -rf .git .gitignore dist
	rm -rf bin # clean executables
	rm -rf docs/src
	rm -rf benchmarks papers talks tests examples experiments
	rm -f TODO compilerdoc.wiki testsuite/TODO

# publish the distribution files in the local web pages
HTMLDIR=${HOME}/public_html/kics2/download
.PHONY: publish
publish:
	cp kics2.tar.gz docs/INSTALL.html ${HTMLDIR}
	chmod -R go+rX ${HTMLDIR}
	@echo "Don't forget to run 'update-kics2' to make the update visible!"
