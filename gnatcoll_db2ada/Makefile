##############################################################################
##                                                                          ##
##                              GNATCOLL LIBRARY                            ##
##                                                                          ##
##                         Copyright (C) 2017, AdaCore.                     ##
##                                                                          ##
## This library is free software;  you can redistribute it and/or modify it ##
## under terms of the  GNU General Public License  as published by the Free ##
## Software  Foundation;  either version 3,  or (at your  option) any later ##
## version. This library is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN# ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            ##
##                                                                          ##
## As a special exception under Section 7 of GPL version 3, you are granted ##
## additional permissions described in the GCC Runtime Library Exception,   ##
## version 3.1, as published by the Free Software Foundation.               ##
##                                                                          ##
## You should have received a copy of the GNU General Public License and    ##
## a copy of the GCC Runtime Library Exception along with this program;     ##
## see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    ##
## <http://www.gnu.org/licenses/>.                                          ##
##                                                                          ##
##############################################################################

# Makefile targets
# ----------------
#
# Setup:                   make [VAR=VALUE] setup (see below)
# Build:                   make
# Install:                 make install

# Variables which can be set:
#
# General:
#
#   prefix        : root install directory
#   ENABLE_SHARED : yes / no (or empty)
#   BUILD         : DEBUG PROD
#   PROCESSORS    : nb parallel compilations (0 to use all cores)
#   TARGET        : target triplet for cross-compilation
#   INTEGRATED    : installs the project as part of the compiler installation;
#                   this adds NORMALIZED_TARGET subdir to prefix

# helper programs
CAT := cat
ECHO  := echo
WHICH := which

NAME=gnatcoll_db2ada

# check for out-of-tree build
SOURCE_DIR := $(dir $(MAKEFILE_LIST))
ifeq ($(SOURCE_DIR),./)
  RBD=
  GNATCOLL_GPR=$(NAME).gpr
  MAKEPREFIX=
else
  RBD=--relocate-build-tree
  GNATCOLL_GPR=$(SOURCE_DIR)/$(NAME).gpr
  MAKEPREFIX=$(SOURCE_DIR)/
endif

TARGET := $(shell gcc -dumpmachine)
NORMALIZED_TARGET := $(subst normalized_target:,,$(wordlist 6,6,$(shell gprconfig  --config=ada --target=$(TARGET) --mi-show-compilers)))
ifeq ($(NORMALIZED_TARGET),)
  $(error No toolchain found for target "$(TARGET)")
endif

prefix := $(dir $(shell $(WHICH) gnatls))..
GNATCOLL_VERSION := $(shell $(CAT) ../version_information)

BUILD         = PROD
PROCESSORS    = 0
BUILD_DIR     =
ENABLE_SHARED = yes
LIBRARY_TYPE  = relocatable
INTEGRATED    = no
TEMPLATES     = no

all: build

# Load current setup if any
-include makefile.setup

GTARGET=--target=$(NORMALIZED_TARGET)

ifeq ($(ENABLE_SHARED), yes)
   LIBRARY_TYPE=relocatable
else
   LIBRARY_TYPE=static
endif

ifeq ($(INTEGRATED), yes)
   integrated_install=/$(NORMALIZED_TARGET)
endif

GPR_VARS= \
	 -XLIBRARY_TYPE=$(LIBRARY_TYPE) \
	 -XXMLADA_BUILD=$(LIBRARY_TYPE) \
	 -XGNATCOLL_VERSION=$(GNATCOLL_VERSION) \
	 -XLIBRARY_TYPE=$(LIBRARY_TYPE) \
	 -XTEMPLATES=$(TEMPLATES) \
	 -XBUILD=$(BUILD)

# Used to pass extra options to GPRBUILD, like -d for instance
GPRBUILD_OPTIONS=

BUILDER=gprbuild -p -m $(GTARGET) $(RBD) -j$(PROCESSORS) $(GPR_VARS) \
	$(GPRBUILD_OPTIONS)
INSTALLER=gprinstall -p -f --target=$(TARGET) $(GPR_VARS) \
	$(RBD) --sources-subdir=include/$(NAME) --prefix=$(prefix)$(integrated_install)
CLEANER=gprclean -q $(GPR_VARS) $(RBD)
UNINSTALLER=$(INSTALLER) -p -f --install-name=$(NAME) --uninstall

#########
# build #
#########

build:
	$(BUILDER) $(GNATCOLL_GPR)

###########
# Install #
###########

uninstall:
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/$(NAME)))
	$(UNINSTALLER) $(GNATCOLL_GPR)
endif

install: uninstall
	$(INSTALLER) $(GNATCOLL_GPR)

###########
# Cleanup #
###########

clean:
	-$(CLEANER) $(GNATCOLL_GPR)

#########
# setup #
#########

.SILENT: setup

setup:
	$(ECHO) "prefix=$(prefix)" > makefile.setup
	$(ECHO) "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	$(ECHO) "INTEGRATED=$(INTEGRATED)" >> makefile.setup
	$(ECHO) "BUILD=$(BUILD)" >> makefile.setup
	$(ECHO) "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	$(ECHO) "TARGET=$(TARGET)" >> makefile.setup
	$(ECHO) "SOURCE_DIR=$(SOURCE_DIR)" >> makefile.setup
	$(ECHO) "GNATCOLL_VERSION=$(GNATCOLL_VERSION)" >> makefile.setup
	$(ECHO) "LIBRARY_TYPE=$(LIBRARY_TYPE)" >> makefile.setup
	$(ECHO) "TEMPLATES=$(TEMPLATES)" >> makefile.setup
