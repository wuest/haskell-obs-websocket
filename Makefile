.PHONY: clean all dist test
.DEFAULT_GOAL := all

SRC_DIR     := "src"
HS_SOURCES  := $(shell find $(SRC_DIR) -name '*.hs')
CABAL_FILE  := $(shell find . -name '*.cabal')
HS_VERSION  := $(shell ghc --version|sed 's/.*version /ghc-/')
APP_VERSION := $(shell grep '^version:' *.cabal|sed 's/.* //')
APP_NAME    := $(shell grep '^executable .' *.cabal|sed 's/.* //'|head -1)
PROJ_NAME   := $(shell grep '^name: *.' *.cabal|sed 's/.* //'|head -1)
APP_ARCH    := $(shell uname -m)
APP_OS      := $(shell uname -s|tr 'A-Z' 'a-z')

TARGET_BASE := dist-newstyle/build/$(APP_ARCH)-$(APP_OS)/$(HS_VERSION)/$(PROJ_NAME)-$(APP_VERSION)/x/$(APP_NAME)/build/$(APP_NAME)
TARGET      := $(TARGET_BASE)/$(APP_NAME)

.DEFAULT_GOAL := all

all: $(TARGET)

$(TARGET): $(CABAL_FILE) $(HS_SOURCES)
	cabal v2-build

dist: $(TARGET)
	cabal v2-sdist

test:
	cabal v2-test

clean:
	rm -rf dist*

run: $(TARGET)
	$(TARGET)
