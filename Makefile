# If you're on Windows, run this with Cygwin.

WINDOWS = Windows
OSX = OSX
LINUX = LINUX

ifeq ($(OS),Windows_NT)
	SHELL_OS = $(WINDOWS)
	CYGAPPDATA = $(shell cygpath "$(APPDATA)")
else
	UNAME_S = $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		SHELL_OS = $(LINUX)
	else ifeq ($(UNAME_S),Darwin)
		SHELL_OS = $(OSX)
	else
		SHELL_OS = Other
	endif
endif

boot:
	cabal configure
	cabal build
	cabal install --force-reinstalls

clean:
	cabal clean

# TODO: Debug 'Error while removing dist/: DeleteFile "dist\\setup\\setup.exe": permission denied (Access is denied.)'. For now, work around with "rm -rf dist".
uninstall: clean
	ghc-pkg unregister hermit-bluetooth
ifeq ($(SHELL_OS),$(WINDOWS))
	rm -rf dist
	( cd $(CYGAPPDATA)/cabal/bin; rm hermit-bluetooth.exe wsa_utils.dll )
else ifeq ($(SHELL_OS),$(OSX))
	echo Hi, Mac users! Be patient, I'll get to you eventually.
else ifeq ($(SHELL_OS),$(LINUX))
	( cd ~/.cabal/bin; rm hermit-bluetooth )
endif