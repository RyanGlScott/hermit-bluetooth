# If you're on Windows, run this with Cygwin.

ifeq ($(OS),Windows_NT)
	SHELL_OS = Windows
	CYGAPPDATA = $(shell cygpath "$(APPDATA)")
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		SHELL_OS = Linux
	else ifeq ($(UNAME_S),Darwin)
		SHELL_OS = OSX
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
ifeq ($(SHELL_OS),Windows)
	rm -rf dist
	( cd $(CYGAPPDATA)/cabal/bin; rm hermit-bluetooth.exe wsa_utils.dll )
else ifeq ($(SHELL_OS),OSX)
	echo Hi, Mac users! Be patient, I'll get to you eventually.
else ifeq ($(SHELL_OS),Linux)
	( cd ~/.cabal/bin; rm hermit-bluetooth )
endif