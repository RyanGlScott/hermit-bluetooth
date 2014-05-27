# If you're on Windows, I encourage you to run this with Cygwin.
boot:
	cabal configure
	cabal build
	cabal install --force-reinstalls

# TODO: Debug 'Error while removing dist/: DeleteFile "dist\\setup\\setup.exe": permission denied (Access is denied.)'

clean:
	cabal clean

# TODO: Remove hermit-bluetooth.exe (and wsa_utils.dll, if on Windows)
uninstall: clean
	ghc-pkg unregister hermit-bluetooth