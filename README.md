hermit-bluetooth
================

An attempt to make Haskell, Android, and Bluetooth have a group conversation.

Windows
=======

### Can I run it?
If your computer has Bluetooth support, it should work. If you're not sure if your computer supports Bluetooth, try installing `hermit-bluetooth`. If one of these two things happen, then you probably don't have Bluetooth support.

* You run `hermit-bluetooth` on cmd.exe, PowerShell, or some other native Windows console and nothing happens (it should print some debugging information on startup).
* You run `hermit-bluetooth` on Cygwin or some other Unix-like terminal and receive an error message:

```
$ hermit-bluetooth
.../AppData/Roaming/cabal/bin/hermit-bluetooth.exe: error while loading shared libraries: ?: cannot open shared object file: No such file or directory
```

Of course, this could also be caused by the DLL file not finding the needed libraries correctly. If you suspect this is the case, file a bug report.

### Can I build it?
I have successfully built `hermit-bluetooth` on my Windows 7 computer. I have not tested on any other Windows computer, so if you have access to some Windows machine, I'd appreciate it if you attempted to test it (and file a bug report if stuff breaks).

I have included the Visual Studio project used to compile `wsa_utils.dll` under `lib/wsa_utils`. You do not need Visual Studio to rebuild `wsa_utils.dll`. Instead, you can run `MSBuild.exe` in the `lib/wsa_utils` directory. `MSBuild.exe` is bundled with the [.NET Framework](http://www.microsoft.com/net); on my computer, it resides in `C:\Users\Program Files (x86)\MSBuild\12.0\Bin`, but I've seen it live in `C:\Users\Program Files (x86)\Microsoft.NET\...` too.

Mac OS X
========
Under construction. I first need to acquire a Mac.

Linux
=====
Make sure you have the BlueZ development libraries installed (on Ubuntu, this is libbluetooth-dev).