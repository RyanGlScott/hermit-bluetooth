hermit-bluetooth
================

An attempt to make Haskell, Android, and Bluetooth have a group conversation.

Windows
=======
I have successfully built `hermit-bluetooth` on my Windows 7 computer. I have not tested on any other Windows computer, so if you have access to some Windows machine, I'd appreciate it if you attempted to test it (and file a bug report if stuff breaks).

I have included the Visual Studio project used to compile `wsa_utils.dll` under `lib/wsa_utils`. You do not need Visual Studio to rebuild `wsa_utils.dll`. Instead, you can run `MSBuild.exe` in the `lib/wsa_utils` directory. I believe `MSBuild.exe` is bundled with the [Windows SDK](http://www.microsoft.com/en-us/download/details.aspx?id=8279); on my computer, it resides in `C:\Users\Program Files (x86)\MSBuild\12.0\Bin`.

Mac OS X
========
Under construction. I first need to acquire a Mac.

Linux
=====
Make sure you have the BlueZ development libraries installed (on Ubuntu, this is libbluetooth-dev).