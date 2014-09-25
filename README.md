libpomelo-delphi
================

A delphi interface for the libpomelo client SDK.

##Dependencies

* [libpomelo](https://github.com/NetEase/libpomelo) - a C language client SDK for Pomelo.
* [jansson](https://github.com/akheron/jansson) - JSON encode and decode library.
* [tpc-pomelo](https://github.com/changchang/tcp-pomelo) - Tcp server for test.

 
## Examples

You'll find the examples in \examples

##Build

###Prerequisite

Open git bash, switch to your project directory and type in

```
git clone https://github.com/NetEase/libpomelo
cd libpomelo
mkdir build
git clone https://github.com/martine/gyp.git build/gyp
```    

then prepare the shared library with

```
build\gyp\gyp --depth=. -Dlibrary=shared_library pomelo.gyp -DTO=pc
``` 

open the pomelo.sln in Visual Studio and build the 'libpomelo' project. You'll then find the DLL files in the \default folder. Copy 'pomelo.dll' and 'jansson.dll' to your delphi pomelo project directory.

##Known Bugs
```pc_client_stop(Client)``` fails with an exception

##Notice
This interface is in beta state. Not all functions of libpomelo are made availabe for use. 
