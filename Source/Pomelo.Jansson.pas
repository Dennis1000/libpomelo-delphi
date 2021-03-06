unit Pomelo.Jansson;

{*
 * Copyright (c) 2009-2012 Petri Lehtinen <petri@digip.org>
 *
 * Jansson is free software; you can redistribute it and/or modify
 * it under the terms of the MIT license. See LICENSE for details.
 *
 * Minimal Delphi Conversion 2014/08 Dennis D. Spreen (dennis@spreendigital.de)
 * http://blog.spreendigital.de/
 *}

interface

{$Z4}  // store delphi enums as 4 bytes

// ***********************************
// Windows declarations
// ***********************************
{$IF defined(MSWINDOWS)}
uses
  System.Classes, WinApi.Windows;

const
  JanssonLibraryName = 'jansson.dll'; {Do not Localize}

// ***********************************
// MacOs & iOS declarations
// ***********************************
{$ELSEIF defined(MACOS)}
uses
  System.Classes, System.IOUtils, System.SysUtils;

const
  {$IFDEF IOS}
    {$DEFINE STATICLIBRARY}
    {$IFDEF CPUARM} // iOS device
      JanssonLibraryName = 'libjansson.a'; {Do not Localize}
    {$ELSE} // iOS Simulator
      JanssonLibraryName = 'libjansson_sim.a'; {Do not Localize}
    {$ENDIF}
  {$ELSE} // MacOS
    JanssonLibraryName = 'libjansson.dylib'; {Do not Localize}
  {$ENDIF}

// ***********************************
// Android declarations
// ***********************************
{$ELSEIF defined(ANDROID)}
uses
  System.Classes, System.IOUtils, System.SysUtils;

const
  JanssonLibraryName = 'libjansson.so'; {Do not Localize}
{$ENDIF}

{* version *}

  JANSSON_MAJOR_VERSION = 2;
  JANSSON_MINOR_VERSION = 4;
  JANSSON_MICRO_VERSION = 99;

{* Micro version is omitted if it's 0 *}
  JANSSON_VERSION = '2.5-dev';

{* Version as a 3-byte hex number, e.g. 0x010201 == 1.2.1. Use this
   for numeric comparisons, e.g. #if JANSSON_VERSION_HEX >= ... *}
  JANSSON_VERSION_HEX = ((JANSSON_MAJOR_VERSION SHL 16) OR
                         (JANSSON_MINOR_VERSION SHL 8) OR
                         (JANSSON_MICRO_VERSION SHL 0));

{* types *}

type
  ULONG_PTR = NativeUInt;
  SIZE_T = ULONG_PTR;


  json_type = (
    tJSON_OBJECT,
    tJSON_ARRAY,
    tJSON_STRING,
    tJSON_INTEGER,
    tJSON_REAL,
    tJSON_TRUE,
    tJSON_FALSE,
    ttSON_NULL);

  json_t = record
    type_: JSON_TYPE;
    refcount: SIZE_T;
  end;
  Pjson_t = ^json_t;

{* construction, destruction, reference counting *}

{$IFNDEF STATICLIBRARY}
var
{$ENDIF}

{$IFDEF STATICLIBRARY}
  function json_object: Pjson_t; cdecl; external JanssonLibraryName;
{$ELSE}
  json_object: function: Pjson_t; cdecl;
{$ENDIF}

{$IFDEF STATICLIBRARY}
  function json_string(const value: MarshaledAString): Pjson_t; cdecl; external JanssonLibraryName;
{$ELSE}
  json_string: function(const value: MarshaledAString): Pjson_t; cdecl;
{$ENDIF}

{$IFDEF STATICLIBRARY}
  function json_string_value(string_: Pjson_t): MarshaledAString; cdecl; external JanssonLibraryName;
{$ELSE}
  json_string_value: function(string_: Pjson_t): MarshaledAString; cdecl;
{$ENDIF}

{$IFDEF STATICLIBRARY}
  function json_object_set_new(object_: Pjson_t; const key: MarshaledAString; value: PJson_t): Integer; cdecl; external JanssonLibraryName;
{$ELSE}
  json_object_set_new: function(object_: Pjson_t; const key: MarshaledAString; value: PJson_t): Integer; cdecl;
{$ENDIF}

{* getters, setters, manipulation *}
{$IFDEF STATICLIBRARY}
  function json_object_get(object_: Pjson_t; const key: MarshaledAString): Pjson_t; cdecl; external JanssonLibraryName;
{$ELSE}
  json_object_get: function(object_: Pjson_t; const key: MarshaledAString): Pjson_t; cdecl;
{$ENDIF}

{* do not call json_delete directly *}
{$IFDEF STATICLIBRARY}
  procedure json_delete(json: Pjson_t); cdecl; external JanssonLibraryName;
{$ELSE}
  json_delete: procedure(json: Pjson_t); cdecl;
{$ENDIF}

{$IFDEF STATICLIBRARY}
  function json_dumps(json: Pjson_t; flags: SIZE_T): MarshaledAString; cdecl; external JanssonLibraryName;
{$ELSE}
  json_dumps: function(json: Pjson_t; flags: SIZE_T): MarshaledAString; cdecl;
{$ENDIF}

function json_object_set(object_: Pjson_t; const key: MarshaledAString; value: Pjson_t): Integer; inline;
function json_incref(json: Pjson_t): Pjson_t; inline;
procedure json_decref(json: Pjson_t); inline;

var
  JanssonErrorLog: TStringList; // LoadLibrary ErrorLog

implementation

function json_object_set(object_: Pjson_t; const key: MarshaledAString; value: Pjson_t): Integer; inline;
begin
  Result := json_object_set_new(object_, key, json_incref(value));
end;

function json_incref(json: Pjson_t): Pjson_t; inline;
begin
  if Assigned(json) and (json.refcount <> High(SIZE_T) - 1)  then
    Inc(json.refcount);
  Result := json;
end;

procedure json_decref(json: Pjson_t); inline;
begin
  if Assigned(json) and (json.refcount <> High(SIZE_T)-1) then
  begin
    Dec(json.refcount);
    if json.refcount = 0 then
      json_delete(json);
  end;
end;

var
  LibraryHandle: THandle;

function GetLibraryFolder: String;
begin
{$IFDEF MSWINDOWS}
  Result := '';
{$ELSE}
  Result := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath);
{$ENDIF}
end;

function LibraryAvailable: Boolean;
begin
  result := LibraryHandle <> 0;
end;

function GetAddress(Name: String): Pointer;
begin
  Result := GetProcAddress(LibraryHandle, PWideChar(Name));
  if Result = NIL then
    JanssonErrorLog.Add('Entry point "' + Name + '" not found');
end;


{$IFNDEF STATICLIBRARY}
procedure LoadExternal;
begin
  LibraryHandle := LoadLibrary(PChar(GetLibraryFolder + JanssonLibraryName));
  if not LibraryAvailable then
    Exit;

  json_object := GetAddress('json_object');
  json_string := GetAddress('json_string');
  json_delete := GetAddress('json_delete');
  json_object_set_new := GetAddress('json_object_set_new');
  json_string_value := GetAddress('json_string_value');
  json_object_get := GetAddress('json_object_get');
  json_dumps := GetAddress('json_dumps');
end;
{$ENDIF}

initialization
  LibraryHandle := 0;
  JanssonErrorLog := TStringList.Create;
  {$IFNDEF STATICLIBRARY}
  LoadExternal;
  {$ENDIF}

finalization
  JanssonErrorLog.Free;
  if LibraryHandle <> 0 then
    FreeLibrary(LibraryHandle);
end.

