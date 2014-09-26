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

uses
  Windows;

{$Z4}  // store delphi enums as 4 bytes

{* version *}

const
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

function json_object: Pjson_t; cdecl stdcall;
function json_string(const value: PAnsiChar): Pjson_t; cdecl stdcall;
function json_string_value(string_: Pjson_t): PAnsiChar; cdecl stdcall;

function json_object_set(object_: Pjson_t; const key: PAnsiChar; value: Pjson_t): Integer; inline;
function json_object_set_new(object_: Pjson_t; const key: PAnsiChar; value: PJson_t): Integer; cdecl stdcall;
function json_incref(json: Pjson_t): Pjson_t; inline;
procedure json_decref(json: Pjson_t); inline;

{* getters, setters, manipulation *}
function json_object_get(object_: Pjson_t; const key: PAnsiChar): Pjson_t; cdecl stdcall;

{* do not call json_delete directly *}
procedure json_delete(json: Pjson_t); cdecl stdcall;

implementation

function json_object_set(object_: Pjson_t; const key: PAnsiChar; value: Pjson_t): Integer; inline;
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

const
  DllName = 'jansson.dll';

function json_object; external DllName;
function json_string; external DllName;
procedure json_delete; external DllName;
function json_object_set_new; external DllName;
function json_string_value; external DllName;
function json_object_get; external DllName;

end.
