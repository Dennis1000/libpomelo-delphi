unit Pomelo.Lib;

{**
 * Copyright (c) 2014 NetEase, Inc. and other Pomelo contributors
 * MIT Licensed.
 *
 * Delphi Conversion 2014/08 Dennis D. Spreen (dennis@spreendigital.de)
 * http://blog.spreendigital.de/
 *}

interface

{$Z4}  // store delphi enums as 4 bytes

// ***********************************
// Windows declarations
// ***********************************
{$IF defined(MSWINDOWS)}
uses
  System.Classes, Pomelo.Jansson, Winapi.Windows, Winapi.WinSock;

type
  Psockaddr_in = PSockAddrIn;

const
  PomeloLibraryName = 'libpomelo.dll'; {Do not Localize}

// ***********************************
// MacOs & iOS declarations
// ***********************************
{$ELSEIF defined(MACOS)}
uses
  System.Classes, Pomelo.Jansson, System.IOUtils, System.SysUtils,  Posix.NetinetIn;

const
  {$IFDEF IOS}
    {$DEFINE STATICLIBRARY}
    {$IFDEF CPUARM} // iOS device
      PomeloLibraryName = 'libpomelo.a'; {Do not Localize}
      UvLibraryName = 'libuv.a'; {Do not Localize}
    {$ELSE} // iOS Simulator
      PomeloLibraryName = 'libpomelo_sim.a'; {Do not Localize}
      UvLibraryName = 'libuv_sim.a'; {Do not Localize}
    {$ENDIF}
  {$ELSE} // MacOS
    PomeloLibraryName = 'libpomelo.dylib'; {Do not Localize}
    UvLibraryName = 'libuv.dylib'; {Do not Localize}
  {$ENDIF}

// ***********************************
// Android declarations
// ***********************************
{$ELSEIF defined(ANDROID)}
uses
  System.Classes, System.IOUtils, System.SysUtils, Pomelo.Jansson, Posix.SysSocket;

type
  Psockaddr_in = Psockaddr_storage;

const
  PomeloLibraryName = 'libpomelo.so'; {Do not Localize}
{$ENDIF}

  PC_TYPE = 'c';
  PC_VERSION = '0.1.2';

  PC_EVENT_DISCONNECT = 'disconnect';
  PC_EVENT_TIMEOUT = 'timeout';
  PC_EVENT_KICK = 'onKick';
  PC_EVENT_RECONNECT = 'reconnect';

  PC_PROTO_VERSION = 'protoVersion';
  PC_PROTO_CLIENT = 'clientProtos';
  PC_PROTO_SERVER = 'serverProtos';

  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }

type
  uint32_t = uint32;
  ULONG = Cardinal;
  ULONG_PTR = NativeUInt;
  SIZE_T = ULONG_PTR;
  u_short = Word;
  u_long = Longint;
  u_char = Byte; //AnsiChar;
{
  TSunB = packed record
    s_b1, s_b2, s_b3, s_b4: Byte; //u_char;
  end;

  TSunW = packed record
    s_w1, s_w2: word; // u_short;
  end;

  PInAddr = ^TInAddr;
  in_addr = packed record
    case integer of
      0: (S_un_b: TSunB);
      1: (S_un_w: TSunW);
      2: (S_addr: LongWord);
  end;
  TInAddr = in_addr;

  PSockAddrIn = ^sockaddr_in;
  sockaddr_in = packed record
    sin_family: u_short;
    sin_family: u_short;
    sin_port: u_short;
    sin_addr: TSunB;
    sin_zero: array[0..7] of u_char;
  end;
}

  //TSockAddr_in = sockaddr_in;

  {**
   * It should be possible to cast uv_buf_t[] to WSABUF[]
   * see http://msdn.microsoft.com/en-us/library/ms741542(v=vs.85).aspx
   *}
  uv_buf_t = record
    ln: ULONG;
    base: MarshaledAString;
  end;

{**
 * ngx-queue.h
 *}
  ngx_queue_s = record
    prev: Pointer; //ngx_queue_t  *prev;
    next: Pointer; //ngx_queue_t  *next;
  end;
  ngx_queue_t = ngx_queue_s;

{**
 * pomelo.h
 *}
  Ppc_client_t = ^pc_client_t;
  pc_buf_t = uv_buf_t;
  Ppc_buf_t = ^pc_buf_t;
  Ppc_connect_t = ^pc_connect_t;
  Ppc_request_t = ^pc_request_t;
  Ppc_notify_t = ^pc_notify_t;
  Ppc_msg_t = ^pc_msg_t;

{**
 * State machine for Pomelo package parser
 *}
  pc_pkg_parser_state =(
    PC_PKG_HEAD = 1,        // parsing header
    PC_PKG_BODY,            // parsing body
    PC_PKG_CLOSED);

{**
 * Package type of Pomelo package
 *}
  pc_pkg_type_e = (
    PC_PKG_HANDSHAKE = 1,
    PC_PKG_HANDSHAKE_ACK,
    PC_PKG_HEARBEAT,
    PC_PKG_DATA,
    PC_PKG_KICK);

{**
 * Pomelo client states.
 *}
 pc_client_state = (
    PC_ST_INITED = 1,
    PC_ST_CONNECTING,
    PC_ST_CONNECTED,
    PC_ST_WORKING,
    PC_ST_DISCONNECTING,
    PC_ST_CLOSED);

{**
 * Pomelo client async request types.
 *}
  pc_req_type = (
    PC_CONNECT_,
    PC_REQUEST_,
    PC_NOTIFY_);

{**
 * State of transport.
 *}
  pc_transport_state = (
    PC_TP_ST_INITED = 1,
    PC_TP_ST_CONNECTING,
    PC_TP_ST_WORKING,
    PC_TP_ST_CLOSED);

{**
 * operation for proto files.
 *}
  pc_proto_op = (
    PC_PROTO_OP_READ = 1,
    PC_PROTO_OP_WRITE,
    PC_PROTO_OP_UNKONWN);

{**
 * Callbacks
 *}

{**
 * Event callback.
 *
 * @param client client instance that fire the event.
 * @param event event name that registered before.
 * @param data attach data of the event.
 *}
  pc_event_cb = procedure(client: Ppc_client_t; const char: MarshaledAString; data: Pointer); cdecl;

{**
 * Connection established callback.
 *
 * @param req connect request.
 * @param status connect status. 0 for ok and -1 for error.
 *}
  pc_connect_cb = procedure(req: Ppc_connect_t; status: Integer); cdecl;

{**
 * Request callback.
 *
 * @param  req    request instance.
 * @param  status request status. 0 for ok and -1 for error.
 * @param  resp   response message from server, NULL for error.
 *}
  pc_request_cb = procedure(req: Ppc_request_t; status: Integer; resp: Pjson_t);cdecl;

{**
 * Notify callback.
 *
 * @param req request instance.
 * @param status notify status. o for ok and -1 for error.
 *}
  pc_notify_cb = procedure(req: Ppc_notify_t; status: Integer); cdecl;

{**
 * Handshake callback for client which would be fired during handshake phase and
 * passing the customized handshake information from server.
 *
 * @param client client instance.
 * @param msg customized handshake information from server.
 * @return 0 for ok and -1 for error and terminate the connection.
 *}
  pc_handshake_cb = function(client: Ppc_client_t; msg: Pjson_t): Integer; cdecl;

{**
 * Message parse callback which would be fired when a new message arrived.
 *
 * @param client client instance.
 * @param data original message data in bytes.
 * @param len length of the data.
 * @return the parse result or NULL for error.
 *}
  pc_msg_parse_cb = function(client: Ppc_client_t; data: MarshaledAString; len: SIZE_T): Ppc_msg_t; cdecl;

{**
 * Message parse done callback which would be fired when the the message has
 * processed to release the resources created in the message parsing phase.
 *
 * @param client client instance.
 * @param msg message instance.
 *}
  pc_msg_parse_done_cb = procedure(client: Ppc_client_t; msg: Ppc_msg_t); cdecl;

{**
 * Message encode callback which would be fired when a new request or notify is
 * emitted. This is the place to customized the message layer encode and the
 * result would be delivered on the Pomelo package layer.
 *
 * @param client client instance.
 * @param reqId request id, positive for request and 0 for notify.
 * @param route route string.
 * @param msg message content.
 * @return encode result, buf.len = -1 for error.
 *}
  pc_msg_encode_cb = function(client: Ppc_client_t; reqId: Uint32_t; route: MarshaledAString; msg: Pjson_t): Ppc_buf_t; cdecl;

{**
 * Message encode done callback which would be fired when the encode data has
 * been delivered or meeting some error to release the resources created during
 * the encode phase.
 *
 * @param client client instance.
 * @param buf encode result.
 *}
  pc_msg_encode_done_cb = procedure(client: Ppc_client_t; buf: pc_buf_t); cdecl;

  pc_proto_cb = procedure(client: Ppc_client_t; op: pc_proto_op; fileName: MarshaledAString; data: Pointer); cdecl;

{**
 * Simple structure for memory block.
 * The pc_buf_s is cheap and could be passed by value.
 *}
  pc_buf_s = record
    base: MarshaledAString;
    len: SIZE_T;
  end;

{**
 * Transport structure.
 *}
  pc_transport_t = record
    client: Ppc_client_t;
    socket: Pointer; //uv_tcp_t *socket;
    state: pc_transport_state;
  end;

  PC_REQ_FIELDS = record
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
  end;

  PC_TCP_REQ_FIELDS = record
    {* public *}
    route: MarshaledAString;
    msg: Pjson_t;
  end;

{**
 * The abstract base class of all async request in Pomelo client.
 *}
  pc_req_s = record
    //PC_REQ_FIELDS
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
  end;

{**
 * The abstract base class of all tcp async request and a subclass of pc_req_t.
 *}
  pc_tcp_req_s = record
   //PC_REQ_FIELDS
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
   //PC_TCP_REQ_FIELDS
    {* public *}
    route: MarshaledAString;
    msg: Pjson_t;
  end;

{**
 * Pomelo client instance
 *}
  pc_client_s = record
    {* public *}
    state: pc_client_state;
    {* private *}
    PrivateFields: Array[1..344 - SizeOf(pc_client_state)] of Byte;
  end;
  pc_client_t = pc_client_s;

{**
 * Connect request class is a subclass of pc_req_t.
 * Connect is the async context for a connection request to server.
 *}
  pc_connect_s = record
    // PC_REQ_FIELDS
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
    {* public *}
    address: Psockaddr_in;
    cb: pc_connect_cb;
    {* private *}
    socket: Pointer; //uv_tcp_t *socket;
  end;
  pc_connect_t = pc_connect_s;

{**
 * Pomelo request class is a subclass of pc_tcp_req_t.
 * Request is the async context for a Pomelo request to server.
 *}
  pc_request_s  = record
    //PC_REQ_FIELDS;
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
    //PC_TCP_REQ_FIELDS
    {* public *}
    route: MarshaledAString;
    msg: Pjson_t;
    //
    id: Uint32_t;
    cb: pc_request_cb;
    queue: ngx_queue_t;
  end;
  pc_request_t = pc_request_s;

{**
 * Pomelo notify class is a subclass of pc_tcp_req_t.
 * Notify is the async context for a Pomelo notify to server.
 *}
  pc_notify_s = record
    //PC_REQ_FIELDS;
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
    //PC_TCP_REQ_FIELDS
    {* public *}
    route: MarshaledAString;
    msg: Pjson_t;
    //
    cb: pc_notify_cb;
  end;
  pc_notify_t = pc_notify_s;

{**
 * Message structure.
 *}
  pc_msg_s = record
    id: uint32_t;
    route: MarshaledAString;
    msg: Pjson_t;
  end;
  pc_msg_t = pc_msg_s;

// Use procedure entries as variables (only if dynamic library)
{$IFNDEF STATICLIBRARY}
var
{$ENDIF}

{**
 * Create and initiate Pomelo client intance.
 *
 * @return Pomelo client instance
 *}
{$IFDEF STATICLIBRARY}
  function pc_client_new: Ppc_client_t; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_client_new: function: Ppc_client_t; cdecl;
{$ENDIF}

{**
 * Create and init Pomelo client instance with reconnect enable
 *
 * @param delay delay time in second
 * @param delay_max the max delay time in second
 * @param exp_backoff whether enable exponetial backoff
 *
 * For example, if 2 -> delay, 10 -> delay_max, then the reconnect delay will be
 * 2, 4, 6, 8, 10, 10, 10 seconds...
 * if 2 -> delay, 30 -> delay_max enable exponetial backoff, the reconnect delay will be
 * 2, 4, 8, 16, 30, 30 seconds...
 *}
{$IFDEF STATICLIBRARY}
  function pc_client_new_with_reconnect(delay, delay_max, exp_backoff: Integer): Ppc_client_t; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_client_new_with_reconnect: function(delay, delay_max, exp_backoff: Integer): Ppc_client_t; cdecl;
{$ENDIF}

{**
 * Disconnect Pomelo client and reset all status back to initialted.
 *
 * @param client Pomelo client instance.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_client_disconnect(client: Ppc_client_t); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_client_disconnect: procedure(client: Ppc_client_t); cdecl;
{$ENDIF}

{**
 * Stop the connection of the client. It is suitable for calling in the child
 * thread and the main thread called the pc_client_join funtion the wait the
 * worker child thread return.
 *
 * @param client client instance.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_client_stop(client: Ppc_client_t); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_client_stop: procedure(client: Ppc_client_t); cdecl;
{$ENDIF}

{**
 * Destroy and disconnect the connection of the client instance.
 *
 * @param client client instance.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_client_destroy(client: Ppc_client_t); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_client_destroy: procedure(client: Ppc_client_t); cdecl;
{$ENDIF}

{**
 * Join and wait the worker child thread return. It is suitable for the
 * situation that the main thread has nothing to do after the connction
 * established.
 *
 * @param client client instance.
 * @return 0 for ok or error code for error.
 *}
{$IFDEF STATICLIBRARY}
  function pc_client_join(client: Ppc_client_t): Integer cdecl; external PomeloLibraryName;
{$ELSE}
  pc_client_join: function(client: Ppc_client_t): Integer cdecl;
{$ENDIF}

{**
 * Create and initiate a request instance.
 *
 * @return req request instance
 *}
{$IFDEF STATICLIBRARY}
  function pc_request_new: Ppc_request_t cdecl; external PomeloLibraryName;
{$ELSE}
  pc_request_new: function: Ppc_request_t cdecl;
{$ENDIF}

{**
 * Destroy and release inner resource of a request instance.
 *
 * @param req request instance to be destroied.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_request_destroy(req: Ppc_request_t) cdecl; external PomeloLibraryName;
{$ELSE}
  pc_request_destroy: procedure(req: Ppc_request_t) cdecl;
{$ENDIF}

{**
 * Connect the client to the server which would create a worker child thread
 * and connect to the server.
 *
 * @param client client instance.
 * @param addr server address.
 * @return 0 or -1.
 *}
{$IFDEF STATICLIBRARY}
  function pc_client_connect(client: Ppc_client_t; addr: Psockaddr_in): Integer; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_client_connect: function(client: Ppc_client_t; addr: Psockaddr_in): Integer; cdecl;
{$ENDIF}

{**
 * Connect the client to server just like pc_client_connect,
 * except that it's the asynchronous version for it.
 * The user should be responsible to conn_req's allocation, initialization and reclamation
 *
 * @param client client instance
 * @param conn_req connect request which are allocated and initialized by pc_connect_req_new
 * @return 0 or -1
 *}
{$IFDEF STATICLIBRARY}
  function pc_client_connect2(client: Ppc_client_t; conn_req: Ppc_connect_t; cb: pc_connect_cb): Integer; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_client_connect2: function(client: Ppc_client_t; conn_req: Ppc_connect_t; cb: pc_connect_cb): Integer; cdecl;
{$ENDIF}

{**
 *
 * Use for async connection
 *
 * @param addr address to which the connection is made
 * @return an instance of pc_connect_t, which should be released manually by user.
 *}
{$IFDEF STATICLIBRARY}
  function pc_connect_req_new(addr: Psockaddr_in): Ppc_connect_t; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_connect_req_new: function(addr: Psockaddr_in): Ppc_connect_t; cdecl;
{$ENDIF}

{**
 * Destroy instance of pc_connect_t
 *
 * @param conn_req pc_connect_t instance
 * @return none
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_connect_req_destroy(conn_req: Ppc_connect_t); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_connect_req_destroy: procedure(conn_req: Ppc_connect_t); cdecl;
{$ENDIF}

{**
 * Send rerquest to server.
 * The message object and request object must keep
 * until the pc_request_cb invoked.
 *
 * @param client Pomelo client instance
 * @param req initiated request instance
 * @param route route string
 * @param msg message object
 * @param cb request callback
 * @return 0 or -1
 *}
{$IFDEF STATICLIBRARY}
  function pc_request(client: Ppc_client_t; req: Ppc_request_t; const route: MarshaledAString;
    msg: Pjson_t; cb: pc_request_cb): Integer; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_request: function(client: Ppc_client_t; req: Ppc_request_t; const route: MarshaledAString;
    msg: Pjson_t; cb: pc_request_cb): Integer; cdecl;
{$ENDIF}

{**
 * Create and initiate notify instance.
 *
 * @return notify instance
 *}
{$IFDEF STATICLIBRARY}
  function pc_notify_new: Ppc_notify_t; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_notify_new: function: Ppc_notify_t; cdecl;
{$ENDIF}

{**
 * Destroy and release inner resource of a notify instance.
 *
 * @param req notify instance to be destroied.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_notify_destroy(req: Ppc_notify_t); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_notify_destroy: procedure(req: Ppc_notify_t); cdecl;
{$ENDIF}

{**
 * Send notify to server.
 * The message object and notify object must keep
 * until the pc_notify_cb invoked.
 *
 * @param client Pomelo client instance
 * @param req initiated notify instance
 * @param route route string
 * @param msg message object
 * @param cb notify callback
 * @return 0 or -1
 *}
{$IFDEF STATICLIBRARY}
  function pc_notify(client: Ppc_client_t; req: Ppc_notify_t; const route: MarshaledAString;
    msg: Pjson_t; cb: pc_notify_cb): Integer; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_notify: function(client: Ppc_client_t; req: Ppc_notify_t; const route: MarshaledAString;
    msg: Pjson_t; cb: pc_notify_cb): Integer; cdecl;
{$ENDIF}

{**
 * Register a listener in the client.
 *
 * @param client client instance.
 * @param event event name.
 * @param event_cb event callback.
 * @return 0 or -1.
 *}
{$IFDEF STATICLIBRARY}
  function pc_add_listener(client: Ppc_client_t; const event: MarshaledAString;
    event_cb: pc_event_cb): Integer; cdecl; external PomeloLibraryName;
{$ELSE}
  pc_add_listener: function(client: Ppc_client_t; const event: MarshaledAString;
    event_cb: pc_event_cb): Integer; cdecl;
{$ENDIF}

{**
 * Remove a listener in the client.
 *
 * @param client client instance.
 * @param event event name.
 * @param event_cb event callback.
 * @return void.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_remove_listener(client: Ppc_client_t; const event: MarshaledAString;
    event_cb: pc_event_cb); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_remove_listener: procedure(client: Ppc_client_t; const event: MarshaledAString;
    event_cb: pc_event_cb); cdecl;
{$ENDIF}

{**
 * Emit a event from the client.
 *
 * @param client client instance.
 * @param event event name.
 * @param data attach data of the event.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_emit_event(client: Ppc_client_t; const event: MarshaledAString; data: Pointer); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_emit_event: procedure(client: Ppc_client_t; const event: MarshaledAString; data: Pointer); cdecl;
{$ENDIF}

{**
 * jansson memory malloc, free self-defined function.
 *
 * @param malloc_fn malloc function.
 * @param free_fn free function.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_json_set_alloc_funcs(malloc_fn: Pointer; free_fn: Pointer); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_json_set_alloc_funcs: procedure(malloc_fn: Pointer; free_fn: Pointer); cdecl;
{$ENDIF}

{**
 * Init protobuf settings, set the read/write proto files directorys
 *
 * @param client client instance.
 * @param proto_read_dir directory of proto files to read.
 * @param proto_write_dir directory of proto files to write.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_proto_init(client: Ppc_client_t; const proto_read_dir: MarshaledAString; const proto_write_dir: MarshaledAString); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_proto_init: procedure(client: Ppc_client_t; const proto_read_dir: MarshaledAString; const proto_write_dir: MarshaledAString); cdecl;
{$ENDIF}

{**
 * Init protobuf settings, set the callback for read/write proto files
 *
 * @param client client instance.
 * @param proto_cb callback when read or write proto files.
 *}
{$IFDEF STATICLIBRARY}
  procedure pc_proto_init2(client: Ppc_client_t; proto_cb: pc_proto_cb); cdecl; external PomeloLibraryName;
  procedure pc_proto_copy(client: Ppc_client_t; proto_ver: Pjson_t; client_protos: Pjson_t; server_protos: Pjson_t); cdecl; external PomeloLibraryName;
{$ELSE}
  pc_proto_init2: procedure(client: Ppc_client_t; proto_cb: pc_proto_cb); cdecl;
  pc_proto_copy: procedure(client: Ppc_client_t; proto_ver: Pjson_t; client_protos: Pjson_t; server_protos: Pjson_t); cdecl;
{$ENDIF}

var
  ErrorLog: TStringList; // LoadLibrary ErrorLog

implementation

{$IFDEF STATICLIBRARY}
// Assure UV and Jansson are linked before
function uv_version: Integer; cdecl; external UvLibraryName;
function json_object: Pointer; cdecl; external JanssonLibraryName;
{$ENDIF}

var
  LibraryHandle: THandle;

function GetLibraryFolder: String;
begin
{$IFDEF MSWINDOWS}
  Result := '';
{$ELSE}
  Result := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath);
{$ENDIF}
  //Result := '@executable_path/';
end;

function LibraryAvailable: Boolean;
begin
  result := LibraryHandle <> 0;
end;

function GetAddress(Name: String): Pointer;
begin
  Result := GetProcAddress(LibraryHandle, PWideChar(Name));
  if Result = NIL then
    ErrorLog.Add('Entry point "' + Name + '" not found');
end;

{$IFNDEF STATICLIBRARY}
procedure LoadExternal;
begin
  LibraryHandle := LoadLibrary(PChar(GetLibraryFolder + PomeloLibraryName));
  if not LibraryAvailable then
  begin
    ErrorLog.Add('Library "' + GetLibraryFolder + PomeloLibraryName + '" not found');
    Exit;
  end;

  pc_client_new := GetAddress('pc_client_new');
  pc_client_new_with_reconnect := GetAddress('pc_client_new_with_reconnect');
  pc_client_disconnect := GetAddress('pc_client_disconnect');
  pc_client_stop := GetAddress('pc_client_stop');
  pc_client_destroy := GetAddress('pc_client_destroy');
  pc_client_join := GetAddress('pc_client_join');
  pc_request_new := GetAddress('pc_request_new');
  pc_request_destroy := GetAddress('pc_request_destroy');
  pc_client_connect := GetAddress('pc_client_connect');
  pc_client_connect2 := GetAddress('pc_client_connect2');
  pc_connect_req_new := GetAddress('pc_connect_req_new');
  pc_connect_req_destroy := GetAddress('pc_connect_req_destroy');
  pc_request := GetAddress('pc_request');
  pc_notify_new := GetAddress('pc_notify_new');
  pc_notify_destroy := GetAddress('pc_notify_destroy');
  pc_notify := GetAddress('pc_notify');
  pc_add_listener := GetAddress('pc_add_listener');
  pc_remove_listener := GetAddress('pc_remove_listener');
  pc_emit_event := GetAddress('pc_emit_event');
  pc_json_set_alloc_funcs := GetAddress('pc_json_set_alloc_funcs');
  pc_proto_init := GetAddress('pc_proto_init');
  pc_proto_init2 := GetAddress('pc_proto_init2');
  pc_proto_copy := GetAddress('pc_proto_copy');
end;
{$ENDIF}


initialization
  LibraryHandle := 0;
  ErrorLog := TStringList.Create;
  {$IFNDEF STATICLIBRARY}
  LoadExternal;
  {$ENDIF}

finalization
  ErrorLog.Free;
  if LibraryHandle <> 0 then
    FreeLibrary(LibraryHandle);
end.

