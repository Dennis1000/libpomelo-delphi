{**
 * Copyright (c) 2014 NetEase, Inc. and other Pomelo contributors
 * MIT Licensed.
 *
 * Delphi Conversion 2014/08 Dennis D. Spreen (dennis@spreendigital.de)
 * http://blog.spreendigital.de/
 *}

unit Pomelo.Lib;

interface

uses
  Windows, WinSock, Pomelo.Jansson;

const
  PC_TYPE = 'c';
  PC_VERSION = '0.1.2';

  PC_EVENT_DISCONNECT = 'disconnect';
  PC_EVENT_TIMEOUT = 'timeout';
  PC_EVENT_KICK = 'onKick';
  PC_EVENT_RECONNECT = 'reconnect';

  PC_PROTO_VERSION = 'protoVersion';
  PC_PROTO_CLIENT = 'clientProtos';
  PC_PROTO_SERVER = 'serverProtos';

type
  {$Z4}  // store delphi enums as 4 bytes
  uint32_t = uint32;

{**
 * uv-win.h
 *}
  uv_mutex_t = RTL_CRITICAL_SECTION;

  {**
   * It should be possible to cast uv_buf_t[] to WSABUF[]
   * see http://msdn.microsoft.com/en-us/library/ms741542(v=vs.85).aspx
   *}
  uv_buf_t = record
    ln: ULONG;
    base: PAnsiChar;
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
  pc_event_cb = procedure(client: Ppc_client_t; const char: PAnsiChar; data: Pointer); cdecl stdcall;

{**
 * Connection established callback.
 *
 * @param req connect request.
 * @param status connect status. 0 for ok and -1 for error.
 *}
  pc_connect_cb = procedure(req: Ppc_connect_t; status: Integer); cdecl stdcall;

{**
 * Request callback.
 *
 * @param  req    request instance.
 * @param  status request status. 0 for ok and -1 for error.
 * @param  resp   response message from server, NULL for error.
 *}
  pc_request_cb = procedure(req: Ppc_request_t; status: Integer; resp: Pjson_t);cdecl stdcall;

{**
 * Notify callback.
 *
 * @param req request instance.
 * @param status notify status. o for ok and -1 for error.
 *}
  pc_notify_cb = procedure(req: Ppc_notify_t; status: Integer); cdecl stdcall;

{**
 * Handshake callback for client which would be fired during handshake phase and
 * passing the customized handshake information from server.
 *
 * @param client client instance.
 * @param msg customized handshake information from server.
 * @return 0 for ok and -1 for error and terminate the connection.
 *}
  pc_handshake_cb = function(client: Ppc_client_t; msg: Pjson_t): Integer; cdecl stdcall;

{**
 * Message parse callback which would be fired when a new message arrived.
 *
 * @param client client instance.
 * @param data original message data in bytes.
 * @param len length of the data.
 * @return the parse result or NULL for error.
 *}
  pc_msg_parse_cb = function(client: Ppc_client_t; data: PAnsiChar; len: SIZE_T): Ppc_msg_t; cdecl stdcall;

{**
 * Message parse done callback which would be fired when the the message has
 * processed to release the resources created in the message parsing phase.
 *
 * @param client client instance.
 * @param msg message instance.
 *}
  pc_msg_parse_done_cb = procedure(client: Ppc_client_t; msg: Ppc_msg_t); cdecl stdcall;

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
  pc_msg_encode_cb = function(client: Ppc_client_t; reqId: Uint32_t; route: PAnsiChar; msg: Pjson_t): Ppc_buf_t; cdecl stdcall;

{**
 * Message encode done callback which would be fired when the encode data has
 * been delivered or meeting some error to release the resources created during
 * the encode phase.
 *
 * @param client client instance.
 * @param buf encode result.
 *}
  pc_msg_encode_done_cb = procedure(client: Ppc_client_t; buf: pc_buf_t); cdecl stdcall;

  pc_proto_cb = procedure(client: Ppc_client_t; op: pc_proto_op; fileName: PAnsiChar; data: Pointer); cdecl stdcall;

{**
 * Simple structure for memory block.
 * The pc_buf_s is cheap and could be passed by value.
 *}
  pc_buf_s = packed record
    base: PAnsiChar;
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

  PC_REQ_FIELDS = packed record
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
  end;

  PC_TCP_REQ_FIELDS = packed record
    {* public *}
    route: PAnsiChar;
    msg: Pjson_t;
  end;

{**
 * The abstract base class of all async request in Pomelo client.
 *}
  pc_req_s = packed record
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
  pc_tcp_req_s = packed record
   //PC_REQ_FIELDS
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
   //PC_TCP_REQ_FIELDS
    {* public *}
    route: PAnsiChar;
    msg: Pjson_t;
  end;

{**
 * Pomelo client instance
 *}
  pc_client_s = packed record
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
    address: PSockAddrIn;
    cb: pc_connect_cb;
    {* private *}
    socket: Pointer; //uv_tcp_t *socket;
  end;
  pc_connect_t = pc_connect_s;

{**
 * Pomelo request class is a subclass of pc_tcp_req_t.
 * Request is the async context for a Pomelo request to server.
 *}
  pc_request_s  = packed record
    //PC_REQ_FIELDS;
    {* private *}
    client: Ppc_client_t;
    transport: Pointer; // pc_transport_t *transport;
    type_: pc_req_type;
    data: Pointer;
    //PC_TCP_REQ_FIELDS
    {* public *}
    route: PAnsiChar;
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
    route: PAnsiChar;
    msg: Pjson_t;
    //
    cb: pc_notify_cb;
  end;
  pc_notify_t = pc_notify_s;

{**
 * Message structure.
 *}
  pc_msg_s = packed record
    id: uint32_t;
    route: PAnsiChar;
    msg: Pjson_t;
  end;
  pc_msg_t = pc_msg_s;

{**
 * Create and initiate Pomelo client intance.
 *
 * @return Pomelo client instance
 *}
function pc_client_new: Ppc_client_t; cdecl stdcall;

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
function pc_client_new_with_reconnect(delay, delay_max, exp_backoff: Integer): Ppc_client_t; cdecl stdcall;

{**
 * Disconnect Pomelo client and reset all status back to initialted.
 *
 * @param client Pomelo client instance.
 *}
procedure pc_client_disconnect(client: Ppc_client_t); cdecl stdcall;

{**
 * Stop the connection of the client. It is suitable for calling in the child
 * thread and the main thread called the pc_client_join funtion the wait the
 * worker child thread return.
 *
 * @param client client instance.
 *}
procedure pc_client_stop(client: Ppc_client_t); cdecl stdcall;

{**
 * Destroy and disconnect the connection of the client instance.
 *
 * @param client client instance.
 *}
procedure pc_client_destroy(client: Ppc_client_t); cdecl stdcall;

{**
 * Join and wait the worker child thread return. It is suitable for the
 * situation that the main thread has nothing to do after the connction
 * established.
 *
 * @param client client instance.
 * @return 0 for ok or error code for error.
 *}
function pc_client_join(client: Ppc_client_t): Integer cdecl stdcall;

{**
 * Create and initiate a request instance.
 *
 * @return req request instance
 *}
function pc_request_new: Ppc_request_t cdecl stdcall;

{**
 * Destroy and release inner resource of a request instance.
 *
 * @param req request instance to be destroied.
 *}
procedure pc_request_destroy(req: Ppc_request_t) cdecl stdcall;

{**
 * Connect the client to the server which would create a worker child thread
 * and connect to the server.
 *
 * @param client client instance.
 * @param addr server address.
 * @return 0 or -1.
 *}
function pc_client_connect(client: Ppc_client_t; var addr: sockaddr_in): Integer; cdecl stdcall;

{**
 * Connect the client to server just like pc_client_connect,
 * except that it's the asynchronous version for it.
 * The user should be responsible to conn_req's allocation, initialization and reclamation
 *
 * @param client client instance
 * @param conn_req connect request which are allocated and initialized by pc_connect_req_new
 * @return 0 or -1
 *}
function pc_client_connect2(client: Ppc_client_t; conn_req: Ppc_connect_t; cb: pc_connect_cb): Integer; cdecl stdcall;

{**
 *
 * Use for async connection
 *
 * @param addr address to which the connection is made
 * @return an instance of pc_connect_t, which should be released manually by user.
 *}
function pc_connect_req_new(var addr: sockaddr_in): Ppc_connect_t; cdecl stdcall;

{**
 * Destroy instance of pc_connect_t
 *
 * @param conn_req pc_connect_t instance
 * @return none
 *}
procedure pc_connect_req_destroy(conn_req: Ppc_connect_t); cdecl stdcall;

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
function pc_request(client: Ppc_client_t; req: Ppc_request_t; const route: PAnsiChar;
  msg: Pjson_t; cb: pc_request_cb): Integer; cdecl stdcall;

{**
 * Create and initiate notify instance.
 *
 * @return notify instance
 *}
function pc_notify_new: Ppc_notify_t; cdecl stdcall;

{**
 * Destroy and release inner resource of a notify instance.
 *
 * @param req notify instance to be destroied.
 *}
procedure pc_notify_destroy(req: Ppc_notify_t); cdecl stdcall;

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
function pc_notify(client: Ppc_client_t; req: Ppc_notify_t; const route: PAnsiChar;
  msg: Pjson_t; cb: pc_notify_cb): Integer; cdecl stdcall;

{**
 * Register a listener in the client.
 *
 * @param client client instance.
 * @param event event name.
 * @param event_cb event callback.
 * @return 0 or -1.
 *}
function pc_add_listener(client: Ppc_client_t; const event: PAnsiChar;
  event_cb: pc_event_cb): Integer; cdecl stdcall;

{**
 * Remove a listener in the client.
 *
 * @param client client instance.
 * @param event event name.
 * @param event_cb event callback.
 * @return void.
 *}
procedure pc_remove_listener(client: Ppc_client_t; const event: PAnsiChar;
  var event_cb: pc_event_cb); cdecl stdcall;

{**
 * Emit a event from the client.
 *
 * @param client client instance.
 * @param event event name.
 * @param data attach data of the event.
 *}
procedure pc_emit_event(client: Ppc_client_t; const event: PAnsiChar; data: Pointer); cdecl stdcall;

{**
 * jansson memory malloc, free self-defined function.
 *
 * @param malloc_fn malloc function.
 * @param free_fn free function.
 *}
procedure pc_json_set_alloc_funcs(malloc_fn: Pointer; free_fn: Pointer); cdecl stdcall;

{**
 * Init protobuf settings, set the read/write proto files directorys
 *
 * @param client client instance.
 * @param proto_read_dir directory of proto files to read.
 * @param proto_write_dir directory of proto files to write.
 *}
procedure pc_proto_init(client: Ppc_client_t; const proto_read_dir: PAnsiChar; const proto_write_dir: PAnsiChar); cdecl stdcall;

{**
 * Init protobuf settings, set the callback for read/write proto files
 *
 * @param client client instance.
 * @param proto_cb callback when read or write proto files.
 *}
procedure pc_proto_init2(client: Ppc_client_t; proto_cb: pc_proto_cb); cdecl stdcall;

procedure pc_proto_copy(client: Ppc_client_t; proto_ver: Pjson_t; client_protos: Pjson_t; server_protos: Pjson_t); cdecl stdcall;

implementation

const
  DllName = 'libpomelo.dll';

function pc_client_new; external DllName;
function pc_client_new_with_reconnect; external DllName;
procedure pc_client_disconnect; external DllName;
procedure pc_client_stop; external DllName;
procedure pc_client_destroy; external DllName;
function pc_client_join; external DllName;
function pc_request_new; external DllName;
procedure pc_request_destroy; external DllName;
function pc_client_connect; external DllName;
function pc_client_connect2; external DllName;
function pc_connect_req_new; external DllName;
procedure pc_connect_req_destroy; external DllName;
function pc_request; external DllName;
function pc_notify_new; external DllName;
procedure pc_notify_destroy; external DllName;
function pc_notify; external DllName;
function pc_add_listener; external DllName;
procedure pc_remove_listener; external DllName;
procedure pc_emit_event; external DllName;
procedure pc_json_set_alloc_funcs; external DllName;
procedure pc_proto_init; external DllName;
procedure pc_proto_init2; external DllName;
procedure pc_proto_copy; external DllName;

end.
