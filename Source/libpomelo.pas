{**
 * Copyright (c) 2014 NetEase, Inc. and other Pomelo contributors
 * MIT Licensed.
 *
 * Delphi Conversion 2014/08 Dennis D. Spreen (dennis@spreendigital.de)
 * http://blog.spreendigital.de/
 *}

unit libpomelo;

interface

uses
  Windows, WinSock, jansson;

const
  PC_TYPE: AnsiString = 'c';
  PC_VERSION: AnsiString = '0.1.2';

  PC_EVENT_DISCONNECT: AnsiString = 'disconnect';
  PC_EVENT_TIMEOUT: AnsiString  = 'timeout';
  PC_EVENT_KICK: AnsiString = 'onKick';
  PC_EVENT_RECONNECT: AnsiString = 'reconnect';

  PC_PROTO_VERSION: AnsiString = 'protoVersion';
  PC_PROTO_CLIENT: AnsiString = 'clientProtos';
  PC_PROTO_SERVER: AnsiString = 'serverProtos';

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
 * operation for proto files.
 *}
  pc_proto_op = (
    PC_PROTO_OP_READ = 1,
    PC_PROTO_OP_WRITE,
    PC_PROTO_OP_UNKONWN);


{**
 * Message structure.
 *}
  pc_msg_s = packed record
    id: UINT32_T;
    route: PAnsiChar;
    msg: PJSON_T;
  end;
  pc_msg_t = pc_msg_s;
  Ppc_msg_t = ^pc_msg_t;

  pc_client_s = packed record
  {* public *}
    state: pc_client_state;
  {* private *}
    PrivateVars: Array[1..344 - SizeOf(pc_client_state)] of Byte;
  end;

  pc_client_t = pc_client_s;
  Ppc_client_t = ^pc_client_t;
  pc_buf_t = uv_buf_t;
  Ppc_buf_t = ^pc_buf_t;


  PC_REQ_FIELDS = array[1..16] of Byte;

  pc_req_s = packed record
    Fields: PC_REQ_FIELDS;
  end;

  PC_TCP_REQ_FIELDS = packed record
  {* public *}
    route: PAnsiChar;
    msg: Pjson_t;
  end;

  pc_request_s  = packed record
    PrivateFields: PC_REQ_FIELDS;
    //PC_TCP_REQ_FIELDS
    {* public *}
    route: PAnsiChar;
    msg: Pjson_t;
    Dummy: array [1..40 - 16 - 8] of Byte;
  end;
  pc_request_t = pc_request_s;
  Ppc_request_t = ^pc_request_t;

{**
 * Connect request class is a subclass of pc_req_t.
 * Connect is the async context for a connection request to server.
 *}
  pc_connect_s = record
    Dummy: array[1..28] of Byte;
  end;
  pc_connect_t = pc_connect_s;
  Ppc_connect_t = ^pc_connect_t;

  pc_connect_cb = record
    Dummy: array[1..4] of Byte;
  end;


{**
 * Callbacks
 *}

{**
 * Handshake callback for client which would be fired during handshake phase and
 * passing the customized handshake information from server.
 *
 * @param client client instance.
 * @param msg customized handshake information from server.
 * @return 0 for ok and -1 for error and terminate the connection.
 *}

  PC_HANDSHAKE_CB = function (client: Ppc_client_t; var MSG: JSON_T): Integer; cdecl stdcall;

{**
 * Message encode done callback which would be fired when the encode data has
 * been delivered or meeting some error to release the resources created during
 * the encode phase.
 *
 * @param client client instance.
 * @param buf encode result.
 *}

  pc_msg_encode_done_cb =  procedure(client: Ppc_client_t; buf: pc_buf_t); cdecl stdcall;

  pc_proto_cb = procedure(CLIENT: Ppc_client_t; op: pc_proto_op; fileName: PAnsiChar; Data: Pointer); cdecl stdcall;

{**
 * Message parse callback which would be fired when a new message arrived.
 *
 * @param client client instance.
 * @param data original message data in bytes.
 * @param len length of the data.
 * @return the parse result or NULL for error.
 *}
  PC_MSG_PARSE_CB = function (CLIENT: Ppc_client_t; Data: PAnsiChar; LEN: SIZE_T): PPC_MSG_T; cdecl stdcall;

{**
 * Message parse done callback which would be fired when the the message has
 * processed to release the resources created in the message parsing phase.
 *
 * @param client client instance.
 * @param msg message instance.
 *}
  PC_MSG_PARSE_DONE_CB = procedure (CLIENT: Ppc_client_t; MSG: PPC_MSG_T); cdecl stdcall;

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
  PC_MSG_ENCODE_CB = function (CLIENT: Ppc_client_t; REQID: UINT32_T; ROUTE: PAnsiChar; MSG: PJSON_T): PPC_BUF_T; cdecl stdcall;


{**
 * Request callback.
 *
 * @param  req    request instance.
 * @param  status request status. 0 for ok and -1 for error.
 * @param  resp   response message from server, NULL for error.
 *}
 pc_request_cb = procedure (req: Ppc_request_t; status: Integer; resp: Pjson_t);cdecl stdcall;

  {**
 * Create and initiate Pomelo client intance.
 *
 * @return Pomelo client instance
 *}

function pc_client_new: Ppc_client_t cdecl stdcall;


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

function pc_client_new_with_reconnect(delay, delay_max, exp_backoff: Integer): Ppc_client_t cdecl stdcall;



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

function pc_client_connect(client: Ppc_client_t; var addr: SOCKADDR_IN): Integer; cdecl; stdcall;


{**
 * Connect the client to server just like pc_client_connect,
 * except that it's the asynchronous version for it.
 * The user should be responsible to conn_req's allocation, initialization and reclamation
 *
 * @param client client instance
 * @param conn_req connect request which are allocated and initialized by pc_connect_req_new
 * @return 0 or -1
 *}
function pc_client_connect2(client: Ppc_client_t; conn_req: Ppc_connect_t; cb: pc_connect_cb): Integer; cdecl; stdcall;

{**
 *
 * Use for async connection
 *
 * @param addr address to which the connection is made
 * @return an instance of pc_connect_t, which should be released manually by user.
 *}
function pc_connect_req_new(var addr: SOCKADDR_IN): Ppc_connect_t; cdecl; stdcall;

{**
 * Destroy instance of pc_connect_t
 *
 * @param conn_req pc_connect_t instance
 * @return none
 *}
procedure pc_connect_req_destroy(conn_req: Ppc_connect_t); cdecl; stdcall;

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
  msg: Pjson_t; cb: pc_request_cb): Integer; cdecl; stdcall;

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

end.

