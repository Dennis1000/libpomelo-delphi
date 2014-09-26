program echo2;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, WinApi.WinSock,
  Pomelo.Lib in '..\..\Source\Pomelo.Lib.pas',
  Pomelo.Jansson in '..\..\Source\Pomelo.Jansson.pas';

const
  Ip = '127.0.0.1';
  Port = 3010;

var
  Client: Ppc_client_t;
  Address: SOCKADDR_IN;
  Input: String;
  Conn_req: Ppc_connect_t;
  working: Integer;

procedure on_connected(conn_req: Ppc_connect_t; status: Integer); cdecl stdcall;
begin
  writeln(format('data = %p', [conn_req.data]));
  pc_connect_req_destroy(conn_req);

  if status = -1 then
    writeln('Connected error.')
  else
    working := 1;
end;

// request callback
procedure on_request_cb(req: Ppc_request_t; status: Integer; resp: Pjson_t); cdecl stdcall;
var
  Msg: Pjson_t;
  Value: String;
begin
  if Status = -1 then
    writeln('Fail to send request to server.')
  else
  if Status = 0 then
  begin
    Value := String(json_string_value(json_object_get(resp, 'body')));
    writeln('server echo: ', Value);
  end;

  // release relative resource with pc_request_t
  Msg := req.msg;
  json_decref(Msg);
  pc_request_destroy(req);
end;


procedure do_request(client: Ppc_client_t; const Input: AnsiString);
var
  Msg, Str: Pjson_t;
  Request: Ppc_request_t;
const
  Route = 'connector.helloHandler.echo';
begin
  // compose request
  Msg := json_object();
  Str := json_string(PAnsiChar(Input));
  json_object_set(Msg, 'body', str);
  // decref for json object
  json_decref(str);

  Request := pc_request_new;
  pc_request(client, Request, PAnsiChar(Route), Msg, on_request_cb);
end;


// disconnect event callback.
procedure on_close(client: Ppc_client_t; const event: PAnsiChar; data: Pointer); cdecl stdcall;
begin
  writeln(format('client closed: %d.', [Integer(client.state)]));
end;

begin
  try
    working := 0;
    Client := pc_client_new;
    if Client.state <> PC_ST_INITED then
    begin
      writeln('failed to initialize client');
      Exit;
    end;

    address.sin_family := AF_INET;
    address.sin_port := htons(Port);
    address.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(Ip)));

    Conn_req := pc_connect_req_new(address);
    //~ bring some private date on
    Conn_req.data := Pointer($2013);

    // add some event callback.
    pc_add_listener(client, PAnsiChar(PC_EVENT_DISCONNECT), on_close);

   // try to connect to server.
    if pc_client_connect2(Client, Conn_req, on_connected) <> 0 then
    begin
      writeln('fail to connect server.');
      pc_connect_req_destroy(conn_req);
      pc_client_destroy(client);
      Exit;
    end;

    writeln('Input a line to send message to server and input `bye` to exit.');
    repeat
      readln(Input);
      if Input = 'bye' then
        Break;
      do_request(client, AnsiString(input));
    until False;

    pc_client_stop(Client);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
