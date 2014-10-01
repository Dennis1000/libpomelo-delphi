program echo3;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Pomelo.Lib in '..\..\Source\Pomelo.Lib.pas',
  Pomelo.Jansson in '..\..\Source\Pomelo.Jansson.pas',
  Pomelo.Client in '..\..\Source\Pomelo.Client.pas';

const
  IP = '127.0.0.1';
  PORT = 3010;
  MAX_LINE_CHARS = 1024;
  END_STR = 'bye';

type
  TEcho = class(TObject)
  private
    Working: Integer;
    procedure DoRequest(Client: TPomeloClient; Value: String);
    procedure OnRequest(Client: TPomeloClient; Request: Ppc_request_t; Status: Integer; Response: Pjson_t);
    procedure OnConnect(Client: TPomeloClient; Request: Ppc_connect_t; Status: Integer);
    procedure OnEvent(Client: TPomeloClient; const Event: String; Data: Pointer);
    procedure OnHey(Client: TPomeloClient; const Event: String; Data: Pointer);
  protected
  public
    procedure Run;
  end;

{ TEcho }

procedure TEcho.DoRequest(Client: TPomeloClient; Value: String);
var
  Msg, Str: Pjson_t;
  Data: Pointer;
const
  Route = 'connector.helloHandler.echo';
begin
  // compose request
  Msg := json_object();
  Str := json_string(PAnsiChar(AnsiString(Value)));
  json_object_set(Msg, 'body', str);
  // decref for json object
  json_decref(str);

  // Add some client data
  Data := Pointer($12345);
  Client.Request(Route, Msg, Data);
end;

procedure TEcho.OnConnect(Client: TPomeloClient; Request: Ppc_connect_t;
  Status: Integer);
begin
  if Status = -1 then
    writeln('Connected error.\n')
  else
    Working := 1;
end;

procedure TEcho.OnEvent(Client: TPomeloClient; const Event: String;
  Data: Pointer);
begin
  writeln('OnEvent: ', Event);
  writeln(format('data = %p', [Data]));
end;

procedure TEcho.OnHey(Client: TPomeloClient; const Event: String;
  Data: Pointer);
var
  PushMsg: Pjson_t;
  JsonStr: PAnsiChar;
begin
  PushMsg := Data;
  JsonStr := json_dumps(PushMsg, 0);
  writeln(format('on event: %s, serve push msg: %s', [Event, JsonStr]));
  FreeMem(JsonStr);
  // stop the working thread.
  Client.Stop;
end;


procedure TEcho.OnRequest(Client: TPomeloClient; Request: Ppc_request_t;
  Status: Integer; Response: Pjson_t);
var
  Value: String;
begin
  writeln(format('data = %p', [Request.data]));
  if Status = -1 then
    writeln('Fail to send request to server.')
  else
  if Status = 0 then
  begin
    Value := String(json_string_value(json_object_get(Response, 'body')));
    writeln('server echo: ', Value);
  end;
end;


procedure TEcho.Run;
var
  Input: String;
  Client: TPomeloClient;
begin
  Client := TPomeloClient.Create;
  Client.OnRequest := OnRequest;
  Client.OnConnect := OnConnect;
  Client.OnEvent := OnEvent;

  if Client.Initialize <> PC_ST_INITED then
  begin
    writeln('failed to initialize client');
    Client.Free;
    Exit;
  end;

  // add some event callback, use OnHey as event callback
  Client.AddListener('onHey', OnHey);

  // add a predefined listener, use Client.OnEvent as callback
  Client.AddListener(PC_EVENT_DISCONNECT);

  // try to connect to server.
  Client.Host := IP;
  Client.Port := PORT;
  if not Client.Connect then
  begin
    writeln('fail to connect server.');
    Client.Free;
    Exit;
  end;

  writeln('Input a line to send message to server and input `bye` to exit.');
  repeat
    readln(Input);

    if Input = END_STR then
      Break;

    if Length(Input) > MAX_LINE_CHARS then
      Input := Copy(Input, 1, MAX_LINE_CHARS);

    DoRequest(Client, input);
  until False;

  Client.Free;
end;


var
  Echo: TEcho;

begin
  Echo := TEcho.Create;
  try
    Echo.Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Echo.Free;
end.
