program echo3;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Pomelo.Lib in '..\..\Source\Pomelo.Lib.pas',
  Pomelo.Jansson in '..\..\Source\Pomelo.Jansson.pas',
  Pomelo.Client in '..\..\Source\Pomelo.Client.pas';

const
  Ip = '127.0.0.1';
  Port = 3010;

type
  TEcho = class(TObject)
  private
    procedure DoRequest(Client: TPomeloClient; Value: String);
    procedure OnRequestCallback(Sender: TObject; Request: Ppc_request_t; Status: Integer; Response: Pjson_t);
  protected
  public
    procedure Run;
  end;

{ TEcho }

procedure TEcho.DoRequest(Client: TPomeloClient; Value: String);
var
  Msg, Str: Pjson_t;
const
  Route = 'connector.helloHandler.echo';
begin
  // compose request
  Msg := json_object();
  Str := json_string(PAnsiChar(AnsiString(Value)));
  json_object_set(Msg, 'body', str);
  // decref for json object
  json_decref(str);

  Client.Request(Route, Msg);
end;

procedure TEcho.OnRequestCallback(Sender: TObject; Request: Ppc_request_t;
  Status: Integer; Response: Pjson_t);
var
  Value: String;
begin
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
  Client.OnRequestCallback := OnRequestCallback;
  if Client.Initialize <> PC_ST_INITED then
  begin
    writeln('failed to initialize client');
    Client.Free;
    Exit;
  end;

  // try to connect to server.
  if not Client.Connect(Ip, Port) then
  begin
    writeln('fail to connect server.');
    Client.Free;
    Exit;
  end;

  writeln('Input a line to send message to server and input `bye` to exit.');
  repeat
    readln(Input);
    if Input = 'bye' then
      Break;
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
