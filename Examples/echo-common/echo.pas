unit echo;

interface

uses
  System.SysUtils, Classes,
  Pomelo.Lib,
  Pomelo.Jansson,
  Pomelo.Client;

const
  MAX_LINE_CHARS = 1024;
  END_STR = 'bye';

type
  TEchoOutput = procedure(Sender: TObject; Output: String) of object;

  TEchoThread = class(TThread)
  private
    FWriteString: String;
    procedure WriteString;
  protected
    procedure Execute; override;
  public
    OnOutput: TEchoOutput;
    Client: TObject;
    procedure Writeln(Value: String);
  end;

  TEcho = class(TObject)
  private
    Working: Integer;
    FOnOutput: TEchoOutput;
    Client: TPomeloClient;
    FConnected: Boolean;
    EchoThread: TEchoThread;
    procedure DoRequest(Client: TPomeloClient; Value: String);
    procedure OnRequest(Client: TPomeloClient; Request: Ppc_request_t; Status: Integer; Response: Pjson_t);
    procedure OnConnect(Client: TPomeloClient; Request: Ppc_connect_t; Status: Integer);
    procedure OnEvent(Client: TPomeloClient; const Event: String; Data: Pointer);
    procedure OnHey(Client: TPomeloClient; const Event: String; Data: Pointer);
    procedure WriteLn(Value: String);
    procedure SetOnOutput(Value: TEchoOutput);
  protected
  public
    Ip: String;
    Port: Integer;
    constructor Create;
    destructor Destroy; override;
    function Connect: Boolean;
    procedure Disconnect;
    property OnOutput: TEchoOutput read FOnOutput write SetOnOutput;
    property Connected: Boolean read FConnected;
    procedure Send(Value: String);
  end;

implementation

{ TEcho }

constructor TEcho.Create;
begin
  EchoThread := TEchoThread.Create(True);
  EchoThread.FreeOnTerminate := True;
  EchoThread.Client := Self;
  EchoThread.Start;
  Client := TPomeloClient.Create;
  Client.OnRequest := OnRequest;
  Client.OnConnect := OnConnect;
  Client.OnEvent := OnEvent;
  IP := '127.0.0.1';
  Port := 3010;
end;

destructor TEcho.Destroy;
begin
  EchoThread.Terminate;
  Disconnect;
  if Assigned(Client) then
    FreeAndNil(Client);
  inherited;
end;

procedure TEcho.Disconnect;
begin
  Client.Disconnect;
  FConnected := False;
end;

procedure TEcho.DoRequest(Client: TPomeloClient; Value: String);
var
  Msg, Str: Pjson_t;
  Data: Pointer;
  Marshall: TMarshaller;
const
  Route = 'connector.helloHandler.echo';
begin
  // compose request
  Msg := json_object();
  Str := json_string(Marshall.AsAnsi(Value).ToPointer);
  json_object_set(Msg, 'body', str);
  // decref for json object
  json_decref(str);

  // Add some client data
  Data := Pointer($12345);
  Client.Request(Route, Msg, Data);
end;

function TEcho.Connect: Boolean;
begin
  if Client.Initialize <> PC_ST_INITED then
  begin
    writeln('failed to initialize client');
    Client.Free;
    FConnected := False;
    Result := False;
    Exit;
  end;

  // add some event callback, use OnHey as event callback
  Client.AddListener('onHey', OnHey);

  // add a predefined listener, use Client.OnEvent as callback
  Client.AddListener(PC_EVENT_DISCONNECT);

  // try to connect to server.
  Client.Port := Port;
  Client.Host := Ip;
  if not Client.Connect then
  begin
    writeln('fail to connect server.');
    Client.Free;
    Result := False;
    FConnected := False;
    Exit;
  end;
  Result := True;
  FConnected := True;
end;

procedure TEcho.OnConnect(Client: TPomeloClient; Request: Ppc_connect_t;
  Status: Integer);
begin
  if Status = -1 then
    writeln('Connected error.\n')
  else
  begin
    Working := 1;
    writeln('Connected');
  end;
end;

procedure TEcho.OnEvent(Client: TPomeloClient; const Event: String;
  Data: Pointer);
begin
  writeln('OnEvent: ' + Event);
  writeln(format('data = %p', [Data]));
end;

procedure TEcho.OnHey(Client: TPomeloClient; const Event: String;
  Data: Pointer);
var
  PushMsg: Pjson_t;
  JsonStr: MarshaledAString;
begin
  PushMsg := Data;
  JsonStr := json_dumps(PushMsg, 0);
  writeln(format('on event: %s, serve push msg: %s', [Event, JsonStr]));
  FreeMem(JsonStr);
  // stop the working thread.
  Client.Stop;
end;


procedure TEchoThread.Execute;
begin
  inherited;
  while not Terminated do
    Sleep(0);
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
    writeln('server echo: ' + Value);
  end;
end;


procedure TEchoThread.Writeln(Value: String);
begin
  if Assigned(OnOutput) then
  begin
    FWriteString := Value;
    Synchronize(WriteString);
  end;
end;

procedure TEchoThread.WriteString;
begin
  OnOutput(Client, FWriteString);
end;

procedure TEcho.Send(Value: String);
begin
  DoRequest(Client, Value);
end;

procedure TEcho.SetOnOutput(Value: TEchoOutput);
begin
  EchoThread.OnOutput := Value;
  FOnOutput := Value;
end;

procedure TEcho.WriteLn(Value: String);
begin
  if Assigned(OnOutput) then
    EchoThread.Writeln(Value);
end;

end.
