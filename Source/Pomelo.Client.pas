{**
 * Pomelo.Client.pas v0.2 - An object oriented Pomelo client interface for Delphi
 *
 * Copyright (c) 2014 Dennis D. Spreen (dennis@spreendigital.de)
 * MIT Licensed.
 *
 * http://blog.spreendigital.de/
 *}
unit Pomelo.Client;

interface

uses
  Generics.Defaults, Generics.Collections, Pomelo.Lib, Pomelo.Jansson, WinApi.Winsock, WinApi.Windows;

type
  TPomeloClient = class;
  TPomeloRequest = class;

  TPomeloClientOnEvent = procedure(Client: TPomeloClient; const Event: String; Data: Pointer) of object;
  TPomeloClientOnConnect = procedure(Client: TPomeloClient; Request: Ppc_connect_t; Status: Integer) of object;
  TPomeloClientOnRequest = procedure(Client: TPomeloClient; Request: Ppc_request_t; Status: Integer; Response: Pjson_t) of object;
  TPomeloClientOnNotify = procedure(Client: TPomeloClient; Notify: Ppc_notify_t; Status: Integer) of object;
  TPomeloClientOnHandshake = function(Client: TPomeloClient; Msg: Pjson_t): Integer of object;
  TPomeloClientOnMsgParse = function(Client: TPomeloClient; Data: PAnsiChar; Len: SIZE_T): Ppc_msg_t of object;
  TPomeloClientOnMsgParseDone = procedure(Client: TPomeloClient; Msg: Ppc_msg_t) of object;
  TPomeloClientOnMsgEncode = function(Client: TPomeloClient; ReqId: Uint32_t; Route: PAnsiChar; Msg: Pjson_t): Ppc_buf_t of object;
  TPomeloClientOnMsgEncodeDone = procedure(Client: TPomeloClient; Buf: pc_buf_t) of object;
  TPomeloClientOnProto = procedure(Client: TPomeloClient; Op: pc_proto_op; FileName: PAnsiChar; Data: Pointer) of object;

  TPomeloRequest = class
    Request: Ppc_request_t;
    PomeloClient: TPomeloClient;
    Data: Pointer;
    OnRequest: TPomeloClientOnRequest;
  end;

  TPomeloConnect = class
    Request: Ppc_connect_t;
    PomeloClient: TPomeloClient;
    Data: Pointer;
  end;

  TPomeloEventListener = class
    Event: String;
    PomeloClient: TPomeloClient;
    OnEvent: TPomeloClientOnEvent;
  end;

  TPomeloEventListenerObjList = class(TObjectList<TPomeloEventListener>)
  public
    destructor Destroy; override;
  end;

  TPomeloClient = class(TObject)
  private
    FHost: String;
    FPort: Integer;
    FData: Pointer;
    FOnEvent: TPomeloClientOnEvent;
    FOnRequest: TPomeloClientOnRequest;
    FOnConnect: TPomeloClientOnConnect;
    FOnNotify: TPomeloClientOnNotify;
    FHostAddress: SOCKADDR_IN;
    Client: Ppc_client_t;  //libpomelo client
    EventListener: TPomeloEventListenerObjList;
    procedure DeInitialize; // destroy libpomelo client
    procedure SetHost(Value: String);
    procedure SetPort(Value: Integer);
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Initialize: pc_client_state;
    function Connect: Boolean; overload;
    function Connect(Address: String; Port: Integer): Boolean; overload;
    procedure Stop;

    function Request(Route: String; Msg: Pjson_t): TPomeloRequest; overload;
    function Request(Route: String; Msg: Pjson_t; Data: Pointer): TPomeloRequest; overload;
    function Request(Route: String; Msg: Pjson_t; OnRequest: TPomeloClientOnRequest): TPomeloRequest; overload;
    function Request(Route: String; Msg: Pjson_t; Data: Pointer; OnRequest: TPomeloClientOnRequest): TPomeloRequest; overload;

    function AddListener(Event: String): TPomeloEventListener; overload;
    function AddListener(Event: String; OnEvent: TPomeloClientOnEvent): TPomeloEventListener; overload;
    procedure RemoveListener(Event: String); overload;
    procedure RemoveListener(EventListener: TPomeloEventListener); overload;

    procedure EmitEvent(Event: String; Data: Pointer);
    function Notify(Request: Ppc_notify_t; Route: String; Msg: PJson_t; OnNotify: TPomeloClientOnNotify): Integer; overload;

    property Host: String read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Data: Pointer read FData write FData;
    property OnRequest: TPomeloClientOnRequest read FOnRequest write FOnRequest;
    property OnConnect: TPomeloClientOnConnect read FOnConnect write FOnConnect;
    property OnEvent: TPomeloClientOnEvent read FOnEvent write FOnEvent;
    property OnNotify: TPomeloClientOnNotify read FOnNotify write FOnNotify;
  end;


implementation

{ TPomeloClient }

uses
  System.Classes;

type
  TPomeloEventListenerList = class
  private
    FList: TStringList;
    FLock: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Event: String; OnEvent: TPomeloEventListener);
    procedure Clear;
    function LockList: TStringList;
    procedure Remove(Item: String);
    procedure UnlockList;
  end;

var
  PomeloClientList: TThreadList<TPomeloClient>;
  PomeloEventListenerList: TPomeloEventListenerList;

// request callback
procedure on_request_cb(req: Ppc_request_t; status: Integer; resp: Pjson_t);cdecl;
var
  Msg: Pjson_t;
  PomeloClient: TPomeloClient;
  PomeloRequest: TPomeloRequest;
begin
  if (Assigned(req.data)) and (TObject(req.data).ClassType = TPomeloRequest) then
  begin
    // Get the request and the client
    PomeloRequest := TPomeloRequest(req.data);
    PomeloClient := PomeloRequest.PomeloClient;
    if Assigned(PomeloRequest.OnRequest) then
    begin
      // Change Request data to original attached data
      req.data := PomeloRequest.Data;
      PomeloRequest.OnRequest(PomeloClient, req, status, resp);
    end;
    PomeloRequest.Free;
  end;

  // release relative resource with pc_request_t
  Msg := req.msg;
  json_decref(Msg);
  pc_request_destroy(req);
end;


procedure on_connected(req: Ppc_connect_t; status: Integer); cdecl;
var
  PomeloClient: TPomeloClient;
  PomeloConnect: TPomeloConnect;
begin
  if (Assigned(req.data)) and (TObject(req.data).ClassType = TPomeloConnect) then
  begin
    // Get the request and the client
    PomeloConnect := TPomeloConnect(req.data);
    PomeloClient := PomeloConnect.PomeloClient;
    if Assigned(PomeloClient) and Assigned(PomeloClient.OnConnect) then
    begin
      // Change Request data to original attached data
      req.data := PomeloConnect.Data;
      PomeloClient.OnConnect(PomeloClient, req, status);
    end;
    PomeloConnect.Free;
  end;
  //release connection request
  pc_connect_req_destroy(req);
end;

procedure on_event_cb(client: Ppc_client_t; const char: PAnsiChar; data: Pointer); cdecl;
var
  List: TStringList;
  Event: String;
  Index: Integer;
  PomeloEvent: TPomeloEventListener;
  OnEvent: TPomeloClientOnEvent;
  PomeloClient: TPomeloClient;
begin
  // Find event notifier
  List := PomeloEventListenerList.LockList;
  Event := String(char);
  Index := List.IndexOf(Event);
  if Index <> -1 then
  begin
    PomeloEvent := List.Objects[Index] as TPomeloEventListener;
    PomeloClient := PomeloEvent.PomeloClient;
    if Assigned(PomeloEvent.OnEvent) then
      OnEvent := PomeloEvent.OnEvent
    else
    if Assigned(PomeloEvent.PomeloClient.OnEvent) then
      OnEvent := PomeloEvent.PomeloClient.OnEvent
    else
      OnEvent := NIL;
  end
  else
  begin
    OnEvent := NIL;
    PomeloClient := NIL;
  end;
  PomeloEventListenerList.UnlockList;

  if Assigned(OnEvent) then
    OnEvent(PomeloClient, Event, data);
end;




function TPomeloClient.Connect: Boolean;
var
  RequestConnect: TPomeloConnect;
begin
  // Verify the pomelo client instance
  if not Assigned(Client) then
    if Initialize <> PC_ST_INITED then
    begin
      DeInitialize;
      Result := False;
      Exit;
    end;

  if Assigned(OnConnect) then
  begin
    // Create a connection request
    RequestConnect := TPomeloConnect.Create;
    RequestConnect.Request := pc_connect_req_new(FHostAddress);
    RequestConnect.Request.data := RequestConnect;

    // connect with the connection request
    if pc_client_connect2(client, RequestConnect.Request, on_connected) <> 0 then
    begin
      pc_connect_req_destroy(RequestConnect.Request);
      RequestConnect.Free;
      DeInitialize;
      Result := False;
      Exit;
    end;
  end
  else
    // try to connect to server.
    if pc_client_connect(Client, FHostAddress) <> 0 then
    begin
      DeInitialize;
      Result := False;
      Exit;
    end;

  Result := True;
end;

function TPomeloClient.AddListener(Event: String): TPomeloEventListener;
begin
  Result := AddListener(Event, TPomeloClientOnEvent(NIL));
end;

function TPomeloClient.AddListener(Event: String;
  OnEvent: TPomeloClientOnEvent): TPomeloEventListener;
var
  PomeloEvent: TPomeloEventListener;
begin
  PomeloEvent := TPomeloEventListener.Create;
  PomeloEvent.OnEvent := OnEvent;
  PomeloEvent.PomeloClient := Self;
  PomeloEvent.Event := Event;
  PomeloEventListenerList.Add(Event, PomeloEvent);
  pc_add_listener(client, PAnsiChar(AnsiString(Event)), on_event_cb);
  Result := PomeloEvent;
end;

function TPomeloClient.Connect(Address: String; Port: Integer): Boolean;
begin
  Host := Address;
  Self.Port := Port;
  Result := Connect;
end;

constructor TPomeloClient.Create;
begin
  FHostAddress.sin_family := AF_INET;
  PomeloClientList.Add(Self);
  EventListener := TPomeloEventListenerObjList.Create;
end;

procedure TPomeloClient.DeInitialize;
begin
  if Assigned(Client) then
  begin
     pc_client_stop(Client);
     pc_client_destroy(Client);
     Client := NIL;
  end;
end;

destructor TPomeloClient.Destroy;
begin
  EventListener.Free;
  PomeloClientList.Remove(Self);
  DeInitialize;
  inherited;
end;


procedure TPomeloClient.EmitEvent(Event: String; Data: Pointer);
begin
  pc_emit_event(Client, PAnsiChar(AnsiString(Event)), Data);
end;

function TPomeloClient.Initialize: pc_client_state;
begin
  if Assigned(Client) then
    DeInitialize;

  Client := pc_client_new;
  Result := Client.state;
end;

function TPomeloClient.Notify(Request: Ppc_notify_t; Route: String;
  Msg: PJson_t; OnNotify: TPomeloClientOnNotify): Integer;
begin

end;

function TPomeloClient.Request(Route: String; Msg: Pjson_t): TPomeloRequest;
begin
  Result := Request(Route, Msg, FData, FOnRequest);
end;

function TPomeloClient.Request(Route: String; Msg: Pjson_t;
  OnRequest: TPomeloClientOnRequest): TPomeloRequest;
begin
  Result := Request(Route, Msg, FData, OnRequest);
end;

function TPomeloClient.Request(Route: String; Msg: Pjson_t; Data: Pointer): TPomeloRequest;
begin
  Result := Request(Route, Msg, Data, FOnRequest);
end;

procedure TPomeloClient.RemoveListener(Event: String);
var
  List: TStringList;
  EventListener: TPomeloEventListener;
  Index: Integer;
begin
  List := PomeloEventListenerList.LockList;
  try
    Index := List.IndexOf(Event);
    if Index <> -1 then
    begin
      EventListener := List.Objects[Index] as TPomeloEventListener;
      List.Delete(Index);
      pc_remove_listener(Client, PAnsiChar(AnsiString(EventListener.Event)), on_event_cb);
      EventListener.Free;
    end;
  finally
    PomeloEventListenerList.UnlockList;
  end;
end;

procedure TPomeloClient.RemoveListener(EventListener: TPomeloEventListener);
var
  List: TStringList;
  I: Integer;
begin
  List := PomeloEventListenerList.LockList;
  try
    for I := List.Count - 1 Downto 0 do
      if List.Objects[I] = EventListener then
      begin
        List.Delete(I);
        pc_remove_listener(Client, PAnsiChar(AnsiString(EventListener.Event)), on_event_cb);
        EventListener.Free;
        Break;
      end;
  finally
    PomeloEventListenerList.UnlockList;
  end;
end;

function TPomeloClient.Request(Route: String; Msg: Pjson_t; Data: Pointer;
  OnRequest: TPomeloClientOnRequest): TPomeloRequest;
var
  PomeloRequest: TPomeloRequest;
  Request: Ppc_request_t;
begin
  PomeloRequest := TPomeloRequest.Create;
  PomeloRequest.PomeloClient := Self;
  PomeloRequest.Data := Data;
  PomeloRequest.OnRequest := OnRequest;
  Request := pc_request_new;
  Request.data := PomeloRequest;
  pc_request(Client, Request, PAnsiChar(AnsiString(Route)), Msg, on_request_cb);
  Result := PomeloRequest;
end;

procedure TPomeloClient.SetHost(Value: String);
begin
  FHost := Value;
  FHostAddress.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(Value)));
end;

procedure TPomeloClient.SetPort(Value: Integer);
begin
  FPort := Value;
  FHostAddress.sin_port := htons(Value);
end;



procedure TPomeloClient.Stop;
begin
  pc_client_stop(Client);
end;

{ TPomeloNotifierList }

procedure TPomeloEventListenerList.Add(Event: String; OnEvent: TPomeloEventListener);
begin
  LockList;
  try
    FList.AddObject(Event, OnEvent)
  finally
    UnlockList;
  end;
end;

procedure TPomeloEventListenerList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

constructor TPomeloEventListenerList.Create;
begin
  inherited Create;
  FLock := TObject.Create;
  FList := TStringList.Create;
end;

destructor TPomeloEventListenerList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;


function TPomeloEventListenerList.LockList: TStringList;
begin
  TMonitor.Enter(FLock);
  Result := FList;
end;

procedure TPomeloEventListenerList.Remove(Item: String);
begin
  LockList;
  try
    FList.Delete(FList.IndexOf(Item));
  finally
    UnlockList;
  end;
end;

procedure TPomeloEventListenerList.UnlockList;
begin
  TMonitor.Exit(FLock);
end;

{ TPomeloEventObjList }

destructor TPomeloEventListenerObjList.Destroy;
var
  Event: TPomeloEventListener;
begin
  for Event in Self do
  begin
    PomeloEventListenerList.Remove(Event.Event);
    Event.PomeloClient.RemoveListener(Event);
  end;
  inherited;
end;

initialization
  PomeloClientList := TThreadList<TPomeloClient>.Create;
  PomeloEventListenerList := TPomeloEventListenerList.Create;

finalization
  PomeloEventListenerList.Free;
  PomeloClientList.Free;
end.

