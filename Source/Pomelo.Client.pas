unit Pomelo.Client;

interface

uses
  Pomelo.Lib, Pomelo.Jansson, WinApi.Winsock;

type
  TPomeloClientRequestCallback = procedure(Sender: TObject; Request: Ppc_request_t; Status: Integer; Response: Pjson_t) of object;

  TPomeloClient = class(TObject)
  private
    FHost: String;
    FPort: Integer;
    FOnRequestCallback: TPomeloClientRequestCallback;
    FHostAddress: SOCKADDR_IN;
    Client: Ppc_client_t;  //libpomelo client
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
    function Request(Route: String; Msg: Pjson_t): Ppc_request_t;
    property Host: String read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property OnRequestCallback: TPomeloClientRequestCallback read FOnRequestCallback write FOnRequestCallback;
  end;

implementation

{ TPomeloClient }

// request callback
procedure on_request_cb(req: Ppc_request_t; status: Integer; resp: Pjson_t);cdecl stdcall;
var
  Msg: Pjson_t;
  PomeloClient: TPomeloClient;
begin
  if (Assigned(req.data)) and (TObject(req.data).ClassType = TPomeloClient) then
  begin
    PomeloClient := TPomeloClient(req.data);
    if Assigned(PomeloClient.OnRequestCallback) then
      PomeloClient.OnRequestCallback(PomeloClient, req, status, resp);
  end;

  // release relative resource with pc_request_t
  Msg := req.msg;
  json_decref(Msg);
  pc_request_destroy(req);
end;


function TPomeloClient.Connect: Boolean;
begin
  Result := Connect(Host, Port);
end;

function TPomeloClient.Connect(Address: String; Port: Integer): Boolean;
begin
  Host := Address;
  Self.Port := Port;

  if not Assigned(Client) then
    if Initialize <> PC_ST_INITED then
    begin
      Result := False;
      Exit;
    end;

  // try to connect to server.
  if pc_client_connect(Client, FHostAddress) <> 0 then
  begin
    DeInitialize;
    Result := False;
    Exit;
  end;

  Result := True;
end;

constructor TPomeloClient.Create;
begin
  FHostAddress.sin_family := AF_INET;
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
  DeInitialize;
  inherited;
end;


function TPomeloClient.Initialize: pc_client_state;
begin
  if Assigned(Client) then
    DeInitialize;

  Client := pc_client_new;
  Result := Client.state;
end;

function TPomeloClient.Request(Route: String; Msg: Pjson_t): Ppc_request_t;
begin
  Result := pc_request_new;
  Result.data := Self;
  pc_request(Client, Result, PAnsiChar(AnsiString(Route)), Msg, on_request_cb);
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

end.

