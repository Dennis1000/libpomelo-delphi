program echo3;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Pomelo.Lib in '..\..\Source\Pomelo.Lib.pas',
  Pomelo.Jansson in '..\..\Source\Pomelo.Jansson.pas',
  Pomelo.Client in '..\..\Source\Pomelo.Client.pas',
  echo in '..\echo-common\echo.pas';

type
  TConsoleApp = class(TObject)
  private
    procedure OnOutput(Sender: TObject; Value: String);
  public
    procedure Run;
  end;

{ TConsoleApp }

procedure TConsoleApp.OnOutput(Sender: TObject; Value: String);
begin
  writeln(Value);
end;

procedure TConsoleApp.Run;
var
  Echo: TEcho;
  Input: String;
begin
  Echo := TEcho.Create;
  Echo.Ip := '192.168.167.119';
  Echo.OnOutput := OnOutput;
  if not Echo.Connect then
  begin
    Echo.Free;
    Exit;
  end;

  writeln('Input a line to send message to server and input `bye` to exit.');
  repeat
    readln(Input);

    if Input = END_STR then
      Break;

    if Length(Input) > MAX_LINE_CHARS then
      Input := Copy(Input, 1, MAX_LINE_CHARS);

    Echo.Send(Input);
  until False;
  Echo.Free;
end;

var
  ConsoleApp: TConsoleApp;

begin
  ConsoleApp := TConsoleApp.Create;
  try
    ConsoleApp.Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ConsoleApp.Free;
end.













