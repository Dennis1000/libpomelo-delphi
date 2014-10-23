program echomobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Pomelo.Client in '..\..\Source\Pomelo.Client.pas',
  Pomelo.Jansson in '..\..\Source\Pomelo.Jansson.pas',
  Pomelo.Lib in '..\..\Source\Pomelo.Lib.pas',
  echo in '..\echo-common\echo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
