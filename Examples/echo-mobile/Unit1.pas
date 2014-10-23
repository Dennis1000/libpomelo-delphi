unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.Memo, echo, FMX.StdCtrls,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Edit3: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure OnOutput(Sender: TObject; Value: String);
  public
    { Public declarations }
    Count: Integer;
    Echo: TEcho;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Pomelo.Lib;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Echo.Connected then
  begin
    Echo.Disconnect;
    Button1.Text := 'Connect';
    Edit1.Enabled := False;
    OnOutput(Self, 'Disconnected.');
  end
  else
  begin
    Echo.Ip := Edit2.Text;
    Echo.Port := StrToInt(Edit3.Text);
    OnOutput(Self, 'Trying to connect to ' + Echo.Ip + ':' + IntToStr(Echo.Port));
    Echo.Connect;
  end;
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key <> 13 then
    Exit;
  Echo.Send(Edit1.Text);
  Edit1.Text := '';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Echo := TEcho.Create;
  Echo.OnOutput := OnOutput;
  if Pomelo.Lib.ErrorLog.Count <> 0 then
    Memo1.Lines.Assign(Pomelo.Lib.ErrorLog);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Echo.Free;
end;

procedure TForm1.OnOutput(Sender: TObject; Value: String);
begin
  Memo1.Lines.Add(Value);
  Memo1.GoToTextEnd;
  if Value='Connected' then
  begin
    Button1.Text := 'Disconnect';
    Edit1.Enabled := True;
    Edit1.SetFocus;
  end;
end;

end.
