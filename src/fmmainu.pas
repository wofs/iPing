unit FmMainU;
{
 wofs(c)2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1
 Git: https://github.com/wofs/iPing
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, Buttons,
  Grids, ComCtrls, wcthread, LazUTF8, Types, lclintf, UtilsU,
  wPingU;

type

  THost = record
    Host:string;
    Description:string;
    Result: boolean;
    Time: integer;
  end;

  THosts = array of THost;  // array of hosts

  { TFmMain }

  TFmMain = class(TForm)
    BtnClear: TBitBtn;
    btnAbort: TBitBtn;
    BtnRetry: TBitBtn;
    btnClose: TBitBtn;
    BtnSave: TBitBtn;
    Images: TImageList;
    Panel1: TPanel;
    pInputFile: TPanel;
    GridLog: TStringGrid;
    ProgressBar: TProgressBar;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    TaskPing: TTask;
    TaskLoadFromFile: TTask;
    Threads: TWCThread;

    procedure btnAbortClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnRetryClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure GridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure TaskLoadFromFileExecute(const Sender: TTask; const Msg: Word;
      var Param: Variant);
    procedure TaskLoadFromFileFinish(const Sender: TTask; const Msg: Word;
      const Param: Variant);
    procedure TaskLoadFromFileProgress(const Sender: TTask; const Msg: Word;
      const Value: Word);
    procedure TaskPingExecute(const Sender: TTask; const Msg: Word;
      var Param: Variant);
    procedure TaskPingFinish(const Sender: TTask; const Msg: Word;
      const Param: Variant);
    procedure TaskPingMessage(const Sender: TTask; const Msg: Word;
      const Param: Variant);
  private
    wPing: TwPing;
    procedure BtnChangeStatus;
    procedure CheckFileName(aFileName: string);
    procedure CommandLineGetFileName;
    procedure GridLogFill;
    procedure GridLogSetStatus(aIndex: integer; aStatus: Integer; const aMS: integer
      =-1);
    procedure DrawImage(Sender: TObject; aRect: TRect; aIndex: integer);
    procedure RunPing;
  public

  end;

var
  FmMain: TFmMain;

implementation

{$R *.lfm}

{ TFmMain }


procedure TFmMain.GridLogFill;
var
  i: Integer;
begin
  GridLog.BeginUpdate;

  GridLog.RowCount:=0;
  GridLog.RowCount:=wPing.HostsCount+1;

  for i:=0 to wPing.HostsCount-1 do
    begin
      GridLog.Cells[1,i+1]:= wPing.Hosts[i].Host;
      GridLog.Cells[2,i+1]:= wPing.Hosts[i].Description;
    end;

  GridLog.EndUpdate();
end;


procedure TFmMain.RunPing;
begin
  if FileExists(wPing.FileName) then
     Threads.Task[0].Start(wPing.FileName);
end;

procedure TFmMain.CheckFileName(aFileName:string);
var
  aFileExt: string;
begin
  wPing.FileName:= aFileName;
  aFileExt:= LowerCase(ExtractFileExt(wPing.FileName));

  case aFileExt of
    '.txt':
      begin
        GridLog.Visible:= true;
        RunPing;
      end
    else
      ShowMessage('The file must have an extension .txt!');
  end;
end;

procedure TFmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  if Threads.Task[0].IsRunning or Threads.Task[1].IsRunning then exit;

  CheckFileName(FileNames[0]);
end;

procedure TFmMain.BtnClearClick(Sender: TObject);
begin
  if MessageDlg('To clear the list of hosts?', mtWarning, mbOKCancel, 0) = mrCancel then exit;

  wPing.HostsClear;
  GridLog.RowCount:=1;
  ProgressBar.Position:=0;
end;

procedure TFmMain.btnAbortClick(Sender: TObject);
begin
  if Threads.Task[0].IsRunning or Threads.Task[1].IsRunning then
  begin
    if MessageDlg('To cancel all current operations?', mtWarning, mbOKCancel, 0) = mrCancel then exit;

    Threads.FinishAllTasks(100);

    BtnChangeStatus;
    StatusBar.SimpleText:='Operation aborted by user!';
  end;
end;

procedure TFmMain.BtnRetryClick(Sender: TObject);
begin
  if MessageDlg('Repeat ping hosts again?', mtConfirmation, mbOKCancel, 0) = mrCancel then exit;

  RunPing;
end;

procedure TFmMain.BtnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    wPing.SaveToFile(SaveDialog.FileName);
end;

procedure TFmMain.CommandLineGetFileName;
begin
if (Length(ParamStrUTF8(1))>0) and FileExists(ParamStrUTF8(1)) then
   CheckFileName(ParamStrUTF8(1));
end;

procedure TFmMain.FormCreate(Sender: TObject);
begin
  FmMain.Caption:='iPing - '+GetVersion;
  wPing:= TwPing.Create;

  CommandLineGetFileName;
end;

procedure TFmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(wPing);
end;

procedure TFmMain.DrawImage(Sender: TObject; aRect: TRect; aIndex:integer);
begin
  with TStringGrid(Sender) do
  begin
    Canvas.FillRect(aRect);
    Canvas.TextOut(aRect.Right - 2 - Canvas.TextWidth(' '), aRect.Top + 2, ' ');
    TitleImageList.Draw(Canvas,aRect.Left,aRect.Top+2,aIndex)
  end;
end;

procedure TFmMain.GridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  aCellText: String;
begin
  aCellText:= TStringGrid(Sender).Cells[aCol, aRow];

  case aCellText of
    '0': DrawImage(Sender,aRect, 0);//red
    '1': DrawImage(Sender,aRect, 1);//arrow
    '2': DrawImage(Sender,aRect, 2);//green
  end;
end;

procedure TFmMain.TaskLoadFromFileExecute(const Sender: TTask; const Msg: Word;
  var Param: Variant);
begin
  Param:= wPing.GetHostsFromFile(Sender, Param);
end;

procedure TFmMain.TaskLoadFromFileFinish(const Sender: TTask; const Msg: Word;
  const Param: Variant);
begin
  BtnChangeStatus;

  if not Param then
  raise Exception.Create('Error loading file!');

  GridLogFill;
  StatusBar.SimpleText:='The operation completed';
  Threads.Task[1].Start;
end;

procedure TFmMain.TaskLoadFromFileProgress(const Sender: TTask;
  const Msg: Word; const Value: Word);
begin
  case Msg of
    0:
      begin
        StatusBar.SimpleText:='Working...';
        BtnChangeStatus;
        ProgressBar.Max:= Value;
        ProgressBar.Step:=1;
      end;
    1: ProgressBar.Position:= Value;
    2: ProgressBar.StepIt;
  end;
end;


procedure TFmMain.TaskPingExecute(const Sender: TTask; const Msg: Word;
  var Param: Variant);
var
  i, aResult: Integer;
begin
   Sender.PostProgress(0,wPing.HostsCount);
   Sender.PostProgress(1,0);

   for i:=0 to wPing.HostsCount-1 do
     begin
       Sender.PostMessage(i,-2); // set status
       aResult:= wPing.Ping(i);
       Sender.PostProgress(2,0);
       Sender.PostMessage(i,aResult); // set status
     end;
end;

procedure TFmMain.TaskPingFinish(const Sender: TTask; const Msg: Word;
  const Param: Variant);
begin
  BtnChangeStatus;
  StatusBar.SimpleText:='All operations is completed. Statistics: '+wPing.GetStatistics;
  ShowMessage('All operations is completed!');
end;

procedure TFmMain.GridLogSetStatus(aIndex: integer; aStatus: Integer; const aMS: integer = -1);
begin
  GridLog.BeginUpdate;

  GridLog.Cells[0,aIndex+1]:= IntToStr(aStatus);

  if aMS>-1 then
  begin
    GridLog.Cells[3,aIndex+1]:= IntToStr(aMS)+' ms';
    wPing.Hosts[aIndex].Time:= aMS;
    wPing.Hosts[aIndex].Result:= true;
  end
  else
  begin
    if aStatus=1 then
      GridLog.Cells[3,aIndex+1]:= 'ping...'
    else
      GridLog.Cells[3,aIndex+1]:= '<no response>';
    wPing.Hosts[aIndex].Time:= -1;
    wPing.Hosts[aIndex].Result:= false;
  end;

  GridLog.EndUpdate();
end;

procedure TFmMain.BtnChangeStatus;
begin
  BtnClear.Enabled:= not BtnClear.Enabled;
  BtnSave.Enabled:= not BtnSave.Enabled;
  BtnRetry.Enabled:= not BtnRetry.Enabled;
  btnAbort.Enabled:= not btnAbort.Enabled;
end;

procedure TFmMain.TaskPingMessage(const Sender: TTask; const Msg: Word;
  const Param: Variant);
begin
    case Param of
      -1: GridLogSetStatus(Msg,0);
      -2: GridLogSetStatus(Msg,1)
      else
        begin
          GridLogSetStatus(Msg,2,Param);
        end;
    end;
end;

end.

