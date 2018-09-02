unit FmMainU;
{
 wofs(c)2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1
 Git: https://github.com/wofs/iPing
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Grids, ComCtrls, StdCtrls, wcthread, LazUTF8, Types, math, lclintf, UtilsU,
  pingsend;

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
    fHostsFile: TStringList;
    fHosts: THosts;
    fFileName: string;
    procedure BtnChangeStatus;
    function GetStatistics: string;
    procedure GridLogFill;
    procedure GridLogSetStatus(aIndex: integer; aStatus: Integer; const aMS: integer
      =-1);
    procedure HostAdd(aRow: integer; aString: String);
    procedure DrawImage(Sender: TObject; aRect: TRect; aIndex: integer);
    function GetHostsFromFile(const Sender: TTask; aFileName: string): boolean;
    function ParseString(aTextLine: string): THost;
    function Ping(var aPingSend: TPINGSend; aIndex: integer): integer;
    procedure RunPing;
  public

  end;

var
  FmMain: TFmMain;

implementation

{$R *.lfm}

{ TFmMain }

function TFmMain.ParseString(aTextLine: string): THost;
var
  aSpacePosition: PtrInt;
  aText: string;
begin
  aText:='';
  aText:= Trim(aTextLine);
  aSpacePosition:= UTF8Pos(#32,aText);
  if aSpacePosition>0 then
  begin
    Result.Host:=Trim(UTF8Copy(aText,1,aSpacePosition));
    Result.Description:=Trim(UTF8Copy(aText,aSpacePosition, UTF8Length(aText)));
  end else
  begin
    Result.Host:=Trim(aText);
    Result.Description:='';
  end;

end;

procedure TFmMain.HostAdd(aRow:integer; aString:String);
var
  aIpRecord: THost;
begin
  aIpRecord:= ParseString(aString);
  fHosts[aRow].Host:= aIpRecord.Host;
  fHosts[aRow].Description:= aIpRecord.Description;;
end;

procedure TFmMain.GridLogFill;
var
  i: Integer;
begin
  GridLog.BeginUpdate;

  GridLog.RowCount:=0;
  GridLog.RowCount:=Length(fHosts)+1;

  for i:=0 to High(fHosts) do
    begin
      GridLog.Cells[1,i+1]:= fHosts[i].Host;
      GridLog.Cells[2,i+1]:= fHosts[i].Description;
    end;

  GridLog.EndUpdate();
end;

function TFmMain.GetHostsFromFile(const Sender: TTask; aFileName: string
  ): boolean;
var
  i: Integer;
begin
  Result:= false;
  try
    fHostsFile:= TStringList.Create;
    fHostsFile.Clear;
    fHostsFile.LoadFromFile(aFileName);

    SetLength(fHosts,fHostsFile.Count);
    Sender.PostProgress(0,fHostsFile.Count);
    Sender.PostProgress(1,0);

    for i:=0 to fHostsFile.Count-1 do
      begin
        HostAdd(i, fHostsFile.Strings[i]);
        Sender.PostProgress(2,0);
      end;

    Result:= true;
  finally
    FreeAndNil(fHostsFile);
  end;

end;

procedure TFmMain.RunPing;
begin
  if FileExists(fFileName) then
     Threads.Task[0].Start(fFileName);
end;

procedure TFmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  aFileExt: string;
begin
  if Threads.Task[0].IsRunning or Threads.Task[1].IsRunning then exit;

  fFileName:= FileNames[0];
  aFileExt:= LowerCase(ExtractFileExt(fFileName));

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

procedure TFmMain.BtnClearClick(Sender: TObject);
begin
  if MessageDlg('To clear the list of hosts?', mtWarning, mbOKCancel, 0) = mrCancel then exit;

  fHosts:= nil;
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
var
  aFileNameSave, aResult, aResponse: String;
  i, aMS: Integer;
begin
  if GridLog.RowCount = 1 then exit;

  if SaveDialog.Execute then
  begin
    aFileNameSave:= SaveDialog.FileName;
    fHostsFile:= TStringList.Create;

    try
      for i:=0 to High(fHosts) do
        begin
          if fHosts[i].Result then aResult:= 'YES' else aResult:= 'NO';
          aMS:= fHosts[i].Time;

          if aMS>-1 then
            aResponse:= IntToStr(aMS)+' ms'
          else
            aResponse:= '<no response>';

          fHostsFile.Append(aResult+'|'+fHosts[i].Host+'|'+fHosts[i].Description+'|'+aResponse);
        end;
      fHostsFile.SaveToFile(aFileNameSave);
    finally
      FreeAndNil(fHostsFile);
    end;

    if MessageDlg('To open a saved file?', mtConfirmation, mbOKCancel, 0) = mrOK then OpenDocument(aFileNameSave);
  end;
end;

procedure TFmMain.FormCreate(Sender: TObject);
begin
  FmMain.Caption:='iPing - '+GetVersion;
end;

procedure TFmMain.DrawImage(Sender: TObject; aRect: TRect; aIndex:integer);
begin
  TStringGrid(Sender).Canvas.FillRect(aRect);
  TStringGrid(Sender).Canvas.TextOut(aRect.Right - 2 - TStringGrid(Sender).Canvas.TextWidth(' '), aRect.Top + 2, ' ');
  TStringGrid(Sender).TitleImageList.Draw(TStringGrid(Sender).Canvas,aRect.Left,aRect.Top+2,aIndex)
end;

procedure TFmMain.GridLogDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  aCellText: String;
begin
  aCellText:= TStringGrid(Sender).Cells[aCol, aRow];


  case aCellText of
    '0': DrawImage(Sender,aRect, 0);//red
    '1': DrawImage(Sender,aRect, 1);//blue
    '2': DrawImage(Sender,aRect, 2);//green
  end;

end;

procedure TFmMain.TaskLoadFromFileExecute(const Sender: TTask; const Msg: Word;
  var Param: Variant);
begin
  Param:= GetHostsFromFile(Sender, Param);
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

function TFmMain.GetStatistics:string;
var
  aOK, aFAIL, i: Integer;
  aAVG: double;
begin
  Result:= '';
  aOK:= 0;
  aFAIL:= 0;
  aAVG:= 0.0;
  if not Assigned(fHosts) then exit;

  for i:=0 to High(fHosts) do
    begin
      if fHosts[i].Result then
      begin
        Inc(aOK);
        aAVG:= aAVG+fHosts[i].Time;
      end
      else
        Inc(aFAIL);
    end;

  if aOK>0 then
    aAVG:= RoundTo(aAVG/aOK,-1);

  Result:= '| OK:'+IntToStr(aOK)+' | FAIL:'+IntToStr(aFAIL)+' | Average response time:'+FloatToStr(aAVG)+' ms |';
end;

function TFmMain.Ping(var aPingSend: TPINGSend; aIndex: integer):integer;
begin
  Result:= -1;

  if aPingSend.Ping(fHosts[aIndex].Host) then
    Result:= aPingSend.PingTime;

end;

procedure TFmMain.TaskPingExecute(const Sender: TTask; const Msg: Word;
  var Param: Variant);
var
  aPingSend: TPINGSend;
  i, aResult: Integer;
begin
  aPingSend := TPINGSend.Create;

  try
     aPingSend.Timeout := 1500;

     Sender.PostProgress(0,Length(fHosts));
     Sender.PostProgress(1,0);

     for i:=0 to High(fHosts) do
       begin
         Sender.PostMessage(i,-2); // set status
         aResult:= Ping(aPingSend,i);
         Sender.PostProgress(2,0);
         Sender.PostMessage(i,aResult); // set status
       end;

    finally
      aPingSend.Free;
    end;
end;

procedure TFmMain.TaskPingFinish(const Sender: TTask; const Msg: Word;
  const Param: Variant);
begin
  BtnChangeStatus;
  StatusBar.SimpleText:='All operations is completed. Statistics: '+GetStatistics;
  ShowMessage('All operations is completed!');
end;

procedure TFmMain.GridLogSetStatus(aIndex: integer; aStatus: Integer; const aMS: integer = -1);
begin
  GridLog.BeginUpdate;

  GridLog.Cells[0,aIndex+1]:= IntToStr(aStatus);

  if aMS>-1 then
  begin
    GridLog.Cells[3,aIndex+1]:= IntToStr(aMS)+' ms';
    fHosts[aIndex].Time:= aMS;
    fHosts[aIndex].Result:= true;
  end
  else
  begin
    if aStatus=1 then
      GridLog.Cells[3,aIndex+1]:= 'ping...'
    else
      GridLog.Cells[3,aIndex+1]:= '<no response>';
    fHosts[aIndex].Time:= -1;
    fHosts[aIndex].Result:= false;
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

