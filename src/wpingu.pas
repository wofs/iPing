unit wPingU;
{
 wofs(c)2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1
 Git: https://github.com/wofs/iPing
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  pingsend, wcthread, LazUTF8, Dialogs, Controls, LCLIntf;

type

  THost = record
    Host:string;
    Description:string;
    Result: boolean;
    Time: integer;
  end;

  THosts = array of THost;  // array of hosts

  { TwPing }

  TwPing = class
  private
    fFileName: string;
    fHosts: THosts;
    fHostsFile: TStringList;
    fPingSend: TPINGSend;
    function GetHostsCount: integer;
    function GetTimeOut: integer;
    procedure SetTimeOut(aValue: integer);

  public
    constructor Create;
    destructor Destroy; override;

    function ParseLineHost(aTextLine:string):THost;
    function GetHostsFromFile(const Sender: TTask; aFileName: string): boolean;
    function GetStatistics:string;
    function Ping(aIndex: integer): integer;
    procedure HostsClear;
    procedure SaveToFile(aFileName: string);
    procedure HostAdd(aRow:integer; aString:String);

    property HostsFile: TStringList read fHostsFile write fHostsFile;
    property Hosts: THosts read fHosts write fHosts;
    property HostsCount: integer read GetHostsCount;
    property FileName: string read fFileName write fFileName;
    property TimeOut: integer read GetTimeOut write SetTimeOut;
  end;

implementation

{ TwPing }

function TwPing.GetHostsCount: integer;
begin
  Result:= Length(fHosts);
end;

function TwPing.GetTimeOut: integer;
begin
  Result:= fPingSend.Timeout;
end;

procedure TwPing.SetTimeOut(aValue: integer);
begin
  fPingSend.Timeout := aValue;
end;

constructor TwPing.Create;
begin
  fPingSend := TPINGSend.Create;
  Timeout := 500;
end;

destructor TwPing.Destroy;
begin
  fPingSend.Free;
  inherited Destroy;
end;

function TwPing.ParseLineHost(aTextLine: string): THost;
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

function TwPing.GetHostsFromFile(const Sender: TTask; aFileName: string
  ): boolean;
var
  i: Integer;
begin
  Result:= false;
  fHostsFile:= TStringList.Create;

  try
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

function TwPing.GetStatistics: string;
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

function TwPing.Ping(aIndex: integer): integer;
begin
  Result:= -1;

  if fPingSend.Ping(fHosts[aIndex].Host) then
    Result:= fPingSend.PingTime;
end;

procedure TwPing.HostsClear;
begin
  fHosts:= nil;
end;

procedure TwPing.SaveToFile(aFileName:string);
var
  aResult, aResponse: String;
  i, aMS: Integer;
begin
  fHostsFile:= TStringList.Create;
    try
      for i:=0 to HostsCount do
        begin
          if Hosts[i].Result then aResult:= 'YES' else aResult:= 'NO';
          aMS:= Hosts[i].Time;

          if aMS>-1 then
            aResponse:= IntToStr(aMS)+' ms'
          else
            aResponse:= '<no response>';

          HostsFile.Append(aResult+'|'+Hosts[i].Host+'|'+Hosts[i].Description+'|'+aResponse);
        end;
      HostsFile.SaveToFile(aFileName);
    finally
      FreeAndNil(fHostsFile);
    end;

  if MessageDlg('To open a saved file?', mtConfirmation, mbOKCancel, 0) = mrOK then OpenDocument(aFileName);
end;

procedure TwPing.HostAdd(aRow: integer; aString: String);
var
  aIpRecord: THost;
begin
  aIpRecord:= ParseLineHost(aString);
  fHosts[aRow].Host:= aIpRecord.Host;
  fHosts[aRow].Description:= aIpRecord.Description;
end;

end.

