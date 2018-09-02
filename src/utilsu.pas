unit UtilsU;
{
 wofs(c)2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1
 Git: https://github.com/wofs/iPing
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, fileinfo;

function GetVersion: string;

implementation

  function GetVersion: string;
  var
    version: string;
    Info: TVersionInfo;
  begin
    Info := TVersionInfo.Create;
    Info.Load(HINSTANCE);
    //[0] = Major version, [1] = Minor ver, [2] = Revision, [3] = Build Number
    version := IntToStr(Info.FixedInfo.FileVersion[0]) + '.' + IntToStr(
      Info.FixedInfo.FileVersion[1]) + '.' + IntToStr(Info.FixedInfo.FileVersion[2]) +
      '.' + IntToStr(Info.FixedInfo.FileVersion[3]);
    Result := version;
    Info.Free;
  end;

end.

