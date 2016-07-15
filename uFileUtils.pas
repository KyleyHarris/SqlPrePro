unit uFileUtils;

interface

uses
  SysUtils;

    procedure DeleteFileUTF8(const AFileName : string);
    function FileExistsUTF8(const AFileName: string) : Boolean;
    function FindFirstUTF8(const APath: string; AAttribute: Integer; var ARecord: TSearchRec): Integer;
    function DirectoryExistsUTF8(const APath: string): Boolean;
    function FindNextUTF8(var ARecord: TSearchRec): Integer;
    procedure MakeDirectoryUTF8(const AFolder: string);
    procedure FindCloseUTF8(var ARecord: TSearchRec);

implementation


procedure DeleteFileUTF8(const AFileName: string);
begin
  DeleteFile(AFileName);
end;

function FileExistsUTF8(const AFileName: string) : Boolean;
begin
  Result := FileExists(AFileName);
end;

function FindFirstUTF8(const APath: string; AAttribute: Integer; var ARecord: TSearchRec): Integer;
begin
  Result := FindFirst(APath,AAttribute,ARecord);
end;

procedure MakeDirectoryUTF8(const AFolder: string);
begin
  MkDir(AFolder);
end;

function DirectoryExistsUTF8(const APath: string): Boolean;
begin
  Result := DirectoryExists(APath);
end;

function FindNextUTF8(var ARecord: TSearchRec): Integer;
begin
  Result := FindNext(ARecord);
end;

procedure FindCloseUTF8(var ARecord: TSearchRec);
begin
  FindClose(ARecord);
end;


end.
