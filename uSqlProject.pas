unit uSqlProject;

interface

uses
  SysUtils,
  Classes,
  uTextData;

type

  TTextDataMethod = procedure(aTextData: THsTextData) of object;

  TSqlProject = class(TObject)
  private
    FProjectFolder: string;
    FTextDatas:array[TTextDataType] of TTextDatas;
    procedure SetProjectFolder(const Value: string);
    function GetFolder(aType: TTextDataType): string;
    function GetSqlList(aType: TTextDataType): TTextDatas;
    procedure LoadProject;
    procedure AddFilesToTextData(aFiles: TStrings; aSqlList: TTextDatas; aType: TTextDataType);
    function GetModified: Boolean;
    class procedure AddQuickFind(AFileName, APath: string; AResults: TStrings; ASearchSubFolders: Boolean=False); static;


  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure SaveToDisk;

    property Modified: Boolean read GetModified;

    procedure IterateAll(aMethod: TTextDataMethod);

    function SqlNameExists(aName: string): Boolean;
    function TextDataByName(aName: string; out aTextData: THsTextData; out aType: TTextDataType): Boolean;
    property ProjectFolder: string read FProjectFolder write SetProjectFolder;
    property Folder[aType: TTextDataType]: string read GetFolder;
    property SqlList[aType: TTextDataType]: TTextDatas read GetSqlList;
    property Macros: TTextDatas read FTextDatas[dtMacro];
    property Includes: TTextDatas read FTextDatas[dtInclude];
    property Procs: TTextDatas read FTextDatas[dtProc];
    property Funcs: TTextDatas read FTextDatas[dtFunc];
    property Views: TTextDatas read FTextDatas[dtView];
  end;

implementation

uses
{$IFDEF FPC}
  LazFileUtils,
{$ELSE}
  uFileUtils,
{$ENDIF}
  Masks;

const
  DataTypeToFolderName : array[TTextDataType] of string = ('','Proc','Include','Macro','Func','View','Trigger');

{ TSqlProject }

procedure TSqlProject.AfterConstruction;
var
  i: TTextDataType;
begin
  inherited;
  FTextDatas[dtNone] := nil;
  for i := Succ(dtNone) to High(TTextDataType) do
    FTextDatas[i] := TTextDatas.Create;

end;

procedure TSqlProject.BeforeDestruction;
var
  i: TTextDataType;
begin
  inherited;
  for i := Succ(dtNone) to High(TTextDataType) do
  begin
    FreeAndNil(FTextDatas[i]);
  end;
end;

function TSqlProject.GetFolder(aType: TTextDataType): string;
begin
  Result := ProjectFolder + PathDelim + DataTypeToFolderName[aType];
  if not DirectoryExistsUTF8(Result) then
    CreateDirUTF8(Result);
end;

function TSqlProject.GetModified: Boolean;
var
  dt: TTextDataType;
begin
  Result := False;
  for dt := Succ(dtNone) to High(TTextDataType) do
  begin
    if SqlList[dt].IsModified then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TSqlProject.GetSqlList(aType: TTextDataType): TTextDatas;
begin
  Result := FTextDatas[aType];
end;

procedure TSqlProject.IterateAll(aMethod: TTextDataMethod);
var
  dt: TTextDataType;
  List: TTextDatas;
  i: Integer;
begin
  for dt := Succ(dtNone) to High(TTextDataType) do
  begin
    List := SqlList[dt];
    for i := 0 to List.Count -1 do
    begin
      aMethod(List[i]);
    end;
  end;
end;


class procedure TSqlProject.AddQuickFind(AFileName, APath: string;  AResults: TStrings; ASearchSubFolders: Boolean);

  procedure Iterate(const ABrowsePath:string; const AFolderPrefix: String);
  var
    S:TSearchRec;
    Dirs:TStringList;
    i:integer;
    FileName : String;
  begin
    Dirs := TStringList.Create;
    try
      if FindFirstUTF8(ABrowsePath + PathDelim + '*.*',faAnyFile,s) = 0 then
      try
        repeat
          FileName := ABrowsePath + PathDelim + s.Name;
          if DirectoryExistsUTF8(FileName) then
          begin
            if (Pos(AFolderPrefix,s.name)=0) and (s.Name <> '.') and (s.Name <> '..') then
              Dirs.Add(FileName);
          end
          else
          begin
            if MatchesMask(s.Name,aFileName) then
            begin
              AResults.Add(FileName);
            end;
          end;
        until (FindNextUTF8(s) <> 0) ;
      finally
        FindCloseUTF8(s);
      end;

      if ASearchSubfolders  then
      begin
        for i := 0 to Dirs.Count -1 do
         Iterate(Dirs[i],AFolderPrefix);
      end;
    finally
      FreeAndNil(Dirs);
    end;
  end;

begin
  Iterate(APath, '.' + PathDelim);
end;


procedure TSqlProject.LoadProject;
const
  SQL_FILE = '*.sql';
var
  FileList: TStringList;
  i: TTextDataType;
begin
  FileList := TStringList.Create;
  try
    for i := Succ(dtNone) to High(TTextDataType) do
    begin
      FileList.Clear;
      AddQuickFind(SQL_FILE, Folder[i], FileList);
      SqlList[i].Clear;
      AddFilesToTextData(FileList, SqlList[i], i);
    end;
  finally
    FileList.Free;
  end;
end;

procedure TSqlProject.AddFilesToTextData(aFiles: TStrings; aSqlList: TTextDatas; aType: TTextDataType);
var
  i: Integer;
  TextData: THsTextData;
begin
  for i := 0 to aFiles.Count - 1 do
  begin
    TextData := aSqlList.Add;
    TextData.SqlType := aType;
    TextData.LoadFromDisk(aFiles[i]);
  end;
end;


procedure TSqlProject.SaveToDisk;
var
  dt: TTextDataType;
  List: TTextDatas;
  i: Integer;
begin

  for dt := Succ(dtNone) to High(TTextDataType) do
  begin
    List := SqlList[dt];
    for i := 0 to List.Count -1 do
    begin
      if List[i].IsModified then
        List[i].SaveToDisk;
    end;
  end;
end;


procedure TSqlProject.SetProjectFolder(const Value: string);
begin
  FProjectFolder := Value;
  LoadProject;
end;

function TSqlProject.TextDataByName(aName: string; out aTextData: THsTextData;
 out aType: TTextDataType): Boolean;
var
  dt: TTextDataType;
  List: TTextDatas;
  i: Integer;
begin
  Result := False;

  for dt := Succ(dtNone) to High(TTextDataType) do
  begin
    List := SqlList[dt];
    for i := 0 to List.Count -1 do
    begin
      Result := SameText(List[i].SqlName, aName);
      if Result then
      begin
        aTextData := List[i];
        aType := dt;
        Exit;
      end;
    end;
  end;
end;

function TSqlProject.SqlNameExists(aName: string): Boolean;
var
  dt: TTextDataType;
  List: TTextDatas;
  i: Integer;
begin
  Result := False;

  for dt := Succ(dtNone) to High(TTextDataType) do
  begin
    List := SqlList[dt];
    for i := 0 to List.Count -1 do
    begin
      Result := SameText(List[i].SqlName, aName);
      if Result then
        Exit;
    end;
  end;
end;

end.
