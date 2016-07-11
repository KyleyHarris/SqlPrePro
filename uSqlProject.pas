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
    FIncludes: TTextDatas;
    FMacros: TTextDatas;
    FFuncs: TTextDatas;
    FProcs: TTextDatas;
    FViews: TTextDatas;
    FProjectFolder: string;
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
    property Macros: TTextDatas read FMacros;
    property Includes: TTextDatas read FIncludes;
    property Procs: TTextDatas read FProcs;
    property Funcs: TTextDatas read FFuncs;
    property Views: TTextDatas read FViews;
  end;

implementation

uses
{$IFDEF FPC}
  LazFileUtils,
{$ELSE}
  uFileUtils,
{$ENDIF}
  Masks;

{ TSqlProject }

procedure TSqlProject.AfterConstruction;
begin
  inherited;
  FMacros := TTextDatas.Create;
  FFuncs := TTextDatas.Create;
  FProcs := TTextDatas.Create;
  FViews := TTextDatas.Create;
  FIncludes := TTextDatas.Create;
end;

procedure TSqlProject.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FMacros);
  FreeAndNil(FFuncs);
  FreeAndNil(FIncludes);
  FreeAndNil(FProcs);
  FreeAndNil(FViews);
end;

function TSqlProject.GetFolder(aType: TTextDataType): string;
begin
  case aType of
    dtNone: Result := Format('%s\%s',[ProjectFolder, 'Compiled']);
    dtProc: Result := Format('%s\%s',[ProjectFolder, 'Proc']);
    dtInclude: Result := Format('%s\%s',[ProjectFolder, 'Include']);
    dtMacro: Result := Format('%s\%s',[ProjectFolder, 'Macro']);
    dtFunc: Result := Format('%s\%s',[ProjectFolder, 'Func']);
    dtView: Result := Format('%s\%s',[ProjectFolder, 'View']);
  end;
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
  case atype of
    dtNone: Result := nil ;
    dtProc: Result := Procs;
    dtInclude: Result := Includes;
    dtMacro: Result := Macros;
    dtFunc: Result := Funcs;
    dtView: Result := Views;
    else Result := nil;
  end;
end;

procedure TSqlProject.IterateAll(aMethod: TTextDataMethod);
var
  dt: TTextDataType;
  List: TTextDatas;
  i: Integer;
begin
  for dt :=Succ(dtNone) to High(TTextDataType) do
  begin
    List := SqlList[dt];
    for i := 0 to List.Count -1 do
    begin
      aMethod(List[i]);
    end;
  end;
end;


class procedure TSqlProject.AddQuickFind(AFileName, APath: string;  AResults: TStrings; ASearchSubFolders: Boolean);

  procedure Iterate(ABrowsePath:string);
  var
    S:TSearchRec;
    Dirs:TStringList;
    i:integer;
  begin
    Dirs := TStringList.Create;
    try
      if FindFirstUTF8(ABrowsePath+'\*.*',faAnyFile,s) = 0 then
      try
        repeat
          if DirectoryExistsUTF8(ABrowsePath+'\'+s.Name) then
          begin
            if (Pos( '.\',s.name)=0) and (s.Name <> '.') and (s.Name <> '..') then
              Dirs.Add(ABrowsePath+'\'+s.Name);
          end else
          if  MatchesMask(s.Name,aFileName) then
          begin
            AResults.Add(lowercase(ABrowsePath+'\'+s.Name));
          end;
        until (FindNextUTF8(s) <> 0) ;

        if ASearchSubfolders  then
          for i := 0 to Dirs.Count -1 do
           Iterate(Dirs[i]);

      finally
        FindCloseUTF8(s); { *Converted from FindClose* }
      end;
    finally
      FreeAndNil(Dirs);
    end;

  end;
begin
  Iterate(APath);
end;


procedure TSqlProject.LoadProject;
var
  FileList: TStringList;
begin
  FileList := TStringList.Create;
  try
    AddQuickFind('*.sql', Folder[dtMacro], FileList);
    FMacros.Clear;
    AddFilesToTextData(FileList, FMacros, dtMacro);

    FileList.Clear;
    AddQuickFind('*.sql', Folder[dtInclude], FileList);
    FIncludes.Clear;
    AddFilesToTextData(FileList, FIncludes, dtInclude);

    FileList.Clear;
    AddQuickFind('*.sql', Folder[dtProc], FileList);
    FProcs.Clear;
    AddFilesToTextData(FileList, FProcs, dtProc);

    FileList.Clear;
    AddQuickFind('*.sql', Folder[dtView], FileList);
    FViews.Clear;
    AddFilesToTextData(FileList, FViews, dtView);

    FileList.Clear;
    AddQuickFind('*.sql', Folder[dtFunc], FileList);
    FViews.Clear;
    AddFilesToTextData(FileList, FFuncs, dtFunc);


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

  for dt :=Succ(dtNone) to High(TTextDataType) do
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

  for dt :=Succ(dtNone) to High(TTextDataType) do
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

  for dt :=Succ(dtNone) to High(TTextDataType) do
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
