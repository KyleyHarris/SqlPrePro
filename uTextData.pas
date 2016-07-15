unit uTextData;

interface

uses
  classes;

type

  TTextDataType = (dtCompiled ,dtProc, dtInclude, dtMacro, dtFunc, dtView);

  THsTextData = class(TCollectionItem)
  private
    FSQL:string;
    FSQLName:string;
    FFileName: string;
    FLoadFileName: string;
    FFolder: string;
    FLoadedName: string;
    FSqlType: TTextDataType;
    FIsModified: Boolean;
    procedure SetFolder(const Value: string);
    function GetDisplayText: string;
    procedure SetSqlType(const Value: TTextDataType);
  protected
    procedure SetSQL(const Value:string);
    procedure SetSQLName(const Value:string);
  public
    procedure SaveToDisk;
    procedure LoadFromDisk(aFileName: string);
    function ActiveItemName: string;
    property DisplayText: string read GetDisplayText;

    property IsModified: Boolean read FIsModified;
  published
    property SQL:string read FSQL write SetSQL;
    property SQLName:string read FSQLName write SetSQLName;
    property Folder: string read FFolder write SetFolder;
    property FileName:string read FFileName;
    property SqlType: TTextDataType read FSqlType write SetSqlType;
  end;

  TTextDatas = class(TCollection)
  private
    function GetItem(Index: Integer): THsTextData;
    procedure SetItem(Index: Integer; const Value: THsTextData);
    function GetFirst:THsTextData;
    function GetLast:THsTextData;
  public
    constructor Create;
    function Add: THsTextData;
    function Insert(Index: Integer): THsTextData;
    function IsModified: Boolean;
    property Items[Index: Integer]: THsTextData read GetItem write SetItem;default;
    property First:THsTextData read GetFirst;
    property Last:THsTextData read GetLast;
  end;

implementation

uses
{$IFDEF FPC}
  LazFileUtils,
{$ELSE}
  uFileUtils,
{$ENDIF}
  SysUtils,
  SynEdit,
  SynHighlighterSQL;


{ THsTextData }

procedure THsTextData.SaveToDisk;
begin
  if IsModified then
  begin
    with TStringList.Create do
    try
      Text := SQL;
      SqlName := ActiveItemName;
      if SqlName = '[UNKNOWN]' then
        raise Exception.Create('Cannot Save SQL without correct syntax and name.' + sLineBreak + sql);
      Insert(0,SQLName);
      FFileName := Folder + '\' + SqlName + '.sql';
      SaveToFile(FileName);
      if FLoadFileName <> FileName then
      begin
        DeleteFileUTF8(FLoadFileName);
        FLoadFileName := FileName;
      end;
      FLoadedName := SqlName;
    finally
      Free;
    end;
    FIsModified := False;
  end;
end;

procedure THsTextData.LoadFromDisk(aFileName: string);
var
  sl: TStringList;
begin
  if FileExistsUTF8(aFileName) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(aFileName);
      FFileName := aFileName;
      if sl.Count > 0 then
        SQLName := sl[0];
      sl.Delete(0);
      Sql := sl.Text;
      FIsModified := False;
      FLoadFileName := aFileName;
      FLoadedName := SqlName;
      Folder := ExcludeTrailingPathDelimiter(ExtractFilePath(FLoadFileName));
    finally
      sl.Free;
    end;
  end;
end;

procedure THsTextData.SetFolder(const Value: string);
begin
  FFolder := Value;
end;

procedure THsTextData.SetSQL(const Value:string);
begin
  if Value <> FSQL then
  begin
    FSQL := Value;
    FIsModified := True;
  end;
end;


procedure THsTextData.SetSQLName(const Value:string);
begin
  if Value <> FSQLName then
  begin
    FSQLName := Copy(Value,1,250);
    FIsModified := True;
  end;
end;

procedure THsTextData.SetSqlType(const Value: TTextDataType);
begin
  FSqlType := Value;
end;

function THsTextData.ActiveItemName: string;
var
  Source:string;
  Token:string;

begin
  Result := '[UNKNOWN]';
  with TSynSQLSyn.Create(nil) do
  try
    Source := Sql;
    ResetRange;
    SetLine(Source,1);
    while not GetEol do
    begin
      Token := GetToken;

      if
      ( (GetTokenKind = ord(tkIdentifier)) and ( SameText('macro',token) )) or
      ( (GetTokenKind = ord(tkIdentifier)) and ( SameText('include',token) )) or
      ( (GetTokenKind = ord(tkKey)) and (  SameText('view',token) or SameText('procedure',token) or SameText('trigger',token) or SameText('function',token) or SameText('script',token)) ) then
      begin
        repeat
        Next;
        until GetEOL or
         (
           (GetTokenKind <> ord(tkspace)) and
           (GetTokenKind <> ord(tkComment))
         )
           ;

        Token := GetToken;
        if GetTokenKind = ord(tkIdentifier) then
        begin
          Result := Token;
        end;
        exit;
      end;

      Next;

    end;
  finally
    Free;
  end;

end;

function THsTextData.GetDisplayText: string;
begin
  if not SameText(SqlName, FLoadedName) then
    Result := Format('%s WAS %s',[SqlName, FLoadedName]) else
    Result := SQLName;
end;

{ TTextDatas }

function TTextDatas.Add: THsTextData;
begin
  Result := inherited Add as THsTextData;
end;

constructor TTextDatas.Create;
begin
  inherited Create(THsTextData);
end;

function TTextDatas.GetFirst:THsTextData;
begin
  result := nil;
  if Count > 0 then
    result := Items[0];
end;

function TTextDatas.GetLast:THsTextData;
begin
  result := nil;
  if Count > 0 then
    result := Items[Count-1];
end;

function TTextDatas.GetItem(Index: Integer): THsTextData;
begin
  Result := inherited Items[Index] as THsTextData;
end;

function TTextDatas.Insert(Index: Integer): THsTextData;
begin
  Result := inherited Insert(Index) as THsTextData;
end;

function TTextDatas.IsModified: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if Items[i].IsModified then
    begin
      Result := True;
      exit;
    end;
end;

procedure TTextDatas.SetItem(Index: Integer; const Value: THsTextData);
begin
  inherited Items[Index] := Value;
end;


end.
