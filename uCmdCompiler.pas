unit uCmdCompiler;

interface

function CompilePath(path: string): Boolean;


implementation

uses
  classes,  uSqlProject, uSqlGenerator, uLogger, uTextData, System.SysUtils;

type

  TCompiler = class(TComponent,ILogger)
  private
    FGenerator: TSqlGenerator;
    FProject: TSqlProject;
    procedure CompileTextData(aTextData: THsTextData);
    procedure Log(AValue: string);
    function FolderForCompiledSQL : string;
  public
    constructor Create(path: string);
    destructor Destroy; override;
    procedure Run;
  end;

procedure TCompiler.CompileTextData(aTextData: THsTextData);
var
  fileName: String;
  linesOfCode: TStrings;
begin
  if aTextData.SqlType in [dtMacro, dtInclude] then
    exit;
  linesOfCode := TStringList.Create;
  try
    try
      linesOfCode.Text := FGenerator.CompileSql(aTextData.SQL);
      fileName := FolderForCompiledSQL + PathDelim + Format('%s.sql',[aTextData.SqlName]);
      linesOfCode.SaveToFile(fileName);
      Log(Format('Saved: %s.sql',[aTextData.SqlName]));
    except
      on e:exception do
      begin
        Log(Format('ERROR Processing %s',[aTextData.SQLName]));
        Log(e.Message);
        Log(StringOfChar('-',40));
        Log('');
        Halt(1);
      end;
    end;
  finally
    linesOfCode.Free;
  end;
end;

function CompilePath(path: string): Boolean;
begin
  with TCompiler.Create(path) do
  try
    Run;
  finally
    Free;
  end;
end;


{ TLogger }

procedure TCompiler.Log(AValue: string);
begin
  WriteLn(aValue);
end;

constructor TCompiler.Create(path: string);
var
  sProjectFolder: string;
begin
  sProjectFolder := path;
{$IFDEF FPC}
  DoDirSeparators(sProjectFolder);
{$ENDIF}
  FProject := TSqlProject.Create;
  FProject.ProjectFolder := sProjectFolder;
  FGenerator := TSqlGenerator.Create(FProject.Macros, FProject.Includes, self);
end;

destructor TCompiler.Destroy;
begin
  FreeAndNil(FGenerator);
  FreeAndNil(FProject);
  inherited;
end;

function TCompiler.FolderForCompiledSQL: string;
begin
  Result := FProject.ProjectFolder + PathDelim + 'Compiled';
end;

procedure TCompiler.Run;
begin
  Log('Compiling SQL Routines');
  FProject.IterateAll(CompileTextData);
end;

end.
