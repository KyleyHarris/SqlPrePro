program HsSqlEditor;

uses
  Fastmm4,
  Forms,
  SqlEditorMain in 'SqlEditorMain.pas' {SqlEditorMainFrm},
  uSqlGenerator in 'uSqlGenerator.pas',
  uTextData in 'uTextData.pas',
  uSqlProject in 'uSqlProject.pas',
  uTemplate in 'Hss\uTemplate.pas',
  uHssStringList in 'Hss\uHssStringList.pas',
  uHssStringsCSV in 'Hss\uHssStringsCSV.pas',
  uSqlEditor in 'uSqlEditor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Sql Pre Proccessor';
  Application.CreateForm(TSqlEditorMainFrm, SqlEditorMainFrm);
  Application.Run;
end.