program HsSqlEditor;

uses
{$IFDEF FPC}
  Interfaces,
{$ENDIF}
  Forms,
  SqlEditorMain in 'SqlEditorMain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Sql Pre Proccessor';
  Application.CreateForm(TSqlEditorMainFrm, SqlEditorMainFrm);
  Application.Run;
end.
