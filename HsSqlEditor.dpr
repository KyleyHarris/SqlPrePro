program HsSqlEditor;

uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF }
  Forms,
  SqlEditorMain in 'SqlEditorMain.pas',
  uCmdParams in 'uCmdParams.pas';
  

{$R *.res}

begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'Sql Pre Proccessor';
    Application.CreateForm(TSqlEditorMainFrm, SqlEditorMainFrm);
    Application.Run;

end.
