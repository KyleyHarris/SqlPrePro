program HsSqlCompiler;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uCmdCompiler in 'uCmdCompiler.pas',
  uCmdParams in 'uCmdParams.pas',
  uFileUtils in 'uFileUtils.pas',
  uLogger in 'uLogger.pas',
  uSqlGenerator in 'uSqlGenerator.pas',
  uSqlProject in 'uSqlProject.pas',
  uTextData in 'uTextData.pas';



begin
  try

    try
      CompilePath(SwitchValue('p'));
    except
      Halt(1);
    end;

    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
