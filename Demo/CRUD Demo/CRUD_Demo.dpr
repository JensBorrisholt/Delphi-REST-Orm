program CRUD_Demo;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {FormMain},
  PersonsU in '..\DTO\PersonsU.pas',
  PersonU in '..\DTO\PersonU.pas',
  Pkg.Json.DTO in '..\DTO\Pkg.Json.DTO.pas',
  PersonEditor in 'PersonEditor.pas' {FormPersonEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  ReportMemoryLeaksOnShutdown := True;
  Application.Run;
end.
