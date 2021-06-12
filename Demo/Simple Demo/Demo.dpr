program Demo;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {Form59},
  DelphiRestOrm.ORM.Request in '..\..\ORM\DelphiRestOrm.ORM.Request.pas',
  DelphiRestOrm.ORM.Helper.ObjectContainer in '..\..\ORM\Helpers\DelphiRestOrm.ORM.Helper.ObjectContainer.pas',
  DelphiRestOrm.ORM.IRequest in '..\..\ORM\DelphiRestOrm.ORM.IRequest.pas',
  DelphiRestOrm.ORM.Response in '..\..\ORM\DelphiRestOrm.ORM.Response.pas',
  DelphiRestOrm.ORM.Helper.GenericListHelper in '..\..\ORM\Helpers\DelphiRestOrm.ORM.Helper.GenericListHelper.pas',
  DelphiRestOrm.ORM.Helper.Azure in '..\..\ORM\Helpers\DelphiRestOrm.ORM.Helper.Azure.pas',
  DelphiRestOrm.ORM.Helper.ThreadingEx in '..\..\ORM\Helpers\DelphiRestOrm.ORM.Helper.ThreadingEx.pas',
  PersonU in '..\DTO\PersonU.pas',
  PersonsU in '..\DTO\PersonsU.pas',
  Pkg.Json.DTO in '..\DTO\Pkg.Json.DTO.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := true;
  Application.CreateForm(TForm59, Form59);
  Application.Run;

end.
