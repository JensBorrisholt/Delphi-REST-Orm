program Demo;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {Form59},
  DelphiRestOrm.ORM.Request in '..\ORM\DelphiRestOrm.ORM.Request.pas',
  DelphiRestOrm.ORM.Helper.ObjectContainer in '..\ORM\Helpers\DelphiRestOrm.ORM.Helper.ObjectContainer.pas',
  DelphiRestOrm.ORM.IRequest in '..\ORM\DelphiRestOrm.ORM.IRequest.pas',
  PersonU in 'PersonU.pas',
  DelphiRestOrm.ORM.Response in '..\ORM\DelphiRestOrm.ORM.Response.pas',
  DelphiRestOrm.ORM.Helper.GenericListHelper in '..\ORM\Helpers\DelphiRestOrm.ORM.Helper.GenericListHelper.pas',
  DelphiRestOrm.ORM.Helper.Azure in '..\ORM\Helpers\DelphiRestOrm.ORM.Helper.Azure.pas',
  DelphiRestOrm.ORM.Helper.ThreadingEx in '..\ORM\Helpers\DelphiRestOrm.ORM.Helper.ThreadingEx.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := true;
  Application.CreateForm(TForm59, Form59);
  Application.Run;

end.
