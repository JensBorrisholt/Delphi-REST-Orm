unit DelphiRestOrm.ORM.Helper.CustomBindAuthenticator;

interface

uses
  Data.Bind.ObjectScope, REST.Client, REST.Authenticator.Basic;

type
  TCustomBindAuthenticator = class(TCustomAuthenticator)
  private
    FBindSource: TSubHTTPBasicAuthenticationBindSource;
  protected
    function CreateBindSource: TBaseObjectBindSource; override;
  published
    property BindSource: TSubHTTPBasicAuthenticationBindSource read FBindSource;
  end;

implementation

{ TCustomBindAuthenticator }

function TCustomBindAuthenticator.CreateBindSource: TBaseObjectBindSource;
begin
  FBindSource := TSubHTTPBasicAuthenticationBindSource.Create(Self);
  FBindSource.Name := 'BindSource'; { Do not localize }
  FBindSource.SetSubComponent(True);
  FBindSource.Authenticator := Self;
  Result := FBindSource;
end;

end.
