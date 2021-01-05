unit DelphiRestOrm.ORM.Helper.Azure;

interface

uses
  System.SysUtils, System.Classes, Data.Bind.ObjectScope,
  REST.Authenticator.Basic, REST.Client, REST.Types, REST.Json.Types;

{$M+}

type
  TCustomBindAuthenticator = class(TCustomAuthenticator)
  private
    FBindSource: TSubHTTPBasicAuthenticationBindSource;
  protected
    function CreateBindSource: TBaseObjectBindSource; override;
  published
    property BindSource: TSubHTTPBasicAuthenticationBindSource read FBindSource;
  end;

  TAccessToken = class
  strict private
    FContainsToken: Boolean;
    [JSONName('token_type')]
    FTokenType: string;
    [JSONName('expires_in')]
    FExpiresIn: Integer;
    [JSONName('ext_expires_in')]
    FResource: string;
    [JSONName('access_token')]
    FAccessToken: string;
  public
    property ContainsToken: Boolean read FContainsToken write FContainsToken;
    property TokenType: string read FTokenType write FTokenType;
    property ExpiresIn: Integer read FExpiresIn write FExpiresIn;
    property Resource: string read FResource write FResource;
    property AccessToken: string read FAccessToken write FAccessToken;
  end;

  TAzureAuthenticator = class(TCustomBindAuthenticator)
  private
    FClientId: string;
    FClientSecret: string;
    FTenantId: string;
    FResourceId: string;
    FAuthorizeEndPoint: string;
    procedure SetTenantId(const Value: string);
    procedure SetAuthorizeEndPoint(const Value: string);
    procedure SetClientId(const Value: string);
    procedure SetClientSecret(const Value: string);
    procedure SetResourceId(const Value: string);
    function GetAccessToken: TAccessToken;
  protected
    procedure DoAuthenticate(ARequest: TCustomRESTRequest); override;
  published
    property AuthorizeEndPoint: string read FAuthorizeEndPoint write SetAuthorizeEndPoint;
    property ClientId: string read FClientId write SetClientId;
    property TenantId: string read FTenantId write SetTenantId;
    property ClientSecret: string read FClientSecret write SetClientSecret;
    property ResourceId: string read FResourceId write SetResourceId;
  public
    constructor Create(const aTenantID, aClientID, aClientSecret, aResourceId: string); reintroduce; overload;
  end;

implementation

uses
  DelphiRestOrm.ORM.IRequest;

{ TAzureAuthenticator }

constructor TAzureAuthenticator.Create(const aTenantID, aClientID, aClientSecret, aResourceId: string);
begin
  inherited Create(nil);
  TenantId := aTenantID;
  ClientId := aClientID;
  ClientSecret := aClientSecret;
  ResourceId := aResourceId;
end;

procedure TAzureAuthenticator.DoAuthenticate(ARequest: TCustomRESTRequest);
var
  AccessToken: TAccessToken;
  Param: TRESTRequestParameter;
  BodyParam: TRESTRequestParameter;
begin
  AccessToken := GetAccessToken;
  ARequest.Params.AddItem('Authorization', AccessToken.TokenType + ' ' + AccessToken.AccessToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  ARequest.AddParameter('Content-Type', ContentTypeToString(ctAPPLICATION_JSON), TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  ARequest.AddParameter('Accept', ContentTypeToString(ctAPPLICATION_JSON), TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

  BodyParam := nil;
  for Param in ARequest.Params do
    if Param.Kind = TRESTRequestParameterKind.pkREQUESTBODY then
    begin
      BodyParam := Param;
      Break;
    end;

  if (BodyParam <> nil) and (BodyParam.Value <> '') then
    ARequest.Params.AddItem('Content-Length', (BodyParam.Value.Length).ToString, TRESTRequestParameterKind.pkHTTPHEADER);

  FreeAndNil(AccessToken);
end;

function TAzureAuthenticator.GetAccessToken: TAccessToken;
begin
  Result :=
    NewRequest(AuthorizeEndPoint)
    .AddAuthParameter('grant_type', 'client_credentials')
    .AddAuthParameter('client_id', ClientId)
    .AddAuthParameter('client_secret', ClientSecret)
    .AddAuthParameter('resource', ResourceId)
    .Post.AsType<TAccessToken>;

  Result.ContainsToken := (Result.TokenType = 'Bearer') and (Result.AccessToken.Length > 0);
end;

procedure TAzureAuthenticator.SetAuthorizeEndPoint(const Value: string);
begin
  if (Value <> FAuthorizeEndPoint) then
  begin
    FAuthorizeEndPoint := Value;
    PropertyValueChanged;
  end;
end;

procedure TAzureAuthenticator.SetClientId(const Value: string);
begin
  if (Value <> FClientId) then
  begin
    FClientId := Value;
    PropertyValueChanged;
  end;
end;

procedure TAzureAuthenticator.SetClientSecret(const Value: string);
begin
  if (Value <> FClientSecret) then
  begin
    FClientSecret := Value;
    PropertyValueChanged;
  end;
end;

procedure TAzureAuthenticator.SetResourceId(const Value: string);
begin
  if (Value <> FResourceId) then
  begin
    FResourceId := Value;
    PropertyValueChanged;
  end;
end;

procedure TAzureAuthenticator.SetTenantId(const Value: string);
const
  AuthorizeEndPointTemplate = 'https://login.microsoftonline.com/{tenant}/oauth2/token';
begin
  if (Value <> FTenantId) then
  begin
    FTenantId := Value;
    PropertyValueChanged;
  end;

  AuthorizeEndPoint := AuthorizeEndPointTemplate.Replace('{tenant}', FTenantId);
end;

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
