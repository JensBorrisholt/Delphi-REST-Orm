unit DelphiRestOrm.ORM.Helper.BearerAuth;

interface

uses
  System.Classes, System.SysUtils,

  REST.Json.Types, REST.Client,

  DelphiRestOrm.ORM.Helper.CustomBindAuthenticator;
{$M+}

type
  TBearerAccessToken = class
  strict private
    FContainsToken: Boolean;
    [JSONName('tokenType')]
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

  TBearerAuthenticator = class(TCustomBindAuthenticator)
  private type
    TClientInformation = class
    strict private
      FClientId: string;
      FClientSecret: string;
    public
      property ClientId: string read FClientId write FClientId;
      property ClientSecret: string read FClientSecret write FClientSecret;
    end;
  private
    FClientInformation: TClientInformation;
    FAuthorizeEndPoint: string;
    procedure SetAuthorizeEndPoint(const Value: string);
    procedure SetClientId(const Value: string);
    procedure SetClientSecret(const Value: string);
    function GetClientId: string;
    function GetClientSecret: string;
  protected
    function GetAccessToken: TBearerAccessToken; virtual;
    procedure DoAuthenticate(ARequest: TCustomRESTRequest); override;
  published
    property AuthorizeEndPoint: string read FAuthorizeEndPoint write SetAuthorizeEndPoint;
    property ClientId: string read GetClientId write SetClientId;
    property ClientSecret: string read GetClientSecret write SetClientSecret;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  REST.Types,
  DelphiRestOrm.ORM.IRequest;

{ TBearerAuthenticator }

constructor TBearerAuthenticator.Create(AOwner: TComponent);
begin
  inherited;
  FClientInformation := TClientInformation.Create;
end;

destructor TBearerAuthenticator.Destroy;
begin
  FClientInformation.Free;
  inherited;
end;

procedure TBearerAuthenticator.DoAuthenticate(ARequest: TCustomRESTRequest);
var
  AccessToken: TBearerAccessToken;
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

function TBearerAuthenticator.GetAccessToken: TBearerAccessToken;
begin
  Result := NewRequest(AuthorizeEndPoint).AddBody(FClientInformation, ctAPPLICATION_JSON).Post.AsType<TBearerAccessToken>;

  Result.ContainsToken := (Result.TokenType = 'Bearer') and (Result.AccessToken.Length > 0);
end;

function TBearerAuthenticator.GetClientId: string;
begin
  Result := FClientInformation.ClientId
end;

function TBearerAuthenticator.GetClientSecret: string;
begin
  Result := FClientInformation.ClientSecret;
end;

procedure TBearerAuthenticator.SetAuthorizeEndPoint(const Value: string);
begin
  FAuthorizeEndPoint := Value;
end;

procedure TBearerAuthenticator.SetClientId(const Value: string);
begin
  if (Value <> FClientInformation.ClientId) then
  begin
    FClientInformation.ClientId := Value;
    PropertyValueChanged;
  end;
end;

procedure TBearerAuthenticator.SetClientSecret(const Value: string);
begin
  if (Value <> FClientInformation.ClientSecret) then
  begin
    FClientInformation.ClientSecret := Value;
    PropertyValueChanged;
  end;
end;

end.
