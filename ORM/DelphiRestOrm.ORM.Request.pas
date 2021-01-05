unit DelphiRestOrm.ORM.Request;

interface

uses
  System.Classes, System.Rtti, System.Generics.Collections, System.Threading, REST.Client, REST.Types,

  DelphiRestOrm.ORM.Helper.ObjectContainer, DelphiRestOrm.ORM.Helper.Azure, DelphiRestOrm.ORM.IRequest, DelphiRestOrm.ORM.Response;
{$M+}

type
  TRestRequest = class(TInterfacedObject, IRestRequest)
  private
    FObjectContainer: TObjectContainer;
    FExternalObjectContainer: TObjectContainer;
    FDTOObjectContainer: TObjectContainer;
    FClient: TRESTClient;
    FRequest: TCustomRESTRequest;
    FResponse: TRESTResponse;
    FBaseURL: string;
    FAsyncCall: Boolean;

    FQueryParams: TDictionary<string, string>;
    FCompletionHandler: TOrmCompletionHandler;
    function SetAuthenticator(aCustomAuthenticator: TCustomAuthenticator): IRestRequest;
    function DoExecute(aRESTRequestMethod: TRESTRequestMethod): TResponse; overload;
    function DoExecute(aObject: TObject; aRESTRequestMethod: TRESTRequestMethod): TResponse; overload;
    function InternalParam(aValue: TValue): TRestRequest;
  public
    constructor Create(const aBaseApiURL: string; aCompletionHandler: TOrmCompletionHandler = nil); reintroduce;
    destructor Destroy; override;
    function Param(aValue: TValue): IRestRequest;

    function AddAuthParameter(const AName, aValue: string; const AKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkGetOrPost; const AOptions: TRESTRequestParameterOptions = [])
      : IRestRequest;
    function AddBody(const aBodyContent: string; aContentType: TRESTContentType = ctNone): IRestRequest; overload;
    function AddBody(aObject: TObject; aContentType: TRESTContentType = ctDefault): IRestRequest; overload;
    function AddBody(aBodyContent: TStream; aContentType: TRESTContentType = ctNone; AOwnsStream: TRESTObjectOwnership = ooCopy): IRestRequest; overload;
    function AddBody(aObject: TObject): IRestRequest; overload;
    function ClearBody: IRestRequest;
    function AddFile(const AName, AFileName: string; aContentType: TRESTContentType = TRESTContentType.ctNone): IRestRequest; overload;
    function AddFile(const AFileName: string; aContentType: TRESTContentType = TRESTContentType.ctNone): IRestRequest; overload;
    function AddHeader(const AName, aValue: string; const AOptions: TRESTRequestParameterOptions = [poDoNotEncode]): IRestRequest; overload;
    function ResetToDefaults: IRestRequest;
    function Cancel: IRestRequest;
    function Accept(aContentType: TRESTContentType = ctDefault; const AOptions: TRESTRequestParameterOptions = [poDoNotEncode]): IRestRequest;
    function ContentType(aContentType: TRESTContentType = ctDefault; const AOptions: TRESTRequestParameterOptions = [poDoNotEncode]): IRestRequest;
    function AzureCredentials(const aTennantID, aClientID, aClientSecret, aResourceId: string): IRestRequest; overload;
    function AzureCredentials(const aAzureAuthenticator: TAzureAuthenticator): IRestRequest; overload;
    function Credentials(const aUsername, aPassword: string): IRestRequest;
    function ObjectContainer(var aObjectContainer: TObjectContainer): IRestRequest;
    function IsCancelled: Boolean;
    function Post: TResponse; overload;
    function Post(aObject: TObject): TResponse; overload;
    function Post(aValue: TValue; aObject: TObject): TResponse; overload;
    function Put: TResponse; overload;
    function Put(aObject: TObject): TResponse; overload;
    function Put(aValue: TValue; aObject: TObject): TResponse; overload;
    function GET: TResponse; overload;
    function GET(aObject: TObject): TResponse; overload;
    function GET(aValue: TValue): TResponse; overload;
    function DELETE: TResponse; overload;
    function DELETE(aObject: TObject): TResponse; overload;
    function DELETE(aValue: TValue): TResponse; overload;
    function PATCH: TResponse; overload;
    function PATCH(aObject: TObject): TResponse; overload;
    function PATCH(aValue: TValue; aObject: TObject): TResponse; overload;

    function BaseApiURL: string;
    function ConnectionTimeOut(const aValue: Integer = 30000): IRestRequest;
    function OwnsObjects(const aValue: Boolean = True): IRestRequest;
    function QueryParam(const AName: string; aValue: TValue): IRestRequest;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,

  REST.Authenticator.Basic, DelphiRestOrm.ORM.Helper.ThreadingEx;

{ TRestRequest }

function TRestRequest.Accept(aContentType: TRESTContentType = ctDefault; const AOptions: TRESTRequestParameterOptions = [poDoNotEncode]): IRestRequest;
begin
  AddHeader('Accept', ContentTypeToString(aContentType), AOptions);
  Result := Self;
end;

function TRestRequest.AddBody(aBodyContent: TStream; aContentType: TRESTContentType; AOwnsStream: TRESTObjectOwnership): IRestRequest;
begin
  FRequest.AddBody(aBodyContent, aContentType, AOwnsStream);
  Result := Self;
end;

function TRestRequest.AddBody(aObject: TObject; aContentType: TRESTContentType): IRestRequest;
begin
  AddBody(aObject.ToString, aContentType);
  Result := Self;
end;

function TRestRequest.AddBody(const aBodyContent: string; aContentType: TRESTContentType): IRestRequest;
begin
  FRequest.AddBody(aBodyContent, aContentType);
  Result := Self;
end;

function TRestRequest.AddFile(const AFileName: string; aContentType: TRESTContentType): IRestRequest;
begin
  FRequest.AddFile(AFileName, aContentType);
  Result := Self;
end;

function TRestRequest.AddHeader(const AName, aValue: string; const AOptions: TRESTRequestParameterOptions): IRestRequest;
begin
  FRequest.AddParameter(AName, aValue, TRESTRequestParameterKind.pkHTTPHEADER, AOptions);
  Result := Self;
end;

function TRestRequest.AzureCredentials(const aAzureAuthenticator: TAzureAuthenticator): IRestRequest;
begin
  Result := SetAuthenticator(aAzureAuthenticator);
end;

function TRestRequest.Param(aValue: TValue): IRestRequest;
begin
  Result := QueryParam('', aValue);
end;

function TRestRequest.AzureCredentials(const aTennantID, aClientID, aClientSecret, aResourceId: string): IRestRequest;
begin
  Result := SetAuthenticator(TAzureAuthenticator.Create(aTennantID, aClientID, aClientSecret, aResourceId));
end;

function TRestRequest.BaseApiURL: string;
begin
  Result := FBaseURL;
end;

function TRestRequest.AddFile(const AName, AFileName: string; aContentType: TRESTContentType): IRestRequest;
begin
  FRequest.AddFile(AName, AFileName, aContentType);
  Result := Self;
end;

function TRestRequest.Cancel: IRestRequest;
begin
  FRequest.Cancel;
  Result := Self;
end;

function TRestRequest.ClearBody: IRestRequest;
begin
  FRequest.ClearBody;
  Result := Self;
end;

constructor TRestRequest.Create(const aBaseApiURL: string; aCompletionHandler: TOrmCompletionHandler = nil);
begin
  FObjectContainer := TObjectContainer.Create;
  FBaseURL := aBaseApiURL;

  FClient := TRESTClient.Create(aBaseApiURL);
  FRequest := TCustomRESTRequest.Create(FClient);
  FResponse := TRESTResponse.Create(FClient);
  FRequest.Response := FResponse;

  FDTOObjectContainer := FObjectContainer.Add(TObjectContainer.Create(False));
  FQueryParams := FObjectContainer.Add(TDictionary<string, string>.Create);
  FCompletionHandler := aCompletionHandler;
  FAsyncCall := Assigned(aCompletionHandler);

  FExternalObjectContainer := nil;
end;

function TRestRequest.DELETE: TResponse;
begin
  Result := DoExecute(rmDELETE);
end;

function TRestRequest.DELETE(aObject: TObject): TResponse;
begin
  Result := DoExecute(aObject, rmDELETE);
end;

function TRestRequest.DELETE(aValue: TValue): TResponse;
begin
  Result := InternalParam(aValue).DoExecute(rmDELETE);
end;

destructor TRestRequest.Destroy;
begin
  if not FAsyncCall then
    FreeAndNil(FClient);

  FreeAndNil(FObjectContainer);
  inherited;
end;

function TRestRequest.DoExecute(aObject: TObject; aRESTRequestMethod: TRESTRequestMethod): TResponse;
begin
  FDTOObjectContainer.Add(aObject);
  AddBody(aObject.ToString, ctDefault);
  Result := DoExecute(aRESTRequestMethod);
end;

function TRestRequest.DoExecute(aRESTRequestMethod: TRESTRequestMethod): TResponse;
  function GetURL: string;
  var
    Value: string;
    Pair: TPair<string, string>;
  begin
    Result := FBaseURL;
    if not Result.EndsWith('/') then
      Result := Result + '/';

    if FQueryParams.TryGetValue('', Value) then
    begin
      Result := Result + Value;
      FQueryParams.Remove('');
    end
    else
      Result := Result.Substring(0, Length(Result) - 1);

    if FQueryParams.Count > 0 then
      Result := Result + '?';

    for Pair in FQueryParams do
      Result := Result + Pair.Key + '=' + Pair.Value;

  end;

var
  LCLient: TRESTClient;
  LRequest: TCustomRESTRequest;
  LCompletionHandler: TOrmCompletionHandler;

begin
  LCLient := FClient;
  LRequest := FRequest;
  LCompletionHandler := FCompletionHandler;

  FClient.BaseURL := GetURL;
  FRequest.Method := aRESTRequestMethod;

  if not FAsyncCall then
  begin
    LRequest.Execute;
    Exit(FObjectContainer.Add(TResponse.Create(LRequest.Response, FExternalObjectContainer)));
  end;

  Result := nil;

  LRequest.ExecuteAsync(
      procedure
    var
      Response: TResponse;
      ObjectContainer: TObjectContainer;
    begin
      ObjectContainer := TObjectContainer.Create;
      ObjectContainer.Add(FClient);

      if Assigned(LCompletionHandler) then
      begin
        Response := ObjectContainer.Add(TResponse.Create(LRequest.Response, ObjectContainer));
        LCompletionHandler(Response);
      end;

      FreeAndNil(ObjectContainer);
    end, True, True,
    procedure(aErrorObject: TObject)
    begin
      FreeAndNil(LCLient);
    end);

end;

function TRestRequest.GET(aObject: TObject): TResponse;
begin
  Result := DoExecute(aObject, rmGET);
end;

function TRestRequest.GET: TResponse;
begin
  Result := DoExecute(rmGET);
end;

function TRestRequest.InternalParam(aValue: TValue): TRestRequest;
begin
  Result := Param(aValue) as TRestRequest;
end;

function TRestRequest.IsCancelled: Boolean;
begin
  Result := FRequest.IsCancelled;
end;

function TRestRequest.ObjectContainer(var aObjectContainer: TObjectContainer): IRestRequest;
begin
  if not Assigned(aObjectContainer) then
    aObjectContainer := TObjectContainer.Create;

  FExternalObjectContainer := aObjectContainer;
  Result := Self;
end;

function TRestRequest.OwnsObjects(const aValue: Boolean): IRestRequest;
begin
  FDTOObjectContainer.OwnsObjects := aValue;
  Result := Self;
end;

function TRestRequest.SetAuthenticator(aCustomAuthenticator: TCustomAuthenticator): IRestRequest;
begin
  FObjectContainer.Remove<TCustomAuthenticator>;
  FClient.Authenticator := FObjectContainer.Add(aCustomAuthenticator);
  Result := Self;
end;

function TRestRequest.PATCH: TResponse;
begin
  Result := DoExecute(rmPATCH);
end;

function TRestRequest.Post: TResponse;
begin
  Result := DoExecute(rmPOST);
end;

function TRestRequest.Put: TResponse;
begin
  Result := DoExecute(rmPUT);
end;

function TRestRequest.ResetToDefaults: IRestRequest;
begin
  FRequest.ResetToDefaults;
  Result := Self;
end;

function TRestRequest.Credentials(const aUsername, aPassword: string): IRestRequest;
begin
  Result := SetAuthenticator(THTTPBasicAuthenticator.Create(aUsername, aPassword))
end;

function TRestRequest.ConnectionTimeOut(const aValue: Integer): IRestRequest;
begin
  FClient.ConnectTimeout := aValue;
end;

function TRestRequest.ContentType(aContentType: TRESTContentType; const AOptions: TRESTRequestParameterOptions): IRestRequest;
begin
  AddHeader('Content-Type', ContentTypeToString(aContentType), AOptions);
  Result := Self;
end;

function TRestRequest.PATCH(aObject: TObject): TResponse;
begin
  Result := DoExecute(aObject, rmPATCH);
end;

function TRestRequest.Post(aObject: TObject): TResponse;
begin
  Result := DoExecute(aObject, rmPOST);
end;

function TRestRequest.Put(aObject: TObject): TResponse;
begin
  Result := DoExecute(aObject, rmPUT);
end;

function TRestRequest.AddAuthParameter(const AName, aValue: string; const AKind: TRESTRequestParameterKind; const AOptions: TRESTRequestParameterOptions): IRestRequest;
begin
  FRequest.AddAuthParameter(AName, aValue, AKind, AOptions);
  Result := Self;
end;

function TRestRequest.AddBody(aObject: TObject): IRestRequest;
begin
  FDTOObjectContainer.Add(aObject);
  Result := AddBody(aObject.ToString, ctDefault);
end;

function TRestRequest.GET(aValue: TValue): TResponse;
begin
  Result := InternalParam(aValue).DoExecute(TRESTRequestMethod.rmGET);
end;

function TRestRequest.PATCH(aValue: TValue; aObject: TObject): TResponse;
begin
  Result := InternalParam(aValue).DoExecute(aObject, rmPATCH);
end;

function TRestRequest.Post(aValue: TValue; aObject: TObject): TResponse;
begin
  Result := InternalParam(aValue).DoExecute(aObject, rmPOST);
end;

function TRestRequest.Put(aValue: TValue; aObject: TObject): TResponse;
begin
  Result := InternalParam(aValue).DoExecute(aObject, rmPUT);
end;

function TRestRequest.QueryParam(const AName: string; aValue: TValue): IRestRequest;
var
  s: string;
begin
  if FQueryParams.TryGetValue(AName, s) then
    FQueryParams[AName] := aValue.ToString
  else
    FQueryParams.Add(AName, aValue.ToString);

  Result := Self;
end;

end.
