unit DelphiRestOrm.ORM.Response;

interface

uses
  REST.Client, DelphiRestOrm.ORM.Helper.ObjectContainer;

type
  TResponse = class abstract
  strict protected
    FResponse: TCustomRESTResponse;
    FExternalObjectContainer: TObjectContainer;
    class function FromString(aContent: string; aObject: TObject): TObject; virtual;
  public
    constructor Create(aResponse: TCustomRESTResponse; aExternalObjectContainer: TObjectContainer);
    destructor Destroy; override;
    function AsType<T: class, constructor>: T; overload;
    function AsType<T: class, constructor>(aObject: T): T; overload;
    function ToString: string; override;
  end;

  TJsonResponse = class(TResponse)
  public
    class function FromString(aContent: string; aObject: TObject): TObject; override;
  end;

implementation

uses
  System.SysUtils, System.Generics.Collections, System.RTTI, System.JSONConsts, System.Json,
  REST.JsonReflect, REST.Json, REST.Types,

  DelphiRestOrm.ORM.Helper.GenericListHelper;

{ TResponse }
function TResponse.AsType<T>: T;
begin
  Result := AsType(T.Create);
end;

function TResponse.AsType<T>(aObject: T): T;
begin
  if Assigned(FExternalObjectContainer) then
    FExternalObjectContainer.Add(aObject);

  case ContentTypeFromString(FResponse.ContentType) of
    ctAPPLICATION_JSON, ctAPPLICATION_VND_EMBARCADERO_FIREDAC_JSON:
      Result := TJsonResponse.FromString(FResponse.Content, aObject) as T;
  else
    raise ENotImplemented.Create('AsType are not implemented for content kind ' + FResponse.ContentType)
  end
end;

constructor TResponse.Create(aResponse: TCustomRESTResponse; aExternalObjectContainer: TObjectContainer);
begin
  inherited Create;
  FResponse := aResponse;
  FExternalObjectContainer := aExternalObjectContainer;
end;

destructor TResponse.Destroy;
begin
  inherited;
end;

class function TResponse.FromString(aContent: string; aObject: TObject): TObject;
begin
  raise ENotImplemented.Create('Not Imlemented')
end;

function TResponse.ToString: string;
begin
  Result := FResponse.Content;
end;

{ TJsonResponse }

class function TJsonResponse.FromString(aContent: string; aObject: TObject): TObject;
const
  DefaultOptions = [joDateIsUTC, joDateFormatISO8601];
var
  JSONValue: TJsonValue;
  JSONObject: TJSONObject;
  RttiProperty: TRttiProperty;
begin
  Result := aObject;

 (*
    Yes! I favor my own lib Delphi-JsonToDelphiClass.
    https://github.com/JensBorrisholt/Delphi-JsonToDelphiClass
 *)
  RttiProperty := TRttiContext.Create.GetType(aObject.ClassType).GetProperty('AsJson');

  if (RttiProperty <> nil) and (RttiProperty.IsWritable) then
  begin
    RttiProperty.SetValue(aObject, aContent);
    exit;
  end;

  JSONValue := TJSONObject.ParseJSONValue(aContent);

  try
    if not Assigned(JSONValue) then
      exit;

    if (JSONValue is TJSONArray) then
    begin
      GenericListHelper.UnMarshall(aObject, JSONValue as TJSONArray);
      exit;
    end;

    if (JSONValue is TJSONObject) then
      JSONObject := JSONValue as TJSONObject
    else
    begin
      if (aContent = '') and not Assigned(JSONValue) or (aContent <> '') and Assigned(JSONValue) and JSONValue.Null then
        exit
      else
        raise EConversionError.Create(SCannotCreateObject);
    end;

    TJson.JsonToObject(Result, JSONObject, DefaultOptions);
  finally
    JSONValue.Free;
  end;
end;

end.
