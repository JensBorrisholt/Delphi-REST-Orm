unit DelphiRestOrm.ORM.IRequest;

interface

uses
  System.Classes, System.Rtti, Rest.Client, Rest.Types,

  DelphiRestOrm.ORM.Response, DelphiRestOrm.ORM.Helper.ObjectContainer, DelphiRestOrm.ORM.Helper.Azure;

const
  ctDefault = ctAPPLICATION_JSON;

type
  TOrmCompletionHandler = reference to procedure(const Response: TResponse);

  IRestRequest = interface
    ['{C117D3C1-B571-4CD9-A571-3C720DE689CE}']

    /// <summary>
    /// Adds ABodyContent as Body parameter to the request.
    /// </summary>
    /// <remarks>
    /// An already existing body parameter will NOT be replaced/deleted!
    /// </remarks>
    function AddBody(const aBodyContent: string; aContentType: TRESTContentType = ctNone): IRestRequest; overload;

    /// <summary>
    /// Encodes the string representation of a JSON Object and adds it to the request body.
    /// </summary>
    /// <param name="AObject">The object to serialize, uning the objects ToString method</param>
    /// <param name="AOwnsObject">Defines who will own the object.</param>
    /// <remarks>
    /// An already existing body parameter will be replaced/deleted!
    /// </remarks>
    function AddBody(AObject: TObject; aContentType: TRESTContentType = ctDefault): IRestRequest; overload;

    /// <summary>
    /// Add an authentication parameter
    /// </summary>
    function AddAuthParameter(const AName, aValue: string; const AKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkGetOrPost; const AOptions: TRESTRequestParameterOptions = [])
      : IRestRequest;

    /// <summary>
    /// Adds ABodyContent as Body parameter to the request.
    /// </summary>
    /// <remarks>
    /// An already existing body parameter will be replaced/deleted!
    /// </remarks>
    function AddBody(aBodyContent: TStream; aContentType: TRESTContentType = ctNone; AOwnsStream: TRESTObjectOwnership = ooCopy): IRestRequest; overload;

    /// <summary>
    /// Adds ABodyContent as Body parameter to the request.
    /// </summary>
    /// <remarks>
    /// An already existing body parameter will be replaced/deleted!
    /// </remarks>
    function AddBody(AObject: TObject): IRestRequest; overload;

    /// <summary>
    /// Removes all body parameters (TRESTRequestParameterKind.pkREQUESTBODY) from the request.
    /// </summary>
    function ClearBody: IRestRequest;

    /// <summary>
    /// Adds file content to the request. If same AName parameter already exists, then the previous content
    /// will be replaced by new one.
    /// </summary>
    /// <param name="AName">Name of the parameter</param>
    /// <param name="AFileName">Path to the file</param>
    /// <param name="AContentType">Content type of the file, for HTTP body parameter.
    /// If AContentType=ctNone program automatically define the Content type of the file</param>
    function AddFile(const AName, AFileName: string; aContentType: TRESTContentType = TRESTContentType.ctNone): IRestRequest; overload;

    /// <summary>
    /// Adds file content to the request. Only one file can be added.
    /// If this function is executed second time, then previous content will be replaced by new one.
    /// </summary>
    /// <param name="AFileName">Path to the file</param>
    /// <param name="AContentType">Content type of the file, for HTTP body parameter.
    /// If AContentType=ctNone program automatically define the Content type of the file</param>
    function AddFile(const AFileName: string; aContentType: TRESTContentType = TRESTContentType.ctNone): IRestRequest; overload;

    /// <summary>
    /// Adds a parameter to the header. If a parameter of the same name already exists, then the previous parameter
    /// will be removed and freed.<br /><br />There are five kinds of parameters: - GetOrPost: Either a QueryString
    /// value or encoded form value based on method - HttpHeader: Adds the name/value pair to the HTTP request's
    /// Headers collection - UrlSegment: Inserted into URL if there is a matching url token e.g. {AccountId} -
    /// Cookie: Adds the name/value pair to the HTTP request's Cookies collection - RequestBody: Used by AddBody()
    /// (not recommended to use directly)
    /// </summary>
    /// <param name="AName">
    /// Name of the parameter
    /// </param>
    /// <param name="AValue">
    /// Value of the parameter
    /// </param>
    /// <param name="AKind">
    /// The kind of parameter to add
    /// </param>
    /// <param name="AOptions">
    /// Options for processing the parameter
    /// </param>
    function AddHeader(const AName, aValue: string; const AOptions: TRESTRequestParameterOptions = [poDoNotEncode]): IRestRequest; overload;

    /// <summary>
    /// Sets all fields to their default value. No state information is kept. If Response
    /// is assigned, then Response.ResetToDefaults is called too.
    /// </summary>
    function ResetToDefaults: IRestRequest;

    /// <summary>
    /// Cancels REST request. This will not raise an exception.
    /// Check ARequest.IsCancelled that the request was cancelled.
    /// </summary>
    function Cancel: IRestRequest;
    /// <summary>
    /// Scpecifies when the request was cancalled using Cancel call.
    /// </summary>

    function Accept(aContentType: TRESTContentType = ctDefault; const AOptions: TRESTRequestParameterOptions = [poDoNotEncode]): IRestRequest;
    function ContentType(aContentType: TRESTContentType = ctDefault; const AOptions: TRESTRequestParameterOptions = [poDoNotEncode]): IRestRequest;
    function AzureCredentials(const aTennantID, aClientID, aClientSecret, aResourceId: string): IRestRequest; overload;
    function AzureCredentials(const aAzureAuthenticator: TAzureAuthenticator): IRestRequest; overload;
    function Credentials(const aUsername, aPassword: string): IRestRequest;
    function ConnectionTimeOut(const aValue: Integer = 30000): IRestRequest;
    function ObjectContainer(var aObjectContainer: TObjectContainer): IRestRequest;
    function OwnsObjects(const aValue: Boolean = True): IRestRequest;
    function QueryParam(const AName: string; aValue: TValue): IRestRequest;
    function Param(aValue: TValue): IRestRequest;

    function IsCancelled: Boolean;
    /// <summary>
    /// Sends a NEW object/entity to the server.
    /// </summary>
    function Post: TResponse; overload;

    /// <summary>
    /// Sends a NEW object/entity to the server.
    /// </summary>
    function Post(AObject: TObject): TResponse; overload;

    /// <summary>
    /// Sends a NEW object/entity to the server.
    /// </summary>
    function Post(aValue: TValue; AObject: TObject): TResponse; overload;

    /// <summary>
    /// Updates an already existing object/entity on the server. PUT may also
    /// allow for sending a new entity (depends on the actual server/API
    /// implementation).
    /// </summary>
    function Put: TResponse; overload;

    /// <summary>
    /// Updates an already existing object/entity on the server. PUT may also
    /// allow for sending a new entity (depends on the actual server/API implementation).
    /// </summary>
    function Put(AObject: TObject): TResponse; overload;

    /// <summary>
    /// Updates an already existing object/entity on the server. PUT may also
    /// allow for sending a new entity (depends on the actual server/API implementation).
    /// </summary>
    function Put(aValue: TValue; AObject: TObject): TResponse; overload;

    /// <summary>
    /// Retrieves an object/entity from the server.
    /// </summary>
    function GET: TResponse; overload;

    /// <summary>
    /// Retrieves an object/entity from the server.
    /// </summary>
    function GET(AObject: TObject): TResponse; overload;

    /// <summary>
    /// Retrieves an object/entity from the server.
    /// </summary>
    function GET(aValue: TValue): TResponse; overload;

    /// <summary>
    /// Deletes an object/entity from the server.
    /// </summary>
    function DELETE: TResponse; overload;

    /// <summary>
    /// Deletes an object/entity from the server.
    /// </summary>
    function DELETE(AObject: TObject): TResponse; overload;

    /// <summary>
    /// Deletes an object/entity from the server.
    /// </summary>
    function DELETE(aValue: TValue): TResponse; overload;

    /// <summary>
    /// Patches an object/entity on the server, by only updating the pairs that are sent within that PATCH body.
    /// </summary>
    function PATCH: TResponse; overload;

    /// <summary>
    /// Patches an object/entity on the server, by only updating the pairs that are sent within that PATCH body.
    /// </summary>
    function PATCH(AObject: TObject): TResponse; overload;

    /// <summary>
    /// Patches an object/entity on the server, by only updating the pairs that are sent within that PATCH body.
    /// </summary>
    function PATCH(aValue: TValue; AObject: TObject): TResponse; overload;

    function BaseApiURL: string;
  end;

function NewRequest(aBaseApiURL: string; aOwnsObjects: Boolean = True): IRestRequest; overload;
function NewRequestAsync(aBaseApiURL: string; aCompletionHandler: TOrmCompletionHandler; aOwnsObjects: Boolean = True): IRestRequest; overload;

implementation

uses
  DelphiRestOrm.ORM.Request;

function NewRequest(aBaseApiURL: string; aOwnsObjects: Boolean = True): IRestRequest; overload;
begin
  Result := TRestRequest.Create(aBaseApiURL).OwnsObjects(aOwnsObjects);
end;

function NewRequestAsync(aBaseApiURL: string; aCompletionHandler: TOrmCompletionHandler; aOwnsObjects: Boolean = True): IRestRequest; overload;
begin
  Result := TRestRequest.Create(aBaseApiURL, aCompletionHandler).OwnsObjects(aOwnsObjects);
end;

end.
