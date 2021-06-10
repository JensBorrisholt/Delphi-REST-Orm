# Delphi-REST-Orm
Delphi Rest Orm - At simple  yet powerful ORM to query a WebService

## syntax Examples ##

``` pascal

type
  TPerson = class
  private
   ...
    FAddress: string;
   ...
  published
    ...
    property Address: string read FAddress write FAddress;
   ...
  end;


// Make the GET- requeest to a webservice, singin using basic authentication, and then parses the result into a type strong ObjecttList. 
var
  Persons: TObjectList<TPerson>;
begin
  Persons := NewRequest('https://localhost:44303/Contact').Credentials('test', 'test').GET.AsType<TObjectList<TPerson>>;
  Memo1.Lines.Text := '(Random) Person.Address :' + Persons[Random(Persons.Count)].Address;
  Persons.Free;
end;


// Make a GET request, add a paramater, and return the raw content from the call 
var
  s: string;
begin
  s := NewRequest('https://localhost:44303/Contact').GET(8).ToString;
  Memo1.Lines.Add(s);
end;


// Make a GET request to a Azure service. Using both a paramater and a query parameter. Returns the ras content 
var
  s: string;
  AzureAuthenticator: TAzureAuthenticator;
begin
  AzureAuthenticator := TAzureAuthenticator.Create('<TenantID>', '<ClientID>', '<ClientSecret>', '<ResourceId>');
  s := NewRequest('<URL>').Param(17794452).QueryParam('country', 'DK').AzureCredentials(AzureAuthenticator).GET.ToString;
  Memo1.Lines.Add(s);
end;

// Make an async GET request to a Azure service. Using both a paramater and a query parameter. The response are recived inside a callback function
var
  AzureAuthenticator: TAzureAuthenticator;
begin
  AzureAuthenticator := TAzureAuthenticator.Create('<TenantID>', '<ClientID>', '<ClientSecret>', '<ResourceId>');

  NewRequestAsync('<URL>',
    procedure(const Response: TResponse)
    begin
      Memo1.Lines.Add(Response.ToString)
    end).Param(17794452).QueryParam('country', 'DK').AzureCredentials(AzureAuthenticator).GET;
end;

```



Features : 

This OrM can easy query a WEB Service. Featuring automatic serialization and deserialization, request and response type detection, variety of authentications and other useful features-

## Serialization ##
JSON serialization and deserialization

## Authentication ##
Basic, Azure are supported. Not enough? Write your own!

## Sync and Async ##
Variety of overloads to make synchronous and asynchronous HTTP calls

## Parameters ##
Add query, URL segment, body, form or header parameter using an easy and fluent API
