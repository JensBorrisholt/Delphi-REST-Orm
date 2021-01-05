unit RootU;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TPersonDTO = class
  private
    FAbout: string;
    FAddress: string;
    FAge: Integer;
    FBalance: string;
    FCompany: string;
    FEmail: string;
    FEyeColor: string;
    FFavoriteFruit: string;
    FGender: string;
    FGreeting: string;
    FGuId: string;
    FId: string;
    FIndex: Integer;
    FIsActive: Boolean;
    FLatitude: Double;
    FLongitude: Double;
    FName: string;
    FPhone: string;
    FPicture: string;
    FRegistered: string;
  published
    property About: string read FAbout write FAbout;
    property Address: string read FAddress write FAddress;
    property Age: Integer read FAge write FAge;
    property Balance: string read FBalance write FBalance;
    property Company: string read FCompany write FCompany;
    property Email: string read FEmail write FEmail;
    property EyeColor: string read FEyeColor write FEyeColor;
    property FavoriteFruit: string read FFavoriteFruit write FFavoriteFruit;
    property Gender: string read FGender write FGender;
    property Greeting: string read FGreeting write FGreeting;
    property GuId: string read FGuId write FGuId;
    property Id: string read FId write FId;
    property Index: Integer read FIndex write FIndex;
    property IsActive: Boolean read FIsActive write FIsActive;
    property Latitude: Double read FLatitude write FLatitude;
    property Longitude: Double read FLongitude write FLongitude;
    property Name: string read FName write FName;
    property Phone: string read FPhone write FPhone;
    property Picture: string read FPicture write FPicture;
    property Registered: string read FRegistered write FRegistered;
  end;
  
  TPersons = class(TJsonDTO)
  private
    [JSONName('Items'), JSONMarshalled(False)]
    FItemsArray: TArray<TPersonDTO>;
    [GenericListReflect]
    FItems: TObjectList<TPersonDTO>;
    function GeTPersonDTO: TObjectList<TPersonDTO>;
  published
    property Items: TObjectList<TPersonDTO> read GeTPersonDTO;
  public
    destructor Destroy; override;
  end;
  
implementation

{ TRoot }

destructor TPersons.Destroy;
begin
  GeTPersonDTO.Free;
  inherited;
end;

function TPersons.GeTPersonDTO: TObjectList<TPersonDTO>;
begin
  Result := ObjectList<TPersonDTO>(FItems, FItemsArray);
end;

end.
