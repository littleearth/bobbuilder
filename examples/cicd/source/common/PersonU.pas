unit PersonU;

interface

uses
  System.SysUtils, System.Variants, System.Classes;

type
  TPerson = class
  private
    FLastName: string;
    FDateOfBirth: TDate;
    FFirstName: string;
    procedure SetDateOfBirth(const Value: TDate);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    function GetAge: integer;
  public
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property DateOfBirth: TDate read FDateOfBirth write SetDateOfBirth;
    property Age: integer read GetAge;
  end;

implementation

uses
  System.DateUtils;

{ TPerson }

function TPerson.GetAge: integer;
begin
  Result := YearsBetween(Now, FDateOfBirth);
end;

procedure TPerson.SetDateOfBirth(const Value: TDate);
begin
  FDateOfBirth := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

end.
