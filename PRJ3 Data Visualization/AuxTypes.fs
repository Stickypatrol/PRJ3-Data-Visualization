module AuxTypes

type Theft =
  {
    ID        : int
    District  : string
    Code      : string
    Location  : string
    Date      : string
    Time      : string
    Type      : string
    Brand     : string
    Colour    : string
  }
type Trommel =
  {
    ID            : int
    District      : int
    Location      : string
    Date          : int*int*int
    Streetname    : string
    Streetnumber  : int
  }

type Neighbourhood =
  {
    ID        : int
    Name      : string
    Location  : string
    District  : int
  }

type SubNeighbourhood =
  {
    ID            : int
    Name          : string
    Location      : string
    Neighbourhood : int
  }

type District =
  {
    ID          : int
    SafetyIndex : int
    Trommel     : List<int>
    Thefts      : List<int>
  }

type Attribute =
  | ID of int*int
  | DATE of int*int*int
  | TIME of int*int
  | DISTRICT of int
  | AREA of List<string>
  | PLACE of int*string
  | STREET of string
  | DAY of string
  | TIMERANGE of int*int*int*int
  | BIKETYPE of string
  | OBJECTTYPE of string
  | BRAND of string
  | BIKEMODEL of string
  | COLOR of string
  | INVALID