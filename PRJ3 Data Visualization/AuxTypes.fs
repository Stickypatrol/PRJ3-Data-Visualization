module AuxTypes

type Theft =
  {
    ID        : int
    District  : int
    Code      : string
    Location  : string
    Date      : int*int*int
    Time      : int*int*int
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