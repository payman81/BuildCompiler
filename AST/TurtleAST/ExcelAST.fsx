type value =
    | Number of float
    | String of string
    | Bool of bool
    | DateTime of System.DateTime

type rowIndex = RowIndex of int
type colIndex = ColIndex of int
type address = Address of colIndex * rowIndex

type single =
    | Absolute of address
    | Relative of rowIndex * colIndex

type reference =
    | Single of single
    | Range of single * single

type operator =
    | Plus | Minus | Multiply | Divide | Power
    | Equals // Not Equals | ..

type expr =
    | Value of value
    | Reference of reference
    | Operator of operator * expr * expr
    | Function of string * expr list
