type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec;


type t;

let make : (~year:int=?, ~month:month=?, ~day:int=?, ~hours:int=?, ~minutes:int=?, ~seconds:int=?, ~milliseconds:int=?, unit) => option(t);

let fromUnixTime : float => t;
let toUnixTime : t => float;

let now : unit => t;

let year : t => int;
let month : t => month;
let day : t => int;
let hours : t => int;
let minutes : t => int;
let seconds : t => int;
let milliseconds : t => int;

let addMonth : int=> t => t;


let toJson : t => string;