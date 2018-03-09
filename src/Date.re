module Native = {
    type date;
    [@bs.val] external now: unit => float = "Date.now";
    [@bs.new] external fromUnixTime : float => date = "Date";
    [@bs.new] external getTime : date => float = "Date";
    [@bs.new] external make : array(int) => date = "Date";
    [@bs.send] external year : date => int = "getUTCFullYear";
    [@bs.send] external month : date => int = "getUTCMonth";
    [@bs.send] external day : date => int = "getUTCDate";
    [@bs.send] external hours : date => int = "getUTCHours";
    [@bs.send] external minutes : date => int = "getUTCMinutes";
    [@bs.send] external seconds : date => int = "getUTCSeconds";
    [@bs.send] external millies : date => int = "getUTCMilliseconds";
};

exception InvalidDate;
let _maxYear = 275759;
let _minYear = -271820;

type timeSteps =
  | Year
  | Month
  | Day
  | Hour
  | Minute
  | Second
  | Millisecond;

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

let daysInMonth = (~leapYear=false, month) => switch month {
    | Jan => 31
    | Feb => leapYear ? 29 : 28
    | Mar => 31
    | Apr => 30
    | May => 31
    | Jun => 30
    | Jul => 31
    | Aug => 31
    | Sep => 30
    | Oct => 31
    | Nov => 30
    | Dec => 31
}; 
let intFromMonth = fun
  | Jan => 0
  | Feb => 1
  | Mar => 2
  | Apr => 3
  | May => 4
  | Jun => 5
  | Jul => 6
  | Aug => 7
  | Sep => 8
  | Oct => 9
  | Nov => 10
  | Dec => 11;

let monthFromInt = m => switch (m mod 12) {
    | 0 => Jan
    | 1 => Feb
    | 2 => Mar
    | 3 => Apr
    | 4 => May
    | 5 => Jun
    | 6 => Jul
    | 7 => Aug
    | 8 => Sep
    | 9 => Oct
    | 10 => Nov
    | 11 => Dec
    | _ => Jan
};


type t = {
    year:int,
    month:month,
    day:int,
    hours: int,
    minutes: int,
    seconds: int,
    milliseconds: int
};
let isLeapYear = d => 
    d.year mod 4 == 0 
    && (d.year mod 100) == 0 
    && d.year mod 400 == 0;

let _isValid = d =>     
    d.year > _minYear
    && d.year < _maxYear
    && d.day <= daysInMonth(~leapYear=isLeapYear(d), d.month);

let make = (~year=1970, ~month=Jan, ~day=1, ~hours=0, ~minutes=0, ~seconds=0, ~milliseconds=0, ()) => {
    let d = {
        year,
        month,
        day,
        hours, 
        minutes,
        seconds,
        milliseconds,
    };
    _isValid(d) ? Some(d): None;
};

let _toJs = d => Native.make([|d.year, intFromMonth(d.month) , d.day, d.hours, d.minutes, d.seconds, d.milliseconds|]);

let _fromJs = jsDate => {
    let month = jsDate |> Native.month |> monthFromInt;

    make(
        ~year=Native.year(jsDate),
        ~month=month,
        ~day=Native.day(jsDate),
        ~hours=Native.hours(jsDate),
        ~minutes=Native.minutes(jsDate),
        ~seconds=Native.seconds(jsDate),
        ~milliseconds=Native.millies(jsDate),
        ()
    );
};

let fromUnixTime = unixTime => unixTime *. 1000. 
    |> Native.fromUnixTime 
    |>  _fromJs 
    |> fun 
        | Some(d) => d
        | None => raise(InvalidDate); 
let toUnixTime = date => date |> _toJs |> Native.getTime |> t => t /. 1000.;
let now = () => 
    Native.now() 
    |> Native.fromUnixTime 
    |> _fromJs  
    |> fun 
        | Some(d) => d
        | None => raise(InvalidDate);

let year = d => d.year;
let month = d => d.month;
let day = d => d.day;
let hours = d => d.hours;
let minutes = d => d.minutes;
let seconds = d => d.seconds;
let milliseconds = d => d.milliseconds;

let addYear = (i, d) => {...d, year: d.year + i};
let subYear = (i, d) => {...d, year: d.year - i};
let addMonth =  (i, d) => {
    let newMonth = intFromMonth(d.month) + i;
    {
        ...d,
        year: d.year + newMonth / 12,
        month: monthFromInt(newMonth)
    }
};

let toJson = d => {
    let {year, day, hours, minutes, seconds, milliseconds} = d;
    let month = intFromMonth(d.month) + 1;
    {j|$(year)-$(month)-$(day)T$(hours):$(minutes):$(seconds).$(milliseconds)|j}
};



