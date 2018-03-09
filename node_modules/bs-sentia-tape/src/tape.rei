type t;

type testTool = {
  plan: int => unit,
  ok: message::string? => bool => unit
};

let test : string => (testTool => unit) => unit;