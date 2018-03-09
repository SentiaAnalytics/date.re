

let d = Date.now();

Js.log @@ Date.toJson(d);

let d2 = Date.addMonth(10, d);

Js.log @@ Date.toJson(d2);
