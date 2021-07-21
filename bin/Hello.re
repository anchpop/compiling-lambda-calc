Console.log("Running Test Program:");

let _lc = Lib.Lang.(App(Lambda(Var(0)), [Lambda(Var(0))]));
let one = Lib.Lang.(Lambda(Lambda(App(Var(1), [Var(0)]))));
let two =
  Lib.Lang.(Lambda(Lambda(App(Var(1), [App(Var(1), [Var(0)])]))));
let plus =
  Lib.Lang.(
    Lambda(
      Lambda(
        Lambda(
          Lambda(
            App(Var(3), [Var(1), App(Var(2), [Var(1), Var(0)])]),
          ),
        ),
      ),
    )
  );

let lc = Lib.Lang.(App(plus, [two, one]));

let () = print_endline(Lib.Lang.pretty_lang(lc));
let () = print_endline("↓");

let zinc = Lib.Lang.tail_compile(lc);
let () = print_endline(Lib.Lang.pretty_zinc(zinc));
let () = print_endline("↓");

let itered = Lib.Lang.apply_zinc((zinc, [], []));
let () = print_endline(Lib.Lang.print_state(itered));
let () = print_endline("↓");

let itered = Lib.Lang.apply_zinc(itered);
let () = print_endline(Lib.Lang.print_state(itered));
let () = print_endline("↓");

let itered = Lib.Lang.apply_zinc(itered);
let () = print_endline(Lib.Lang.print_state(itered));
let () = print_endline("↓");

let () = print_endline(Lib.Util.hello());
