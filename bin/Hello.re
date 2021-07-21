Console.log("Running Test Program:");

open Lib.Lang;

let _lc = App(Lambda(Var(0)), [Lambda(Var(0))]);
let one = Lambda(Lambda(App(Var(1), [Var(0)])));
let two = Lambda(Lambda(App(Var(1), [App(Var(1), [Var(0)])])));
let plus =
  Lambda(
    Lambda(
      Lambda(
        Lambda(App(Var(3), [Var(1), App(Var(2), [Var(1), Var(0)])])),
      ),
    ),
  );

let _lc = App(App(plus, [two, one]), [Lambda(S(Var(0))), Z]);
let _lc = App(plus, [two, one]);
let lc = App(Lambda(App(Var(0), [Z])), [Lambda(S(Var(0)))]);
let _lc = App(Lambda(S(Var(0))), [Z]);

let () = print_endline(pretty_lang(lc));
let () = print_endline("↓");

let zinc = tail_compile(lc);
let () = print_endline(pretty_zinc(zinc));
let () = print_endline("↓");

let itered = apply_zinc((zinc, [], []));
let () = print_endline(show_state(itered));
let () = print_endline("↓");

let itered = apply_zinc(itered);
let () = print_endline(show_state(itered));
let () = print_endline("↓");

let itered = apply_zinc(itered);
let () = print_endline(show_state(itered));
let () = print_endline("↓");

let itered = apply_zinc(itered);
let () = print_endline(show_state(itered));
let () = print_endline("↓");

let itered = apply_zinc(itered);
let () = print_endline(show_state(itered));
let () = print_endline("↓");

let itered = apply_zinc(itered);
let () = print_endline(show_state(itered));
let () = print_endline("↓");

let itered = apply_zinc(itered);
let () = print_endline(show_state(itered));
let () = print_endline("↓");

let () = print_endline(Lib.Util.hello());
