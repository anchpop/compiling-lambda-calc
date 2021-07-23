Console.log("Running Test Program:");

open Lib.Lang;

let _lc = App(Lambda(Var(0)), [Lambda(Var(0))]);
let one = Lambda(Lambda(App(Var(1), [Var(0)])));

let two = Lambda(Lambda(App(Var(1), [App(Var(1), [Var(0)])])));
let succlc =
  Lambda(
    Lambda(Lambda(App(Var(1), [App(Var(2), [Var(1), Var(0)])]))),
  );
let plus = Lambda(Lambda(App(Var(0), [succlc, Var(1)])));

let () = Printf.printf("one: %s \n", pretty_lang(one));
let () = Printf.printf("two: %s \n", pretty_lang(two));
let () = Printf.printf("succ: %s \n", pretty_lang(succlc));
let () = Printf.printf("plus: %s \n\n", pretty_lang(plus));
let lc = App(App(plus, [two, one]), [Lambda(S(Var(0))), Z]);
let _lc = App(plus, [two, one]);
let _lc = App(Lambda(App(Var(0), [Z])), [Lambda(S(Var(0)))]);
let _lc = App(Lambda(S(Var(0))), [Z]);

let _lc = App(two, [Lambda(S(Var(0))), Z]);

let () = print_endline(pretty_lang_rev(lc, true));
let () = print_endline("↓");

let initial_state = (tail_compile(lc), [], []);
let () = print_endline(show_state(initial_state));
let () = print_endline("↓");

let rec range = (a, b) =>
  if (a > b) {
    [];
  } else {
    [a, ...range(a + 1, b)];
  };

let _ =
  List.fold_left(
    (itered, n) => {
      let () = Printf.printf("%d:\n", n);
      let itered = apply_zinc(itered);
      let () = print_endline(show_state(itered));
      let () = print_endline("↓");
      itered;
    },
    initial_state,
    range(0, 50),
  );
