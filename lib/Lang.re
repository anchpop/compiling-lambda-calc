// TODO: add numbers, and successor, so I can more easily observe the behavior of the interpreter without having to try to interpret compiled church-encoded numbers...

[@deriving show]
type lang =
  | Var(int) // we're using de brujn indices
  | Lambda(lang) // λ.a
  | App(lang, list(lang)) // f a0 a1 a2 ...
  | Let(lang, lang); // let a in b

[@deriving show]
type zinc =
  | Grab
  | Return
  | PushRetAddr(list(zinc))
  | Apply
  | Access(int)
  | Closure(list(zinc))
  | EndLet;

[@deriving show]
type stack_item =
  | Marker
  | Z(zinc)
  | Clos({
      code: list(zinc),
      env: list(zinc),
    });

let rec pretty_lang = (l: lang) =>
  switch (l) {
  | Var(int) => Printf.sprintf("%d", int)
  | Lambda(l) => Printf.sprintf("(λ.%s)", pretty_lang(l))
  | App(func, args) =>
    String.concat(" ", List.map(pretty_lang, [func, ...args]))
  | Let(l, i) =>
    Printf.sprintf("let %s in (%s)", pretty_lang(l), pretty_lang(i))
  };

let rec pretty_zinc = (z: list(zinc)) =>
  String.concat(" ", List.map(z => show_zinc(z), z));

let print_state = [%derive.show:
  (list(zinc), list(zinc), list(stack_item))
];

let rec tail_compile = (l: lang) => {
  switch (l) {
  | Lambda(a) => [Grab, ...tail_compile(a)]
  | Let(a, b) => other_compile(a, [Grab, ...tail_compile(b)])
  | App(func, args) =>
    let rec comp = l =>
      switch (l) {
      | [] => tail_compile(func)
      | [arg, ...args] => other_compile(arg, comp(args))
      };
    comp(List.rev(args));
  | a => other_compile(a, [Return])
  };
}
and other_compile = (l: lang, k: list(zinc)): list(zinc) => {
  switch (l) {
  | Var(n) => [Access(n), ...k]
  | Lambda(a) => [Closure([Grab, ...tail_compile(a)]), ...k]
  | Let(a, b) => other_compile(a, [Grab, ...other_compile(b, [EndLet])])
  | App(func, args) =>
    let rec comp = l =>
      switch (l) {
      | [] => other_compile(func, [Apply])
      | [arg, ...args] => other_compile(arg, comp(args))
      };
    [PushRetAddr(k), ...comp(List.rev(args))];
  };
};

let apply_zinc = state => {
  let (instructions: list(zinc), env: list(zinc), stack: list(stack_item)) = state;
  let stackify = List.map(x => Z(x));
  switch (instructions, env, stack) {
  | ([Grab, ...c], env, [Z(v), ...s]) => (c, [v, ...env], s)
  | ([Grab, ...c], env, [Marker, Z(c'), Z(e'), ...s]) => (
      [c'],
      [e'],
      [Clos({code: [Grab, ...c], env}), ...s],
    )
  | ([Return, ...c], env, [Z(v), Marker, Z(c'), Z(e'), ...s]) => (
      [c'],
      [e'],
      [Z(v), ...s],
    )
  | ([Return, ...c], env, [Clos({code: c', env: e'}), ...s]) => (
      c',
      e',
      s,
    )
  | ([PushRetAddr(c'), ...c], env, s) => (
      c,
      env,
      [Marker, ...stackify(c')] @ stackify(env) @ s,
    )
  | ([Apply, ...c], env, [Clos({code: c', env: e'}), ...s]) => (c', e', s)
  // Below here is just modern SECD
  | ([Access(n), ...c], env, s) => (
      c,
      env,
      [Z(List.nth(env, n)), ...stackify(c)],
    )
  | ([Closure(c'), ...c], env, s) => (
      c,
      env,
      [Clos({code: c', env}), ...s],
    )
  | ([EndLet, ...c], [v, ...env], s) => (c, env, s)
  // should be unreachable
  | _ => assert(false)
  };
};
