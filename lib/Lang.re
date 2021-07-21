// TODO: add numbers, and successor, so I can more easily observe the behavior of the interpreter without having to try to interpret compiled church-encoded numbers...

[@deriving show]
[@deriving eq]
type lang =
  | Var(int) // we're using de brujn indices
  | Lambda(lang) // λ.a
  | App(lang, list(lang)) // f a0 a1 a2 ...
  | Let(lang, lang) // let a in b
  | Z
  | S(lang);

[@deriving show]
[@deriving eq]
type zinc =
  | Grab
  | Return
  | PushRetAddr(list(zinc))
  | Apply
  | Access(int)
  | Closure(list(zinc))
  | EndLet
  | Succ
  | Num(int);

[@deriving show]
[@deriving eq]
type env_item =
  | ZE(zinc)
  | ClosE(clos)

[@deriving show]
[@deriving eq]
and stack_item =
  | Marker
  | Z(zinc)
  | Clos(clos)

[@deriving show]
[@deriving eq]
and clos = {
  code: list(zinc),
  env: list(env_item),
};
let env_to_stack = x =>
  switch (x) {
  | ZE(z) => Z(z)
  | ClosE({code: c, env: e}) => Clos({code: c, env: e})
  };
let stack_to_env = x =>
  switch (x) {
  | Z(z) => ZE(z)
  | Clos({code: c, env: e}) => ClosE({code: c, env: e})
  | Marker =>
    Printf.eprintf("tried to convert a marker to an environment item");
    assert(false);
  };

let pretty_lang_rev = (l: lang, rev: bool) => {
  let rec pretty_lang = (l: lang) =>
    switch (l) {
    | Var(int) => Printf.sprintf("%d", int)
    | Lambda(l) => Printf.sprintf("(λ.%s)", pretty_lang(l))
    | App(func, args) =>
      String.concat(
        "",
        [
          "(",
          String.concat(
            " ",
            List.map(
              pretty_lang,
              if (rev) {
                List.rev([func, ...args]);
              } else {
                [func, ...args];
              },
            ),
          ),
          ")",
        ],
      )
    | Let(l, i) =>
      Printf.sprintf("let %s in (%s)", pretty_lang(l), pretty_lang(i))
    | Z => "Z"
    | S(l) =>
      if (rev) {
        Printf.sprintf("%s S", pretty_lang(l));
      } else {
        Printf.sprintf("S(%s)", pretty_lang(l));
      }
    };
  pretty_lang(l);
};

let rec pretty_lang = (l: lang) => pretty_lang_rev(l, false);

let rec pretty_zinc = (z: zinc) =>
  switch (z) {
  | Grab => "Grab"
  | Return => "Return"
  | PushRetAddr(l) => Printf.sprintf("PushRetAddr(%s)", pretty_zincs(l))
  | Apply => "Apply"
  | Access(i) => Printf.sprintf("Access(%d)", i)
  | Closure(l) => Printf.sprintf("Closure(%s)", pretty_zincs(l))
  | EndLet => "EndLet"
  | Succ => "Succ"
  | Num(i) => Printf.sprintf("Num(%d)", i)
  }
and pretty_zincs = (z: list(zinc)) =>
  String.concat(" ", List.map(z => pretty_zinc(z), z));

let rec pretty_env_item = e =>
  switch (e) {
  | ZE(z) => pretty_zinc(z)
  | ClosE({code: c, env: e}) =>
    Printf.sprintf("(%s)[%s]", pretty_zincs(c), pretty_env_items(e))
  }
and pretty_env_items = z =>
  String.concat(" ", List.map(z => pretty_env_item(z), z));

let rec pretty_stack_item = (e: stack_item) =>
  switch (e) {
  | Z(z) => pretty_zinc(z)
  | Clos({code: c, env: e}) =>
    Printf.sprintf("(%s)[%s]", pretty_zincs(c), pretty_env_items(e))
  | Marker => "▒"
  }
and pretty_stack_items = (s: list(stack_item)) =>
  String.concat(" ", List.map(item => pretty_stack_item(item), s));
let show_state = state => {
  let (
    instructions: list(zinc),
    env: list(env_item),
    stack: list(stack_item),
  ) = state;
  Printf.sprintf(
    "Instr: %s\nEnv:   %s\nStack  %s",
    pretty_zincs(instructions),
    pretty_env_items(env),
    pretty_stack_items(stack),
  );
};

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
  | Z => [Num(0), ...k]
  | S(n) => other_compile(n, [Succ, ...k])
  };
};

let apply_zinc = state => {
  let (
    instructions: list(zinc),
    env: list(env_item),
    stack: list(stack_item),
  ) = state;
  let stackify = List.map(x => Z(x));
  let env_to_stack_l = List.map(env_to_stack);
  switch (instructions, env, stack) {
  | ([Grab, ...c], env, [Z(v), ...s]) => (c, [ZE(v), ...env], s)
  | ([Grab, ...c], env, [Clos(v), ...s]) => (c, [ClosE(v), ...env], s)
  | ([Grab, ...c], env, [Marker, Z(c'), (Z(_) | Clos(_)) as e', ...s]) => (
      [c'],
      [stack_to_env(e')],
      [Clos({code: [Grab, ...c], env}), ...s],
    )
  | ([Return, ...c], env, [Z(v), Marker, Z(c'), Z(e'), ...s]) => (
      [c'],
      [ZE(e')],
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
      [Marker, ...stackify(c')] @ env_to_stack_l(env) @ s,
    )
  | ([Apply, ...c], env, [Clos({code: c', env: e'}), ...s]) => (c', e', s)
  // Below here is just modern SECD
  | ([Access(n), ...c], env, s) => (
      c,
      env,
      [env_to_stack(List.nth(env, n)), ...s],
    )
  | ([Closure(c'), ...c], env, s) => (
      c,
      env,
      [Clos({code: c', env}), ...s],
    )
  | ([EndLet, ...c], [v, ...env], s) => (c, env, s)
  // math
  | ([Num(n), ...c], env, s) => (c, env, [Z(Num(n)), ...s])
  | ([Succ, ...c], env, [Z(Num(i)), ...s]) => (
      c,
      env,
      [Z(Num(i + 1)), ...s],
    )
  // should be unreachable
  | _ => assert(false)
  };
};
