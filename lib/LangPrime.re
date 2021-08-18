module type Language = {
  type lcF('a);

  type zincF('a);

  module Compile: {
    let tail:
      (
        lcF('a),
        'a => list(zincF('b)),
        ('a, list(zincF('b))) => list(zincF('b))
      ) =>
      list(zincF('b));

    let other:
      (
        [< | `App('a, list('a)) | `Lambda('b) | `Let('a, 'a) | `Var('c)],
        list(
          [>
            | `Access('c)
            | `Apply
            | `Closure(list([> | `Grab] as 'f))
            | `EndLet
            | `Grab
            | `PushRetAddr('d)
          ] as 'e,
        ) as 'd,
        'b => list('f),
        ('a, list('e)) => list('e)
      ) =>
      list('e);
  };
};

module Core = {
  [@deriving show]
  [@deriving eq]
  type lcF('a) = [
    | `Var(int)
    | `Lambda('a)
    | `App('a, list('a))
    | `Let('a, 'a)
  ];

  [@deriving show]
  [@deriving eq]
  type zincF('a) = [
    | `Grab
    | `Return
    | `PushRetAddr(list('a))
    | `Apply
    | `Access(int)
    | `Closure(list('a))
    | `EndLet
  ];

  module Compile = {
    let other = (l, k, t, c) => {
      switch (l) {
      | `Var(n) => [`Access(n), ...k]
      | `Lambda(a) => [`Closure([`Grab, ...t(a)]), ...k]
      | `Let(a, b) => c(a, [`Grab, ...c(b, [`EndLet])])
      | `App(func, args) =>
        let rec comp = l =>
          switch (l) {
          | [] => c(func, [`Apply])
          | [arg, ...args] => c(arg, comp(args))
          };
        [`PushRetAddr(k), ...comp(List.rev(args))];
      };
    };

    let tail = (l, t, c) => {
      switch (l) {
      | `Lambda(a) => [`Grab, ...t(a)]
      | `Let(a, b) => c(a, [`Grab, ...t(b)])
      | `App(func, args) =>
        let rec comp = l =>
          switch (l) {
          | [] => t(func)
          | [arg, ...args] => c(arg, comp(args))
          };
        comp(List.rev(args));
      | a => other(a, [`Return], t, c)
      };
    };
  };

  module Execute = {
    type env_item('a) = [ zincF('a) | `Clos(clos('a))]
    and stack_item('a) = [
      zincF('a)
      | `Clos(clos('a))
      | `Marker(list(zincF('a)), list(env_item('a)))
    ]
    and clos('a) = {
      code: list(zincF('a)),
      env: list(env_item('a)),
    };

    let apply_zinc:
      (
        (
          [> zincF('a)] as 'b,
          list([> zincF('a)]),
          list([> env_item('a)]),
          list([> stack_item('a)]),
        )
      ) =>
      (
        list([> zincF('a)]),
        list([> env_item('a)]),
        list([> stack_item('a)]),
      ) =
      state => {
        let (instruction, c, env, stack: list([> stack_item('a)])) = state;
        switch (instruction, env, stack) {
        | (`Grab, env, [`Clos(v), ...s]) => (c, [`Clos(v), ...env], s)
        | (`Grab, env, [`Marker(c', e'), ...s]) => (
            c',
            e',
            [`Clos({code: [`Grab, ...c], env}), ...s],
          )
        | (`Grab, env, [#zincF as v, ...s]) => (c, [v, ...env], s)
        | (`Return, env, [v, `Marker(c', e'), ...s]) => (
            c',
            e',
            [v, ...s],
          )
        | (`Return, env, [`Clos({code: c', env: e'}), ...s]) => (c', e', s)
        /*
         | (`PushRetAddr(c'), env, s) => (
             c,
             env,
             [`Marker((c', env)), ...s],
           )
          | (`Apply, env, [`Clos({code: c', env: e'}), ...s]) => (c', e', s)
          // Below here is just modern SECD
          | (`Access(n), env, s) =>
            let nth = List.nth(env, n);
            let env_to_stack = (a: env_item('a)): stack_item('a) => {
              switch (a) {
              | #env_item as s => s
              };
            };
            (c, env, [env_to_stack(nth), ...s]);
          | (`Closure(c'), env, s) => (
              c,
              env,
              [`Clos({code: c', env}), ...s],
            )
          | (`EndLet, [v, ...env], s) => (c, env, s)
           // math
           | (`Num(n), env, s) => (c, env, [`Num(n), ...s])
           | (`Succ, env, [`Num(i), ...s]) => (c, env, [`Num(i + 1), ...s])
           */
        // should be unreachable
        | _ => assert(false)
        };
      };
  };
};

module Extension = {
  [@deriving show]
  [@deriving eq]
  type lcF('a) = [ | `Z | `S('a)];

  [@deriving show]
  [@deriving eq]
  type lc = lcF(lc);

  [@deriving show]
  [@deriving eq]
  type zincF('a) = [ | `Succ | `Num(int)];

  [@deriving show]
  [@deriving eq]
  type zinc = zincF(zinc);

  module Compile = {
    let other = (l, k, _, c) => {
      switch (l) {
      | `Z => [`Num(0), ...k]
      | `S(n) => c(n, [`Succ, ...k])
      };
    };

    let tail = (l, t, c) => {
      other(l, [`Return], t, c);
    };
  };

  module Execute = {
    let apply_zinc:
      (([> | `Num(int) | `Succ], 'a, 'b, list([> | `Num(int)] as 'c))) =>
      ('a, 'b, list('c)) =
      state => {
        let (instruction, c, env, stack) = state;
        switch (instruction, env, stack) {
        // math
        | (`Num(n), env, s) => (c, env, [`Num(n), ...s])
        | (`Succ, env, [`Num(i), ...s]) => (c, env, [`Num(i + 1), ...s])

        // should be unreachable
        | _ => assert(false)
        };
      };
  };
};

module Combined = {
  [@deriving show]
  [@deriving eq]
  type lcF('a) = [ Core.lcF('a) | Extension.lcF('a)];

  [@deriving show]
  [@deriving eq]
  type lc = lcF(lc);

  [@deriving show]
  [@deriving eq]
  type zincF('a) = [ Core.zincF('a) | Extension.zincF('a)];

  [@deriving show]
  [@deriving eq]
  type zinc = zincF(zinc);

  module Compile = {
    let tail = l => {
      switch (l) {
      | #Core.lcF as lc => Core.Compile.tail(lc)
      | #Extension.lcF as lc => Extension.Compile.tail(lc)
      };
    };

    let other = l => {
      switch (l) {
      | #Core.lcF as lc => Core.Compile.other(lc)
      | #Extension.lcF as lc => Extension.Compile.other(lc)
      };
    };
  };

  module Execute = {
    type env_item('a) = [ zincF('a) | `Clos(clos('a))]
    and stack_item('a) = [
      zincF('a)
      | `Clos(clos('a))
      | `Marker(list(zincF('a)), list(env_item('a)))
    ]
    and clos('a) = {
      code: list(zincF('a)),
      env: list(env_item('a)),
    };

    let apply_zinc = state => {
      let instruction = state;
      let (
        instruction,
        c,
        env,
        stack:
          list(
            Core.Execute.stack_item(
              [ Core.zincF('a) | Extension.zincF('a)] as 'a,
            ),
          ),
      ) = state;
      switch (instruction) {
      | #Core.zincF as instruction =>
        Core.Execute.apply_zinc((instruction, c, env, stack))
      | #Extension.zincF as instruction => assert(false) //Extension.Execute.apply_zinc((instruction, c, env, stack))
      };
    };
  };
};

let compile = l => {
  let rec t = l => Combined.Compile.tail(l, t, c)
  and c = (l, k) => Combined.Compile.other(l, k, t, c);
  Combined.Compile.tail(l, t, c);
};
