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
};

let compile = l => {
  let rec t = l => Combined.Compile.tail(l, t, c)
  and c = (l, k) => Combined.Compile.other(l, k, t, c);
  Combined.Compile.tail(l, t, c);
};
