type lang =
  | Var(int) // we're using de brujn indices
  | Lambda(lang) // λ.a
  | Apply(lang, list(lang)) // f a0 a1 a2 ...
  | Let(lang, lang); // let a in b

type zinc =
  | Grab
  | Return
  | PushRetAddr(list(zinc))
  | Apply
  | Access(int)
  | Closure(list(zinc))
  | EndLet
and environment = list(zinc);

let rec tail_compile = (l: lang) => {
  switch (l) {
  | Lambda(a) => [Grab, ...tail_compile(a)]
  | Let(a, b) => {
    other_compile(a, [Grab, ...tail_compile(b)])
  }
  | Apply(func, args) =>
    let rec comp = (l) => switch (l) {
      | [] => tail_compile(func)
      | [arg, ...args] => other_compile(arg,(comp(args)))
    };
    comp(List.rev(args))
  | a => other_compile(a, [Return])
  };
}
and other_compile = (l: lang, k: list(zinc)): list(zinc) => {
  switch (l) {
  | Var(n) => [Access(n), ...k]
  | Lambda(a) => [Closure([Grab, ...tail_compile(a)]), ...k]
  | Let(a, b) =>
    other_compile(a, [Grab, ...other_compile(b, [EndLet])])
  | Apply(func, args) => 
    let rec comp = (l) => switch (l) {
      | [] => other_compile(func, [Apply])
      | [arg, ...args] => other_compile(arg, (comp(args)))
    };
    [PushRetAddr(k), ...comp(List.rev (args))]
  };
};
