/*
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
   type lcF('a) =
     [ | `Var(int) | `Lambda('a) | `App('a, list('a)) | `Let('a, 'a)];

   [@deriving show]
   [@deriving eq]
   type zincF('a) = [>
     | `Grab
     | `Return
     | `PushRetAddr(list('a))
     | `Apply
     | `Access(int)
     | `Closure(list('a))
     | `EndLet
   ] as 'a;

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
   let rec fix = f => v => f(fix(f), v);

   module rec M: {
     module type X = {
       type f('f, 'v) constraint 'f = (module M.X);
       type v;
     }
   } = M;

   type t('a) = [> `X] as 'a
   type closed = t(closed);
   type code = {
     x: 'a. zincF('a)
   };
   module Execute = {

     type env_item('a) = 'a constraint 'a = zincF('a) constraint 'a = clos()
     and clos('a, 'b) = {
       code: list(zincF('a)),
       env: list(env_item('b)),
     };
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

     /*
          type whatever('a, 'b, 'c, 'd) =
            ((zincF('a), list('b), list('c), list('d))) =>
            (list('b), list('c), list('d))
          constraint 'b = [> zincF('a)]
          constraint 'c = [> env_item('a)]
          constraint 'c = 'b
          constraint 'd = [> stack_item('a)]
          constraint 'd = 'b;
      */

     let apply_zinc = state => {
       let env_to_stack = (a: env_item('a)): stack_item('a) => {
         switch (a) {
         | #env_item as s => s
         };
       };

       let (instruction, c, env, stack: list([> stack_item('a)])) = state;
       switch (instruction, env, stack) {
       | (`Grab, env, [`Clos(v), ...s]) => (c, [`Clos(v), ...env], s)
       | (`Grab, env, [`Marker(c', e'), ...s]) => (
           c',
           e',
           [`Clos({code: [`Grab, ...c], env}), ...s],
         )
       | (`Grab, env, [#zincF as v, ...s]) => (c, [v, ...env], s)
       | (`Return, env, [v, `Marker(c', e'), ...s]) => (c', e', [v, ...s])
       | (`Return, env, [`Clos({code: c', env: e'}), ...s]) => (c', e', s)
       | (`PushRetAddr(c'), env, s) => (c, env, [`Marker((c', env)), ...s])
       | (`Apply, env, [`Clos({code: c', env: e'}), ...s]) => (c', e', s)
       // Below here is just modern SECD
       | (`Access(n), env, s) =>
         let nth = List.nth(env, n);
         (c, env, [env_to_stack(nth), ...s]);
       | (`Closure(c'), env, s) => (c, env, [`Clos({code: c', env}), ...s])
       | (`EndLet, [v, ...env], s) => (c, env, s)
       /*
        // math
        | (`Num(n), env, s) => (c, env, [`Num(n), ...s])
        | (`Succ, env, [`Num(i), ...s]) => (c, env, [`Num(i + 1), ...s])
        */
       // should be unreachable
       | _ => assert(false)
       };
     };
   };

   module Show = {
     let pretty_lang = (l: lcF('a), f: ('a, ~rev: bool) => string, ~rev: bool) => {
       switch (l) {
       | `Var(int) => Printf.sprintf("%d", int)
       | `Lambda(l) => Printf.sprintf("(λ.%s)", f(l, ~rev))
       | `App(func, args) =>
         String.concat(
           "",
           [
             "(",
             String.concat(
               " ",
               List.map(
                 f(~rev),
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
       | `Let(l, i) =>
         Printf.sprintf("let %s in (%s)", f(l, ~rev), f(i, ~rev))
       };
     };

     let rec pretty_zinc = (z, fs) =>
       switch (z) {
       | `Grab => "Grab"
       | `Return => "Return"
       | `PushRetAddr(l) => Printf.sprintf("PushRetAddr(%s)", fs(l))
       | `Apply => "Apply"
       | `Access(i) => Printf.sprintf("Access(%d)", i)
       | `Closure(l) => Printf.sprintf("Closure(%s)", fs(l))
       | `EndLet => "EndLet"
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

   module Show = {
     let pretty_lang = (l: lcF('a), f: ('a, ~rev: bool) => string, ~rev: bool) => {
       switch (l) {
       | `Z => "Z"
       | `S(l) =>
         if (rev) {
           Printf.sprintf("%s S", f(l, ~rev));
         } else {
           Printf.sprintf("S(%s)", f(l, ~rev));
         }
       };
     };

     let rec pretty_zinc = (z, fs) =>
       switch (z) {
       | `Succ => "Succ"
       | `Num(i) => Printf.sprintf("Num(%d)", i)
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
     /*
      let apply_zinc = state => {
        let (instruction, c, env, stack) = state;
        switch (instruction) {
        | #Core.lcF as instruction =>
          Core.Execute.apply_zinc((instruction, c, env, stack))
        | #Extension.lcF as instruction =>
          Extension.Execute.apply_zinc((instruction, c, env, stack))
        };
      };
      */
   };

   module Show = {
     let pretty_lang = (l: lcF('a), f: ('a, ~rev: bool) => string, ~rev: bool) => {
       switch (l) {
       | #Core.lcF as l => Core.Show.pretty_lang(l, f, ~rev)
       | #Extension.lcF as l => Extension.Show.pretty_lang(l, f, ~rev)
       };
     };

     let pretty_zinc = (l, fs) => {
       switch (l) {
       | #Core.zincF as z => Core.Show.pretty_zinc(z, fs)
       | #Extension.zincF as z => Extension.Show.pretty_zinc(z, fs)
       };
     };
   };
 };

 let compile = l => {
   let rec t = l => Combined.Compile.tail(l, t, c)
   and c = (l, k) => Combined.Compile.other(l, k, t, c);
   Combined.Compile.tail(l, t, c);
 };
 module Execute = {
   type env_item('a) = [ Combined.zincF('a) | `Clos(clos('a))]
   and stack_item('a) = [
     Combined.zincF('a)
     | `Clos(clos('a))
     | `Marker(list(Combined.zincF('a)), list(env_item('a)))
   ]
   and clos('a) = {
     code: list(Combined.zincF('a)),
     env: list(env_item('a)),
   };

   let apply_zinc = state => {
     let (instruction, c, env, stack) = state;
     switch (instruction, env, stack) {
     | (`Grab, env, [`Clos(v), ...s]) => (c, [`Clos(v), ...env], s)
     | (`Grab, env, [`Marker(c', e'), ...s]) => (
         c',
         e',
         [`Clos({code: [`Grab, ...c], env}), ...s],
       )
     | (`Grab, env, [#Combined.zincF as v, ...s]) => (c, [v, ...env], s)
     | (`Return, env, [v, `Marker(c', e'), ...s]) => (c', e', [v, ...s])
     | (`Return, env, [`Clos({code: c', env: e'}), ...s]) => (c', e', s)
     | (`PushRetAddr(c'), env, s) => (c, env, [`Marker((c', env)), ...s])
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
     | (`Closure(c'), env, s) => (c, env, [`Clos({code: c', env}), ...s])
     | (`EndLet, [v, ...env], s) => (c, env, s)

     // math
     | (`Num(n), env, s) => (c, env, [`Num(n), ...s])
     | (`Succ, env, [`Num(i), ...s]) => (c, env, [`Num(i + 1), ...s])

     // should be unreachable
     | _ => assert(false)
     };
   };
 };
 module Show = {
   let rec pretty_lang = (l, ~rev): string => {
     let rec f = (l, ~rev) => pretty_lang(l, ~rev);
     Combined.Show.pretty_lang(l, f, ~rev);
   };

   let rec pretty_zinc = l =>
     switch (l) {
     | #Combined.zincF as z => Combined.Show.pretty_zinc(z, pretty_zincs)
     }
   and pretty_zincs = ls =>
     String.concat(" ", List.map(z => pretty_zinc(z), ls));

   let rec pretty_env_item = e =>
     switch (e) {
     | #Combined.zincF as z => pretty_zinc(z)
     | `Clos(Execute.{code: c, env: e}) =>
       Printf.sprintf("(%s)[%s]", pretty_zincs(c), pretty_env_items(e))
     }
   and pretty_env_items = z =>
     String.concat(" ", List.map(z => pretty_env_item(z), z));

   let rec pretty_stack_item = e =>
     switch (e) {
     | #Combined.zincF as z => pretty_zinc(z)
     | `Clos(Execute.{code: c, env: e}) =>
       Printf.sprintf("(%s)[%s]", pretty_zincs(c), pretty_env_items(e))
     | `Marker(c, e) =>
       Printf.sprintf("▒(%s, %s)", pretty_zincs(c), pretty_env_items(e))
     }
   and pretty_stack_items = s =>
     String.concat(" ", List.map(item => pretty_stack_item(item), s));
   let show_state = state => {
     let (instructions, env, stack) = state;
     Printf.sprintf(
       "Instr: %s\nEnv:   %s\nStack  %s",
       pretty_zincs(instructions),
       pretty_env_items(env),
       pretty_stack_items(stack),
     );
   };
 };
 */
