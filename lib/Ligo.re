let ast_typed_string = {|
type storage = int
type parameter = sum[Decrement -> int , Increment -> int , Reset -> unit]
type return = ( list (operation) * int )
const add = lambda (#188) return let #195 = #188 in  match #195 with
                                                      | ( store , delta ) ->
                                                      ADD(store , delta)
const sub = lambda (#189) return let #197 = #189 in  match #197 with
                                                      | ( store , delta ) ->
                                                      SUB(store , delta)
const main = lambda (#190) return let #199 = #190 in  match #199 with
                                                       | ( action , store ) ->
                                                       ( LIST_EMPTY() , let #201 = action in  match 
                                                                    #201 with
                                                                    | Decrement n ->
                                                                    (sub)@(
                                                                    ( store , n ))
                                                                    | Increment n ->
                                                                    (add)@(
                                                                    ( store , n ))
                                                                    | Reset unit_proj#202 ->
                                                                    0 )
|}