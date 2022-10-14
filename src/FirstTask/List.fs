module List

type List<'value> =
    | Cons of head: 'value * tail: List<'value>
    | Empty

let rec map f lst =
    match lst with
    | Empty -> Empty
    | Cons (hd, tl) -> Cons (f hd, map f tl)
(*
let go () =
    map ((+)1) (Cons (1,Cons(3,Empty)))

let _go () =
    map ((-)1) (Cons (1,Cons(3,Empty)))

*)
(*
let rec oopMap2 (f:IActor<'value,'result>) (lst:IList<'value>) =
    match lst with
    | :? MyOOPEmptyList<'value> ->
        MyOOPEmptyList () :> IList<'result>
    | :? MyOOPNonEmptyList<'value> as lst ->
        MyOOPNonEmptyList(f.Do lst.Head, oopMap f lst.Tail)


type PlusOneActor () =
    interface IActor<int,int> with
        member this.Do x = x + 1

type MinusOneActor () =
    interface IActor<int,int> with
        member this.Do x = x - 1

///Some useful comment
let _go2() =
    let lst = MyOOPNonEmptyList(1,MyOOPNonEmptyList(3,MyOOPEmptyList()))
    oopMap (PlusOneActor()) lst

let go2() =
    let lst = MyOOPNonEmptyList(1,MyOOPNonEmptyList(3,MyOOPEmptyList()))
    oopMap (MinusOneActor()) lst

let rec fromMyListToMyOOPList (lst:MyList<'value>) =
    match lst with
    | Empty -> MyOOPEmptyList<'value>() :> IList<_>
    | Cons (hd,tl) -> MyOOPNonEmptyList(hd, fromMyListToMyOOPList tl)
*)
