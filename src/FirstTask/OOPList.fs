module OOPList

type IList<'value> = interface end

//[<AllowNullLiteral>]
type NonEmptyList<'value> (head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'value>() =
    interface IList<'value>

type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType

let rec map (f:IActor<'value,'result>) (lst:IList<'value>) =
    if lst :? EmptyList<'value>
    then EmptyList() :> IList<'result>
    elif lst :? NonEmptyList<'value>
    then
        let lst = lst :?> NonEmptyList<'value>
        NonEmptyList(f.Do lst.Head, map f lst.Tail)
    else failwith "!!!"
