module FirstTask

type IGraphVertex =
    abstract OutgoingEdges : seq<IGraphVertex> with get

type IGraphForwardVertex =
    //inherit IGraphVertex
    abstract OutgoingEdges : seq<IGraphForwardVertex> with get

type IGraphBackwardVertex =
    //inherit IGraphVertex
    abstract OutgoingEdges : seq<IGraphBackwardVertex> with get


type Vertex (i: int) =
    let outgoingEdges = ResizeArray<Vertex>()
    let incomingEdges = ResizeArray<Vertex>()
    interface IGraphBackwardVertex with
        member this.OutgoingEdges with get () = incomingEdges |> Seq.cast<IGraphBackwardVertex>
      //  member this.OutgoingEdges with get () = incomingEdges |> Seq.cast<IGraphVertex>

    member this.IncomingEdges with get() = incomingEdges
    member this.OutgoingEdges with get() = outgoingEdges

    override this.ToString () = string i

    interface IGraphForwardVertex with
        member this.OutgoingEdges with get () = outgoingEdges |> Seq.cast<IGraphForwardVertex>
        //member this.OutgoingEdges with get () = incomingEdges |> Seq.cast<IGraphVertex>

    interface IGraphVertex with
        member this.OutgoingEdges with get () =
            let edges = ResizeArray(outgoingEdges)
            edges.AddRange incomingEdges
            edges |> Seq.cast<IGraphVertex>



let inline f (v:'t when 't:(member OutgoingEdges: seq<'t>)) =
    (^t :(member OutgoingEdges : seq<'t>)v)
    //v.OutgoingEdges
    |> Seq.iter (fun v -> printf $"%A{v}; ")
    printfn "==="


(*
let f (v:IGraphVertex) =
    v.OutgoingEdges
    |> Seq.iter (fun v -> printf $"%A{v}; ")
    printfn "==="
*)

let go () =
    let v1 = Vertex(1)
    let v2 = Vertex(2)
    let v3 = Vertex(3)

    v1.OutgoingEdges.Add v2
    v2.IncomingEdges.Add v1
    v2.OutgoingEdges.Add v3
    v3.IncomingEdges.Add v2

    f (v2:>IGraphForwardVertex)
    f (v2:>IGraphBackwardVertex)
    f (v2:>IGraphVertex)

let rec sum x acc =
    if x = 0
    then acc
    else sum (x - 1) (acc + x)

[<EntryPoint>]
let main (argv: string array) =
    (*
    let x = 1
    let f y =
        let x = "ads"
        let f y = y + 1
        x.Length + (f y)
    let r = f x
    printfn $"res: %A{sum 10 0}"
    *)
    let mutable x = 0

    let x = if true then 0 else 8

    //go()
    //List.go()
    List._go2()
    |> printfn "%A"
    0
