module FirstTask

open System.Buffers
open OOPList

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

    let tree = Tree.generateRandomFullTreeOfHeight (System.Random()) 25 //26 // 27

    let n = 10

    for i in 0..100 do Tree.minInTree tree |> ignore

    let start1 = System.DateTime.Now
    for i in 0..n-1 do
        let start = System.DateTime.Now
        Tree.minInTree tree |> ignore
        printfn "time: %A" (System.DateTime.Now - start)
    let finish1 = System.DateTime.Now

    let start2 = System.DateTime.Now
    for i in 0..n-1 do
        Tree.minInTree tree |> ignore
               //Tree.parallelMin 0 tree
    let finish2 = System.DateTime.Now

    let start3 = System.DateTime.Now
    for i in 0..n-1 do
        Tree.minInTree tree |> ignore
    let finish3 = System.DateTime.Now


    printfn "Time 1: %A; Time2: %A; Time3: %A"
            ((finish1 - start1).TotalMilliseconds / float n)
            ((finish2 - start2).TotalMilliseconds / float n)
            ((finish3 - start3).TotalMilliseconds / float n)
    //let min1 = Tree.parallelMin 0 tree
    (*
    let n = 10
    let start1 = System.DateTime.Now
    for i in 0..n - 1 do
        Tree.parallelMin 3 tree |> ignore
    let finish1 = System.DateTime.Now
    for i in 0..n - 1 do
        Tree.minInTree tree |> ignore
    let finish2 = System.DateTime.Now

    for i in 0..n - 1 do
        Tree.parallelMin2 3 tree |> ignore
    let finish3 = System.DateTime.Now

    let parallelTime = ((finish1-start1).TotalMilliseconds / float n)
    let parallelTime2 = ((finish3-finish2).TotalMilliseconds / float n)
    let sequentialTime = ((finish2-finish1).TotalMilliseconds / float n)

    printfn "Parallel time: %A ms; Parallel time2: %A ms; Sequential time: %A ms; ratio1: %A; ratio2: %A "  parallelTime parallelTime2 sequentialTime (sequentialTime/parallelTime) (sequentialTime/parallelTime2)
*)
    0
