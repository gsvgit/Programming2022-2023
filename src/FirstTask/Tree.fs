module Tree

open FSharpx.Collections

type BinTree<'value> =
    | FullNode of value: 'value * left: BinTree<'value> * right: BinTree<'value>
    | NodeWithLeft of value: 'value * left: BinTree<'value>
    | NodeWithRight of value: 'value * right: BinTree<'value>
    | Leaf of value: 'value

[<RequireQualifiedAccess>]
type BinTree2<'value> = Node of value: 'value * left: Option<BinTree2<'value>> * right: Option<BinTree2<'value>>
//| Leaf of value:'value

let tree = FullNode(3, BinTree.Leaf(1), NodeWithLeft(4, BinTree.Leaf(2)))
//let tree2 = BinTree2.Node(3, BinTree2.Leaf(1), Some (BinTree2.Node(4, BinTree2.Leaf(2), None)))

let rec minInTree tree =
    match tree with
    | Leaf v -> v
    | NodeWithLeft (v, l) -> min v (minInTree l)
    | NodeWithRight (v, r) -> min v (minInTree r)
    | FullNode (v, l, r) -> min v (min (minInTree l) (minInTree r))

let rec generateRandomFullTreeOfHeight (randomizer: System.Random) n =
    if n = 1 then
        Leaf(randomizer.Next())
    else
        FullNode(
            randomizer.Next(),
            (generateRandomFullTreeOfHeight randomizer (n - 1)),
            (generateRandomFullTreeOfHeight randomizer (n - 1))
        )

let parallelMin level tree =
    let rec parallelMinInTree parallelLevel tree =
        match tree with
        | Leaf v -> v
        | NodeWithLeft (v, l) -> min v (parallelMinInTree parallelLevel l)
        | NodeWithRight (v, r) -> min v (parallelMinInTree parallelLevel r)
        | FullNode (v, l, r) ->
            if parallelLevel = 0 then
                min v (min (minInTree l) (minInTree r))
            else
                let tasks =
                    [| async { return parallelMinInTree (parallelLevel - 1) l }
                       async { return parallelMinInTree (parallelLevel - 1) r } |]

                let results = tasks |> Async.Parallel |> Async.RunSynchronously
                Array.min results |> min v

    parallelMinInTree level tree

let parallelMin2 level tree =
    let valuesFromTopPart = ResizeArray()

    let rec collectTasks level tree =
        if level = 0 then
            [ async { return minInTree tree } ]
        else
            match tree with
            | Leaf v -> [ async { return minInTree tree } ]
            | NodeWithLeft (v, l) ->
                valuesFromTopPart.Add v
                collectTasks level l
            | NodeWithRight (v, r) ->
                valuesFromTopPart.Add v
                collectTasks level r
            | FullNode (v, l, r) ->
                valuesFromTopPart.Add v

                (collectTasks (level - 1) l)
                @ (collectTasks (level - 1) r)

    let tasks = collectTasks level tree
    let valuesFromBottomPart = tasks |> Async.Parallel |> Async.RunSynchronously

    if valuesFromTopPart.Count > 0 then
        min (Array.min valuesFromBottomPart) (valuesFromTopPart.ToArray() |> Array.min)
    else
        Array.min valuesFromBottomPart
