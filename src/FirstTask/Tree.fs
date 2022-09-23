module Tree

type IVisitor =
    abstract HandleNode: Node -> unit
    abstract HandleLeaf: Leaf -> unit

and INode =
    abstract Value: int
    abstract Handle: IVisitor -> unit

and Node (value: int, left:INode, right:INode) =
    member this.Left = left
    member this.Right = right

    interface INode with
        member this.Value = value
        member this.Handle (visitor:IVisitor) = visitor.HandleNode this

and Leaf (value: int) =
    interface INode with
        member this.Value = value
        member this.Handle (visitor:IVisitor) = visitor.HandleLeaf this

type ValuesCollector (acc:ResizeArray<_>) =
    interface IVisitor with
        member this.HandleNode (node:Node) =
            acc.Add (node :> INode).Value
            node.Left.Handle this
            node.Right.Handle this
        member this.HandleLeaf (leaf:Leaf) =
            acc.Add (leaf :> INode).Value

type Tree =
    | Leaf of value: int
    | Node of value:int * left:Tree * right:Tree

