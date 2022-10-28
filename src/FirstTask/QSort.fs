module QSort

open System


[<Struct>]
type MyArray<'value> =
    val Memory: array<'value>
    val Left: int
    val Right: int
    new (memory, left, right) = {Memory = memory; Left = left; Right = right}
    member this.GetItem i =
        this.Memory.[this.Left + i]
    member this.SetItem i v =
        this.Memory.[this.Left + i] <- v
    member this.Length = this.Right - this.Left + 1

let qSort (arr:array<'value>) =
    let rec sort (arr:array<'value>) =
        if arr.Length <= 1
        then arr
        else
            let pivot = arr.[0]
            let parts = Array.partition (fun x -> x <= pivot) arr.[1..arr.Length - 1]
            Array.append (fst parts |> sort)(Array.append [|pivot|](snd parts |> sort))
    sort arr

let partition (arr:MyArray<_>) (tmp:MyArray<_>) pivot =
    let rec partition current left right =
        if current < arr.Length
        then
            if arr.GetItem current <= pivot
            then
                tmp.SetItem left (arr.GetItem current)
                partition (current + 1) (left + 1) right
            else
                tmp.SetItem right (arr.GetItem current)
                partition (current + 1) left (right - 1)
        else
            MyArray(tmp.Memory, tmp.Left, right), MyArray(tmp.Memory, left, tmp.Right)
    partition 0 0 (tmp.Length - 1)

let fastQSort (arr:array<'value>) =
    let tmp = Array.zeroCreate arr.Length
    let rec sort (arr:MyArray<_>) (tmp:MyArray<_>) : MyArray<_> =
        printfn $"Array mem = %A{arr.Memory}"
        printfn $"Tmp mem = %A{tmp.Memory}"
        if arr.Length > 1
        then
            let pivot = arr.GetItem 0
            let leftPart,rightPart =
                partition
                    (MyArray(arr.Memory, arr.Left + 1, arr.Right))
                    tmp
                    pivot
            let sortedLeft = sort leftPart (MyArray(arr.Memory, leftPart.Left, leftPart.Right))
            let sortedRight = sort rightPart (MyArray(arr.Memory, rightPart.Left, rightPart.Right))
            let res = MyArray(tmp.Memory, sortedLeft.Left, sortedRight.Right)
            res.SetItem (leftPart.Right + 1) pivot
            res
        else
            arr
    (sort (MyArray(arr, 0, arr.Length - 1)) (MyArray(tmp, 0, tmp.Length - 1))).Memory



