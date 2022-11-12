module QSort

[<Measure>]type memoryIndex
[<Measure>]type arrayIndex

type MyArray<'value> =
    struct
        val Memory: array<'value>
        val Head: int<memoryIndex>
        val Length: int

        new(memory, head, length) =
            { Memory = memory
              Head = head
              Length = length }

    member this.GetItem (i:int<arrayIndex>) =
        if int i >= this.Length then
            failwith $"Index %A{i} is out of range."
        else
            this.Memory[int this.Head + int i]

    member this.SetItem (i:int<arrayIndex>) v =
        if int i >= this.Length then
            failwith $"Index %A{i} is out of range."
        else
            this.Memory[ int this.Head + int i ] <- v
  end

let qSort (arr: array<'value>) =
    let rec sort (arr: array<'value>) =
        if arr.Length <= 1 then
            arr
        else
            let pivot = arr[0]

            let leftPart, rightPart =
                Array.partition (fun x -> x <= pivot) arr[1 .. arr.Length - 1]

            Array.append (leftPart |> sort) (Array.append [| pivot |] (rightPart |> sort))

    sort arr

let partition (myArr: MyArray<_>) (myTmp: MyArray<_>) pivot =
    let rec loopy left right (current: int<arrayIndex>) =
        if int current < myArr.Length then

            if myArr.GetItem current <= pivot then
                myTmp.SetItem left (myArr.GetItem current)
                loopy (left + 1<arrayIndex>) right (current + 1<arrayIndex>)
            else
                myTmp.SetItem right (myArr.GetItem current)
                loopy left (right - 1<arrayIndex>) (current + 1<arrayIndex>)

        else
            let leftLength = int left
            let rightLength = (myTmp.Length - 1) - int right
            MyArray(myTmp.Memory, myTmp.Head, leftLength), MyArray(myTmp.Memory, (int myTmp.Head + int right + 1) * 1<memoryIndex>, rightLength)

    if myArr.Length > 0
    then
        loopy 0<arrayIndex> ((myTmp.Length - 1) * 1<arrayIndex>) 0<arrayIndex>
    else
        MyArray(myTmp.Memory,0<memoryIndex>,0),MyArray(myTmp.Memory,0<memoryIndex>,0)

let fastQSort (arr: array<'value>) =

    let tmp = Array.zeroCreate arr.Length

    let rec sort (arr: MyArray<_>) (tmp: MyArray<_>) : MyArray<_> =

        if arr.Length > 1 then
            let pivot = arr.GetItem 0<arrayIndex>

            let leftPart, rightPart =
                partition (MyArray(arr.Memory, arr.Head + 1<memoryIndex>, arr.Length - 1)) tmp pivot

            let sortedLeft =
                sort leftPart (MyArray(arr.Memory, leftPart.Head, leftPart.Length))

            let sortedRight =
                sort rightPart (MyArray(arr.Memory, rightPart.Head, rightPart.Length))

            let res =
                MyArray(arr.Memory, sortedLeft.Head, sortedLeft.Length + 1 + sortedRight.Length)

            res.SetItem (sortedLeft.Length * 1<arrayIndex>) pivot
            res
        else
            arr

    (sort (MyArray(arr, 0<memoryIndex>, arr.Length)) (MyArray(tmp, 0<memoryIndex>, tmp.Length))).Memory
