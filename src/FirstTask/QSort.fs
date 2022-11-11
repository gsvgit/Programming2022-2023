module QSort

type MyArray<'value> =
    struct
        val Memory: array<'value>
        val Edge: int
        val Length: int

        new(memory, edge, length) =
            { Memory = memory
              Edge = edge
              Length = length }

    member this.GetItem i =
        if i >= this.Length then
            failwith $"Index %A{i} is out of range."
        else
            this.Memory[this.Edge + i]

    member this.SetItem i v =
        if i >= this.Length then
            failwith $"Index %A{i} is out of range."
        else
            this.Memory[ this.Edge + i ] <- v
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
    let rec loopy left length current =
        if current < myArr.Length then

            if myArr.GetItem current <= pivot then
                myTmp.SetItem left (myArr.GetItem current)
                loopy (left + 1) (length - 1) (current + 1)
            else
                myTmp.SetItem (left + length - 1) (myArr.GetItem current)
                loopy left (length - 1) (current + 1)

        else
            let leftLength = left - myTmp.Edge
            let rightLength = myTmp.Length - leftLength
            MyArray(myTmp.Memory, myTmp.Edge, leftLength), MyArray(myTmp.Memory, left, rightLength)

    loopy 0 myArr.Length 0



let fastQSort (arr: array<'value>) =

    let tmp = Array.zeroCreate arr.Length

    let rec sort (arr: MyArray<_>) (tmp: MyArray<_>) : MyArray<_> =

        if arr.Length > 1 then
            let pivot = arr.GetItem 0

            let leftPart, rightPart =
                partition (MyArray(arr.Memory, arr.Edge + 1, arr.Length - 1)) tmp pivot

            let sortedLeft =
                sort leftPart (MyArray(arr.Memory, leftPart.Edge, leftPart.Length))

            let sortedRight =
                sort rightPart (MyArray(arr.Memory, rightPart.Edge, rightPart.Length))

            let res =
                MyArray(tmp.Memory, sortedLeft.Edge, sortedLeft.Length + 1 + sortedRight.Length)

            res.SetItem sortedLeft.Length pivot
            res
        else
            arr

    (sort (MyArray(arr, 0, arr.Length)) (MyArray(tmp, 0, tmp.Length))).Memory
