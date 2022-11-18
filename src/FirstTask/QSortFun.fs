/// MADE BY POLINA SAVELYEVA

module qSortFun

open QSort

let f (arr: MyArray<_>) (tmp: MyArray<_>) =
    for i in 0 .. arr.Memory.Length - 1 do
        arr.Memory[ i ] <- tmp.Memory[i]

let fastQSortFun (arr: array<'value>) =

    let tmp = Array.zeroCreate arr.Length

    let rec sort (arr: MyArray<_>) (tmp: MyArray<_>) : MyArray<_> =

        if arr.Length > 1 then
            let pivot = arr.GetItem 0<arrayIndex>

            let leftPart, rightPart =
                partition (MyArray(arr.Memory, arr.Head + 1<memoryIndex>, arr.Length - 1)) tmp pivot

            f arr tmp

            arr.SetItem(leftPart.Length * 1<arrayIndex>) pivot

            let sortedLeft = sort leftPart (MyArray(arr.Memory, leftPart.Head, leftPart.Length))

            let sortedRight =
                sort rightPart (MyArray(arr.Memory, rightPart.Head, rightPart.Length))

            let res =
                MyArray(arr.Memory, sortedLeft.Head, sortedLeft.Length + 1 + sortedRight.Length)

            res
        else
            arr

    (sort (MyArray(arr, 0<memoryIndex>, arr.Length)) (MyArray(tmp, 0<memoryIndex>, tmp.Length)))
        .Memory
