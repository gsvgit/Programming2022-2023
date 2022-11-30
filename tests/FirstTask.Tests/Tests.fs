namespace FirstTask.Tests

open Expecto
open QSort
open qSortFun

module SayTests =
    let partitionTest (arr: array<int>) (left: uint) (length: uint) pivot =
        if arr.Length > 0
        //then
        //    skiptest ""
        then
            let (left: int<memoryIndex>), length =
                let left' = left % uint arr.Length

                let length' =
                    if arr.Length - int left' = 0 then
                        0u
                    else
                        length % uint (abs (arr.Length - int left'))

                int left' * 1<memoryIndex>, int length'

            let expectedLeft, expectedRight =
                Array.partition (fun x -> x <= pivot) arr.[int left .. (int left + length - 1)]

            let actualLeft, actualRight =
                let tmp = Array.zeroCreate arr.Length

                let left, right =
                    partition (MyArray(arr, left, length)) (MyArray(tmp, left, length)) pivot

                left.Memory[int left.Head .. int left.Head + left.Length - 1],
                right.Memory[int right.Head .. int right.Head + right.Length - 1]

            Expect.sequenceEqual actualLeft expectedLeft ""
            Expect.sequenceEqual actualRight (Array.rev expectedRight) ""

    [<Tests>]
    let tests =
        (*let f _ =
                  let actualResult = sum 10 0
                  Expect.equal actualResult 55 "Sum from 10 to 0 should be 55."
        *)
        testList
            "Partition tests"
            [ testProperty "Partition is partition"
              <| fun (arr: array<int>) pivot ->
                  let expectedLeft, expectedRight = Array.partition (fun x -> x <= pivot) arr

                  let actualLeft, actualRight =
                      let left, right =
                          partition
                              (MyArray(arr, 0<memoryIndex>, arr.Length))
                              (MyArray(Array.zeroCreate arr.Length, 0<memoryIndex>, arr.Length))
                              pivot

                      left.Memory[int left.Head .. int left.Head + left.Length - 1],
                      right.Memory[int right.Head .. int right.Head + right.Length - 1]

                  Expect.sequenceEqual actualLeft expectedLeft ""
                  Expect.sequenceEqual actualRight (Array.rev expectedRight) ""

              testProperty "Partition is partition on part of memory"
              <| fun (arr: array<int>) (left: uint) (length: uint) pivot -> partitionTest arr left length pivot

              //
              // testProperty "Sort is sort"
              // <| fun (arr:array<int>) ->
              //     Expect.sequenceEqual (qSort arr) (Array.sort arr)
              //
              testProperty "FastSort is sort"
              <| fun (arr: array<int>) -> Expect.sequenceEqual (fastQSort arr) (Array.sort arr)
              testProperty "FastSortFun is sort"
              <| fun (arr: array<int>) -> Expect.sequenceEqual (fastQSortFun arr) (Array.sort arr) ]
