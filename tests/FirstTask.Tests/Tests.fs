namespace FirstTask.Tests

open Expecto
open QSort

module SayTests =
    [<Tests>]
    let tests =
        (*let f _ =
                  let actualResult = sum 10 0
                  Expect.equal actualResult 55 "Sum from 10 to 0 should be 55."
        *)
        testList
            "Partition tests"
            [ testProperty "Partition is partition"
              <| fun (arr:array<int>) pivot ->
                  let expectedLeft, expectedRight = Array.partition (fun x -> x <= pivot) arr
                  let actualLeft, actualRight =
                      let left, right = partition
                                            (MyArray(arr, 0, arr.Length))
                                            (MyArray(Array.zeroCreate arr.Length, 0, arr.Length))
                                            pivot
                      left.Memory[left.Edge..left.Edge + left.Length - 1],
                      right.Memory[right.Edge..right.Edge + right.Length - 1]

                  Expect.sequenceEqual actualLeft expectedLeft ""
                  Expect.sequenceEqual actualRight (Array.rev expectedRight) ""
              //
              // testProperty "Sort is sort"
              // <| fun (arr:array<int>) ->
              //     Expect.sequenceEqual (qSort arr) (Array.sort arr)
              //
              // testProperty "FastSort is sort"
              // <| fun (arr:array<int>) ->
              //     Expect.sequenceEqual (fastQSort arr) (Array.sort arr)
            ]





