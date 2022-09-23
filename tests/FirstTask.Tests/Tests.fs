namespace FirstTask.Tests

open Expecto
open FirstTask

module SayTests =
    [<Tests>]
    let tests =
        let f _ =
                  let actualResult = sum 10 0
                  Expect.equal actualResult 55 "Sum from 10 to 0 should be 55."
        testList
            "samples"
            [ testCase "First test for sum"
              <| f
              testCase "Second test for sum"
              <| fun _ ->
                  let actualResult = sum 20 0
                  Expect.equal actualResult 210 "Sum from 10 to 0 should be 55."
              testProperty "First propertyTest"
              <| fun x ->
                  if x > 0
                  then sum x 0 > 0
                  elif x = 0
                  then sum x 0 = 0
                  else true

              //testProperty "Pow tests"
              //<| fun x y ->
                  //pow1 x y = pow2 x y

            ]


