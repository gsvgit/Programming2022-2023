namespace FirstTask.Tests

module ExpectoTemplate =

    open Expecto

    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv
        //FirstTask.Tests.SayTests.partitionTest  [|-4; 1; 0; 0; 0; 0; 0; 0; 0|] 3u 4u 7
        //0

