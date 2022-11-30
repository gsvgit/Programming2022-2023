open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type SortsBenchmark() =
    let mutable data = [|0|]
    let random = System.Random()

    [<Params(500)>]
    member val arrLength = 0 with get, set

    [<GlobalSetup>]
    member this.SetUpArrayToSort () =
        data <- Array.init this.arrLength (fun _ -> random.Next())

    [<Benchmark(Baseline = true)>]
    member this.SystemSort() =
        Array.sort data

    [<Benchmark>]
    member this.LeonidSort() =
        QSort.fastQSort data

    [<Benchmark>]
    member this.PolinaSort() =
        qSortFun.fastQSortFun data

    [<Benchmark>]
    member this.NaiveQSort() =
        QSort.qSort data

[<EntryPoint>]
let main argv =
    let summary = BenchmarkRunner.Run<SortsBenchmark>();
    0
