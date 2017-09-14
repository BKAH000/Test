open System
let fibonacci (steps:int, fib_list:List<int>) =
    let mutable mL = fib_list
    [1..steps] |> List.iter (fun x -> mL <- List.append mL [mL.[mL.Length-1] + mL.[mL.Length-2] ])
    printfn "%A" mL
let rec fibonacci2 (steps:int, fib_list:List<int>):List<int> =
    match steps with
    | steps when steps = 0 -> fib_list
    | _ -> fibonacci2(steps-1, List.append fib_list [fib_list.[fib_list.Length-1] + fib_list.[fib_list.Length-2]])
let main = fibonacci2(4, [1;1]) |> printfn "%A"
main
//Kommentar
Console.ReadKey() |> ignore                                              