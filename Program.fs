open System

// http://exercism.io/exercises/fsharp/hello-world/readme
let hello arg = printfn "Hello %s" arg

// http://exercism.io/exercises/fsharp/leap/readme
let is_leap(year:int):bool = 
    match year with
    | year when (year % 4  = 0) && (year % 100 = 0 && year % 400  = 0 or not (year % 100 = 0)) -> true
    | _ -> false

// http://exercism.io/exercises/fsharp/bob/readme
let bobs_answer(input:string):string = 
    match input with
    | input when input.Contains "?" -> "Sure"
    | input when input.Contains "!" -> "Whoa, chill out!"
    | input when input.Equals "" -> "Fine. Be that way!"
    | _ -> "Whatever"

// http://exercism.io/exercises/fsharp/gigasecond/readme
let in_1_gs(date:DateTime) = 
    date.AddSeconds(float 1000000000) |> printfn " Datetime : %A"

// http://exercism.io/exercises/fsharp/difference-of-squares/readme
let sum_of_squares(n:int):int = [1..n] |> List.sumBy (fun x -> x*x)

let square_of_sum(n:int):int = [1..n] |> List.sum |> fun x -> x * x

let d_o_s(n:int):int = square_of_sum n - sum_of_squares n |> abs 

// http://exercism.io/exercises/fsharp/rna-transcription/readme 
let rp(c:char):string = 
    match c with 
    | c when c = 'G' -> "C"
    | c when c = 'C' -> "G"
    | c when c = 'T' -> "A"
    | c when c = 'A' -> "U"

let to_RNA(DNA:string):string = 
    String.collect rp DNA

// http://exercism.io/exercises/fsharp/sum-of-multiples/readme
let s_u_m(ms:List<int>, n:int):int = 
    List.distinct [for i in  ms do for j in 1..int (n/i) do if i*j < n then yield i*j] |> List.sum 

// http://exercism.io/exercises/fsharp/space-age/readme
let get_factor(planet:string):float = 
    match planet with
    | "Earth" -> 1.0
    | "Mercury" -> 0.2408467 
    | "Venus" -> 0.61519726 
    | "Mars" -> 1.8808158
    | "Jupiter" -> 11.862615 
    | "Saturn" -> 29.447498
    | "Uranus" -> 84.016846
    | "Neptune" -> 164.79132

let space_age(sec:float, planet:string):float =
    let Earth = (1.0 / 31557600.0)
    ((1.0 / get_factor(planet)) * Earth * sec)

// http://exercism.io/exercises/fsharp/grains/readme                                           
let grains= [1.0..64.0] |> Seq.map(fun x -> 2.0**x) |> fun x -> seq x 
let sum_of_grains = grains |> Seq.sum 

// http://exercism.io/exercises/fsharp/hamming/readme
let calc_Hamming(DNA_1:String, DNA_2:String)= 
   if not (DNA_1.Length = DNA_2.Length) then -1
   else [for i in 0..DNA_1.Length - 1 do if not (DNA_1.[i] = DNA_2.[i]) then yield 1] |> List.sum 

// http://exercism.io/exercises/fsharp/nucleotide-count/readme
type counts = {A : int; C: int; G:int; T:int}

let how_many(c:char, s:string):int = [for i in s do if i = c then yield 1] |> List.sum;

let Nucleotide_Count(input:string):counts = { A = how_many ('A' ,input)  ; C = how_many( 'C' , input)  ; G = how_many ('G' ,input) ; T = how_many ('T' ,input) } 

//http://exercism.io/exercises/fsharp/accumulate/readme
let accumulate(f:'T -> 'T, l:list<'T>):list<'T> = [for i in l do yield f(i)]
accumulate((fun x -> x*x), [1..2]) |> printfn "%A"

Console.ReadKey() |> ignore