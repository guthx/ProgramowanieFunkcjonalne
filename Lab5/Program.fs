// Learn more about F# at http://fsharp.org

open System

let listaTest = [4;1;5;8;3]
let stringList = ["3,4"; "2,1"; "-18"]

let suma list = List.fold (fun sum el -> sum + el) 0 list

let intToStringList list = List.map (fun x -> x.ToString()) list

let stringToFloatList (list:List<string>) : float option list =
    List.map (fun x ->
        match (System.Single.TryParse(x)) with 
        | true, n -> Some (float n)
        | _ -> None) list

let printStringFloatPairs (stringList:List<string>) (floatList:List<float option>) =
    (List.zip stringList floatList) |> List.iter (fun x -> printf "%A" x)

let findMaxMin list = 
    List.fold (fun (min, max) el -> 
        match (min, max) with
        | (None, None) -> (Some el, Some el)
        | (Some min, Some max) -> if el < min 
                                  then (Some el, Some max)
                                  elif el > max
                                  then (Some min, Some el)
                                  else (Some min, Some max)
    ) (None, None) list

[<EntryPoint>]
let main argv =
    printfn "Suma: %A" (suma listaTest)
    printfn "IntToStringList: %A" (intToStringList listaTest)
    printfn "StringToFloatList: %A" (stringToFloatList stringList)
    printStringFloatPairs (stringList) (stringToFloatList stringList)
    printfn "\nminMax: %A" (findMaxMin listaTest)
    0 // return an integer exit code
