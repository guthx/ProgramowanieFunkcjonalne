open System

type kodArt = string
type nazwaArt = string
type cena = double
type rejestr = (kodArt * (nazwaArt * cena)) list

type sztukiA = int
type pozArt = sztukiA * kodArt
type zakupy = pozArt list

type infoArt = sztukiA * nazwaArt * cena
type infoLista = infoArt list
type rachunek = infoLista * cena

exception BLEDNY_KOD_ARTYKULU
let rec szukajArt (art : kodArt) (r : rejestr) : (nazwaArt * cena) =
    match r with
    | [] -> raise BLEDNY_KOD_ARTYKULU
    | (k, (n, c))::tail -> if art = k 
                           then (n, c)
                           else szukajArt art tail
                            
let rec czyElement x = function
    | y::ys -> if x = y then true else czyElement x ys
    | [] -> false
    
let rec utworzRachunek = function
    | ([], _) -> ([], 0.0)
    | ((sztA, kA)::zak, rej) ->
        let (nazA, cenA) = szukajArt kA rej
        let kosztA = sztA * cenA
        let (Rach, suma) = utworzRachunek(zak, rej)
        ((sztA, nazA, kosztA)::Rach, kosztA + suma)

let rec isCodeInRegister (code:kodArt) (register:rejestr) =
    match register with
    | [] -> false
    | (c, (name, price))::tail -> if code = c then true
                                  else isCodeInRegister code tail

let rec areCodesUnique (register:rejestr) =
    match register with
    | [] -> true
    | (code, (name, price))::tail -> 
        if (isCodeInRegister code tail) = true then false
        else areCodesUnique tail

let rec arePricesNonNegative (register:rejestr) =
    match register with
    | [] -> true
    | (code, (name, price))::tail ->
        if price < 0.0 then false
        else arePricesNonNegative tail

let averagePrice (register:rejestr) =
    let rec loop sum count registerT =
        match registerT with
        | [] -> sum/(double)count
        | (code, (name, price))::tail -> loop (sum+price) (count+1) tail
    loop 0.0 0 register

let rec setSale (code:kodArt) (register:rejestr) =
    List.map(fun (codeA, (name, price))  -> 
        if codeA = code then (codeA, (name, price*4.0/5.0))
        else (codeA, (name, price))
    ) register

let rec getArticles min max (register:rejestr) =
    match register with
    | (code, (name, price))::tail -> if (price >= min && price <= max) 
                                     then (code, (name, price))::(getArticles min max tail)
                                     else getArticles min max tail
    | [] -> []

(*
let usunNieparzyste list =
    let rec loop oldList newList =
        match oldList with
        | [] -> List.rev newList
        | x1::x2::tail -> loop tail (x2::newList)
        | x1::tail -> List.rev newList
    loop list []
    *)

let usunNieparzyste list = 
    (List.chunkBySize 2 list) |> 
    List.fold (fun newList el -> 
    match el with
    | x1::x2::t -> newList @ [x2]
    | x1::t -> newList
    | [] -> newList
    ) []

(*
let minElement (list:List<int>) : int option =
    if (List.isEmpty list) then None else
    let rec loop min list = 
        match list with
        | [] -> Some min
        | x::tail -> if x < min 
                     then loop x tail
                     else loop min tail
    loop list.[0] list
*)
let minElement (list:List<int>) : int option =
    List.fold (fun min el -> 
        match min with
        | None -> Some el
        | Some x -> if el < x then Some el else Some x) None list
    
(*
let maxElement (list:List<int>) : int option =
    if (List.isEmpty list) then None else
    let rec loop max list = 
        match list with
        | [] -> Some max
        | x::tail -> if x > max 
                     then loop x tail
                     else loop max tail
    loop list.[0] list
*)

let maxElement (list:List<int>) : int option =
    List.fold (fun max el -> 
        match max with
        | None -> Some el
        | Some x -> if el > x then Some el else Some x) None list

exception ZLA_LISTA
let pairList list =
    (List.chunkBySize 2 list) |> List.map(function
        | a::b::x -> (a, b)
        | _ -> raise ZLA_LISTA
        )

(*
let length list =
    let rec loop list count =
        match list with
        | [] -> count
        | x::tail -> loop tail (count+1)
    loop list 0
*)
let length list =
    List.fold (fun length _ -> length+1) 0 list

(*
let rec krotnosc(x, ys) =
    match ys with
    | y::tail -> if x = y then (1+krotnosc(x, tail))
                 else krotnosc(x, tail)
    | [] -> 0
*)

let krotnosc x list = List.fold (fun stan el -> if el = x then stan+1 else stan) 0 list

let rec areElementsUnique list =
    match list with
    | [] -> true
    | x::tail -> if (List.contains x tail) then false
                 else areElementsUnique tail
let map1 func list =
    let rec loop newList listT =
        match listT with
        | [] -> List.rev newList
        | x::t -> loop ((func x)::newList) t
    loop [] list

let rec map2 func list =
    match list with
    | [] -> []
    | x::tail -> (func x)::(map2 func tail)

let rev list =
    let rec loop nList list =
        match list with
        | [] -> nList
        | x::tail -> loop (x::nList) tail
    loop [] list

let rec nty list n =
    match list with
    | [] -> None
    | x::tail -> if n = 0 then Some x
                 else nty tail (n-1)

let rec areElementsPositive list =
    match list with
    | [] -> true
    | x::tail -> if x < 0 then false
                 else areElementsPositive tail

let wstaw list n el =
    let rec loop newList oldList i =
        match oldList with
        | x::tail -> if i = n then loop (x::el::newList) tail (i+1)
                     else loop (x::newList) tail (i+1)
        | [] -> if i < n then raise ZLA_LISTA
                elif i = n then List.rev (el::newList)
                else List.rev newList
    loop [] list 0
    

[<EntryPoint>]
let main argv =
    let Rejestr = [
        ("a1", ("ser bialy", 3.0));
        ("a2", ("szprotki", 2.0));
        ("a3", ("sok", 4.0))
    ]
    let Rejestr2 = [
        ("a1", ("ser bialy", 3.0));
        ("a2", ("szprotki", 2.0));
        ("a3", ("sok", 4.0));
        ("a3", ("mleko", -1.0))
    ]
    let zakupy = [
        (4.0, "a2");
        (1.0, "a1")
    ]
   // let artykul1 = szukajArt "a38" ([]:rejestr)
    let artykul2 = szukajArt "a3" Rejestr
   // let rachunek1 = utworzRachunek([], Rejestr);
    let rachunek2 = utworzRachunek(zakupy, Rejestr);
        
    printfn "%A" artykul2
    printfn "%A" rachunek2
    printfn "%A" (areCodesUnique Rejestr2)
    printfn "%A" (arePricesNonNegative Rejestr2)
    printfn "%A" (averagePrice Rejestr)
    printfn "%A" (setSale "a3" Rejestr)
    printfn "usunNieparzyste: %A" (usunNieparzyste [1;2;3;4;5])
    printfn "minElement: %A" (minElement [5;2;3])
    printfn "%A" (minElement [])
    printfn "maxElement: %A" (maxElement [5;2;3])
    printfn "%A" (pairList [1;2;3;4])
    printfn "Length: %A" (length [1;2;3])
    printfn "Krotnosc: %A" (krotnosc 1 [1;2;3;1;4;1])
    printfn "%A" (map1 (fun x -> x+5) [1;2;3])
    printfn "%A" (map2 (fun x -> x+5) [1;2;3])
    printfn "%A" (rev [1;2;3])
    printfn "%A" (areElementsUnique [1;2;3;1])
    printfn "%A" (areElementsUnique [1;2;3])
    printfn "%A" (nty [1;2;3;4] 2)
    printfn "%A" (nty [1;2] 3)
    printfn "%A" (areElementsPositive [2;-1;3])
    printfn "%A" (areElementsPositive [2;1;3])
    printfn "%A" (wstaw [1;2;3;4] 2 8)
    printfn "%A" (wstaw [1;2;3;4] 4 8)
    printfn "%A" (getArticles 3.0 4.0 Rejestr)
    0 // return an integer exit code