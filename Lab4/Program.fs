// Learn more about F# at http://fsharp.org

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
    0 // return an integer exit code
