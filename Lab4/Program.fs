// Learn more about F# at http://fsharp.org

open System

type kodArt = string
type nazwaArt = string
type cena = int
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
    | ([], _) -> ([], 0)
    | ((sztA, kA)::zak, rej) ->
        let (nazA, cenA) = szukajArt kA rej
        let kosztA = sztA * cenA
        let (Rach, suma) = utworzRachunek(zak, rej)
        in ((sztA, nazA, kosztA)::Rach, kosztA + suma)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
