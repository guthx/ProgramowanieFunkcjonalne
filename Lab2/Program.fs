open System

type BigNum = List<byte>

let StringToBN (s:string) =
    let ch = s.ToCharArray() |> List.ofArray
    List.map(fun (c:char) -> (byte c) - 0x30uy) ch


let BNToString (bn:BigNum) =
    let ch = List.map(fun (c:byte) -> (char (c + 0x30uy))) bn
    let str = Array.ofList ch
    new string(str)


let addBN (a:BigNum) (b:BigNum) =
    let rec add (l1:BigNum) (l2:BigNum) (p:byte) (lw:BigNum) =
        match l1, l2 with
        | h1::t1, h2::t2 -> add t1 t2 ((h1+h2+p)/10uy) (((h1+h2+p)%10uy)::lw)
        | h1::t1, [] -> add t1 [] ((h1+p)/10uy) ((h1+p)::lw)
        | [], h2::t2 -> add [] t2 ((h2+p)/10uy) ((h2+p)::lw)
        | _, _ -> lw
    add (List.rev(a)) (List.rev(b)) 0uy []

let multBN (a:BigNum) (b:BigNum) = 
    let rec mult (l1:BigNum) (mnoznik:byte) (p:byte) (lw:BigNum) z =
            match l1, mnoznik, z with
            | h1::t1, m, z -> mult t1 m (((m*h1)+p)/10uy) ((((h1*m)+p)%10uy)::lw) z
            | [], 0uy, 0 -> lw
            | [], 0uy, z -> mult [] 0uy 0uy (lw@[0uy]) (z-1)
            | [], m, z -> if p > 0uy then (mult [] 0uy 0uy (p::lw) z)
                          else (mult [] 0uy 0uy lw z)
    let rec listalist (l1:BigNum) (l2:BigNum) (lw:List<BigNum>) z =
        match l1, l2 with
        | h1::t1, l2 -> listalist t1 l2 ( (mult l2 h1 0uy [] z )::lw) (z+1)
        | [], _ -> lw
    let rec dodajlisty (l1:List<BigNum>) (wynik:BigNum) =
        match l1 with
        | h1::t1 -> dodajlisty t1 (addBN h1 wynik)
        | [] -> wynik
    dodajlisty (listalist (List.rev(a)) (List.rev(b)) [] 0) [0uy]



[<EntryPoint>]
let main argv = 
    printfn "%A" (BNToString (multBN (StringToBN "111") (StringToBN "231")))
    printfn "%A" (13uy%10uy)
    printfn "%A" (BNToString [1uy;3uy;2uy])
    0 // return an integer exit code
