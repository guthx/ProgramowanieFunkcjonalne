open System

let decimalToFraction (decimal:String) =
    let indexOfDot = decimal.IndexOf('.')
    let indexOfOpen = decimal.IndexOf('(')
    let indexOfClose = decimal.IndexOf(')')
    let x = decimal.Substring(0, indexOfDot)
    let y = decimal.Substring(indexOfDot+1, indexOfOpen-1-indexOfDot)
    let z = decimal.Substring(indexOfOpen+1, indexOfClose-1-indexOfOpen)
    let a = int x
    let b = (int (String.Concat(y, z)) - int y)
    let nines = new String('9', z.Length)
    let zeros = new String('0', y.Length)
    let c = int (String.Concat(nines, zeros))

    (a, b, c)

let rec NWD a b =
    let reszta = a%b
    if reszta = 0 then b
    else NWD b reszta

let skroc (a, b, c) =
    let d = NWD b c
    (a, b/d, c/d)

let wariacje n k =
    let list = List.init k (fun index -> 1)
    let rec loop (l:List<int>) (wariacje:List<List<int>>) (i:int) =
        if i >= k then List.rev(wariacje)
        elif l.[i] <> n then 
                      let l2 = List.mapi (fun index x -> if index = i then x+1 else x) l
                      if i > 0 && l.[i-1] = n then 
                                             let lNew = List.mapi (fun index x -> if index < i then 1 else x) l2
                                             loop lNew (lNew::wariacje) 0
                      else loop l2 (l2::wariacje) i
        else loop l wariacje (i+1)
    loop list [list] 0
            

[<EntryPoint>]
let main argv =
    let ulamek = (decimalToFraction "2.24(98)")
    printfn "%A" ulamek
    printfn "%A" (skroc ulamek)
    printfn "%A" (wariacje 2 4)
    0 // return an integer exit code
