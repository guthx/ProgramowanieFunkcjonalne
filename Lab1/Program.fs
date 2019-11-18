open System
[<EntryPoint>]
let main argv = 
    (*
    let liczby =
        let rec readNum sum iloczyn qty =
            let line = Console.ReadLine()
            let number = System.Int32.Parse line
            match number with
            | 0 -> (sum, iloczyn, qty)
            | _ -> readNum (sum + number) (iloczyn * number) (qty + 1)
        let (sum, iloczyn, qty) = readNum 0 1 0
        let geo = ((float iloczyn) ** (1.0/float qty))
        (sum, geo)

    //let (sum, geo) = liczby 
    *)
    let drawA size =
        let rec drawLine lineSize =
            printf "*"
            match lineSize with
            | 1 -> printf "\n"
            | _ -> drawLine (lineSize - 1)
        let rec drawTree start treeSize =
            drawLine start
            if start < treeSize then drawTree (start+1) treeSize
        if size < System.Console.WindowWidth && size < System.Console.WindowHeight then
         drawTree 1 size
        else printfn "Za duzy rozmiar"
             
    let drawB size =
        let rec drawGap lineNum maxLines =
            if (maxLines - lineNum) > 0 then
                printf " "
                drawGap (lineNum + 1) maxLines
        let rec drawLine lineSize =
            printf "*"
            match lineSize with
            | 1 -> printf "\n"
            | _ -> drawLine (lineSize - 1)
        let rec drawTree start treeSize =
            drawGap start treeSize
            drawLine (start*2 - 1)
            if start < treeSize then drawTree (start+1) treeSize
        if (size*2 - 1) <= System.Console.WindowWidth && size <= System.Console.WindowHeight then
         drawTree 1 size
        else printfn "Za duzy rozmiar"
    
    let drawC size =
        let rec drawGap lineNum maxLines =
            if (maxLines - lineNum) > 0 then
                printf " "
                drawGap (lineNum + 1) maxLines
        let rec drawLine lineSize =
            printf "*"
            match lineSize with
            | 1 -> printf "\n"
            | _ -> drawLine (lineSize - 1)
        let rec drawTree start treeSize =
            drawGap start treeSize
            drawLine start
            if start < treeSize then drawTree (start+1) treeSize
        if (size) <= System.Console.WindowWidth && size <= System.Console.WindowHeight then
         drawTree 1 size
        else printfn "Za duzy rozmiar"
    //printfn "%A" sum
    //printfn "%A" geo

    let dzielniki number =
        let rec loop currNum endNum list =
           if currNum = endNum then list else
           if (endNum % currNum) = 0 then loop (currNum+1) endNum (List.append list [currNum])
                        else loop (currNum+1) endNum list
        loop 2 number []

    //let size = System.Int32.Parse (Console.ReadLine())

    //drawC size

    printfn "%A" (dzielniki 16)
    0 // return an integer exit code
