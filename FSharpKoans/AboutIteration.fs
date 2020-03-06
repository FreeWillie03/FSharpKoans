namespace FSharpKoans
open NUnit.Framework

module ``05: To iterate is human; to recurse, divine`` =
    (*
        The `rec` keyword exposes the function identifier for use inside the function.
        And that's literally all that it does - it has no other purpose whatsoever.
    *)

    [<Test>]
    let ``01 `rec` exposes the name of the function for use inside the function`` () =
        let rec converge d c n = // 3 10 0
            match d = c with // 3 = 10      13 = 10     12 = 10     11 = 10
            | false ->
                match d < c with // 3 < 10
                | true -> converge (d+10) c (n+1)       // converge 13 10 1
                | false -> converge (d - 1) c (n+1)     // converge 12 10 2         converge 11 10 3    converge 10 10 4
            | true -> n
        converge 3 10 0 |> should equal 4

    [<Test>]
    let ``02 Tail recursion stops a stack overflow from occurring`` () =
        // CHANGE the recursive function to be tail recursive.
        let myfun n =            //12
            let sq = n*n            // 144
            let v = sq*sq*sq*sq         //429 981 696
            let rec inner count acc=        // inner 144 0   145 1
                match count = v with       //144 = 429 981 696
                | true -> acc
                | false -> inner (count+1) (acc+1)    //inner 145 1 146 2
            inner sq 0
        
        myfun 12 |> should equal 429981552
