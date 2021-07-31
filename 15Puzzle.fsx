// 15-puzzle game

open System


type Game =
    { Board: int list
      InitialState: int list }


let findZeroIndex =
    List.findIndex (fun x -> x = 0)


let solved board =
    board = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0]


let countInversions (board: int list) =
    let rec loop i acc =
        if i = 15 then
            acc
        else
            let (_, count) =
                [i + 1 .. 15]
                |> List.countBy (fun j -> board.[i] > board.[j] && board.[j] <> 0)
                |> fun ls -> match List.tryFind (fun (b, _) -> b) ls with Some t -> t | None -> (true, 0)
            loop (i + 1) (acc + count)
    loop 0 0


let isSolvable board =
    let zeroRow = (findZeroIndex board) / 4
    let inversions = countInversions board
    (zeroRow % 2) <> (inversions % 2)


let rec newGame () =
    let rnd = Random()
    let board = [0 .. 15] |> List.sortBy (fun _ -> rnd.Next(0, 16))
    if not (isSolvable board) || solved board then
        newGame()
    else
        { Board = board
          InitialState = board }


let swap a b game =
    let f = function
        | x when x = a -> b
        | x when x = b -> a
        | x            -> x
    let newBoard = List.map f game.Board
    { game with Board = newBoard }


let renderGame game =
    let b = List.map (fun x -> if x = 0 then "  " else $"{x, 2}") game.Board
    printfn $"┌────┬────┬────┬────┐\n\
              │ {b.[0]} │ {b.[1]} │ {b.[2]} │ {b.[3]} │\n\
              ├────┼────┼────┼────┤\n\
              │ {b.[4]} │ {b.[5]} │ {b.[6]} │ {b.[7]} │\n\
              ├────┼────┼────┼────┤\n\
              │ {b.[8]} │ {b.[9]} │ {b.[10]} │ {b.[11]} │\n\
              ├────┼────┼────┼────┤\n\
              │ {b.[12]} │ {b.[13]} │ {b.[14]} │ {b.[15]} │\n\
              └────┴────┴────┴────┘"


let rec main game =
    let board = game.Board
    let zero = findZeroIndex board

    let rec loop () =
        if Console.KeyAvailable then
            match Console.ReadKey().Key with
            | ConsoleKey.Q          -> Environment.Exit 0
            | ConsoleKey.R          -> main { game with Board = game.InitialState }
            | ConsoleKey.N          -> main <| newGame()
            | ConsoleKey.UpArrow    -> if zero / 4 = 3 then loop() else main <| swap 0 board.[zero + 4] game
            | ConsoleKey.DownArrow  -> if zero / 4 = 0 then loop() else main <| swap 0 board.[zero - 4] game
            | ConsoleKey.LeftArrow  -> if zero % 4 = 3 then loop() else main <| swap 0 board.[zero + 1] game
            | ConsoleKey.RightArrow -> if zero % 4 = 0 then loop() else main <| swap 0 board.[zero - 1] game
            | _                     -> loop()
        else
            loop()

    let rec smallLoop () =
        if Console.KeyAvailable then
            match Console.ReadKey().Key with
            | ConsoleKey.Q -> Environment.Exit 0
            | ConsoleKey.R -> main { game with Board = game.InitialState }
            | ConsoleKey.N -> main <| newGame()
            | _            -> smallLoop()
        else
            smallLoop()

    Console.Clear()
    renderGame game
    printfn "[N] New game\n[R] Restart\n[Q] Quit"

    if solved board then
        printfn "\n─── SOLVED! ───"
        smallLoop()
    
    loop()
    

main <| newGame()

(*
┌────┬────┬────┬────┐
│  1 │  2 │  3 │  4 │
├────┼────┼────┼────┤
│  5 │  6 │  7 │  8 │
├────┼────┼────┼────┤
│  9 │ 10 │ 11 │ 12 │
├────┼────┼────┼────┤
│ 13 │ 14 │ 15 │    │
└────┴────┴────┴────┘
*)