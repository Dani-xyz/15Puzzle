// 15-puzzle game

open System


type Game =
    { Board: int list
      InitialState: int list
      Moves: int }


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
    let zeroRow = findZeroIndex board / 4
    let inversions = countInversions board
    zeroRow % 2 <> inversions % 2


let rec newGame () =
    let rnd = Random()
    let board = [0 .. 15] |> List.sortBy (fun _ -> rnd.Next(0, 16))
    if not (isSolvable board) || solved board then
        newGame()
    else
        { Board = board
          InitialState = board
          Moves = 0 }


let swap i j game =
    let board = game.Board
    let f idx v =
        match idx with
        | _ when idx = i -> board.[j]
        | _ when idx = j -> board.[i]
        | _              -> v
    let newBoard = List.mapi f board
    { game with Board = newBoard }


let reset game =
    { game with
        Board = game.InitialState
        Moves = 0 }


let incMoves game =
    { game with Moves = game.Moves + 1 }


let render game =
    let b = List.map (fun x -> if x = 0 then "  " else $"{x, 2}") game.Board
    $"┌────┬────┬────┬────┐\n\
      │ {b.[0]} │ {b.[1]} │ {b.[2]} │ {b.[3]} │\n\
      ├────┼────┼────┼────┤\n\
      │ {b.[4]} │ {b.[5]} │ {b.[6]} │ {b.[7]} │\n\
      ├────┼────┼────┼────┤\n\
      │ {b.[8]} │ {b.[9]} │ {b.[10]} │ {b.[11]} │\n\
      ├────┼────┼────┼────┤\n\
      │ {b.[12]} │ {b.[13]} │ {b.[14]} │ {b.[15]} │\n\
      └────┴────┴────┴────┘\n\
      Moves: {game.Moves}\n\
      \n\
      [N] New game\n\
      [R] Restart\n\
      [Q] Quit"


let rec main game =
    let zero = findZeroIndex game.Board

    let rec loop () =
        match Console.ReadKey().Key with
        | ConsoleKey.Q                             -> () // exit
        | ConsoleKey.R                             -> game |> reset |> main
        | ConsoleKey.N                             -> newGame() |> main
        | ConsoleKey.UpArrow    when zero / 4 <> 3 -> game |> incMoves |> swap zero (zero + 4) |> main
        | ConsoleKey.DownArrow  when zero / 4 <> 0 -> game |> incMoves |> swap zero (zero - 4) |> main
        | ConsoleKey.LeftArrow  when zero % 4 <> 3 -> game |> incMoves |> swap zero (zero + 1) |> main
        | ConsoleKey.RightArrow when zero % 4 <> 0 -> game |> incMoves |> swap zero (zero - 1) |> main
        | _                                        -> loop()

    let rec smallLoop () =
        match Console.ReadKey().Key with
        | ConsoleKey.Q -> () // exit
        | ConsoleKey.R -> reset game |> main
        | ConsoleKey.N -> newGame() |> main
        | _            -> smallLoop()

    Console.Clear()
    render game |> printfn "%s"

    if solved game.Board then
        printfn "\n─── SOLVED! ───"
        smallLoop()
    else
        loop()


let _ =
    // switch to alternate screen + hide cursor
    printf "\x1b[?1049h\x1b[?25l"

    // play
    newGame() |> main

    // show cursor + return from alternate screen + move cursor up one line
    printf "\x1b[?25h\x1b[?1049l\x1b[1A"


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