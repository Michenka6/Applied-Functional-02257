// let design1 tree = snd (design' tree)

// // Michael R. Hansen    30-05-2023
// #r "nuget: Plotly.NET, 4.0.0"


// let test t =
//     t
//     |> design
//     |> addVerticals 0.0
//     |> treeToPoints
//     |> Chart.combine
//     |> Chart.withXAxis mirroredXAxis
//     |> Chart.withYAxis mirroredYAxis
//     |> Chart.show

// let test1 t =
//     t
//     |> design
//     |> addVerticals 0
//     //|> treeToPoints
//     |> printfn "%A"

// let test2 t =
//     t
//     |> design
//     |> absTree
//     |> treeToPoints
//     |> Chart.combine
//     |> Chart.withXAxis mirroredXAxis
//     |> Chart.withYAxis mirroredYAxis
//     |> Chart.show




// //test1 t;;
// //test t ;;
// //design1 t |> printfn "%A"
// //test2 t;;
// test3 a
