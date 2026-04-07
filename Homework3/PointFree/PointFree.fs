module PointFree

let multiplyList1 x l = List.map (fun y -> y * x) l
let multiplyList2 x = List.map (fun y -> y * x)
let multiplyList3 x = List.map (fun y -> x * y)
let multiplyList4 x = List.map ((*) x)
let multiplyList5 = List.map << (*)