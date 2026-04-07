namespace MyProject.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open PointFree

module PointFreeTests =

    [<Property>]
    let ``Все варианты func дают одинаковый результат`` (x: int) (l: int list) =
        let expected = multiplyList1 x l
        
        let r1 = multiplyList2 x l
        let r2 = multiplyList3 x l
        let r3 = multiplyList4 x l
        let r4 = multiplyList5 x l
        
        expected = r1 && r1 = r2 && r2 = r3 && r3 = r4