namespace MyProject.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open PointFree

module PointFreeTests =

    [<Property>]
    let ``Все варианты func дают одинаковый результат`` (x: int) (l: int list) =
        let expected = func1 x l
        
        let r1 = func2 x l
        let r2 = func3 x l
        let r3 = func4  x l
        
        expected = r1 && r1 = r2 && r2 = r3