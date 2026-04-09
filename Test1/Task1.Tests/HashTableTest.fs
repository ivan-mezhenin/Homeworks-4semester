module HashTableTest

open NUnit.Framework
open FsUnit
open HashTableImplementation

let hashInt (x: int) = x

[<Test>]
let ``Add increases count`` () =
    let ht = HashTable<int>(10, hashInt)
    
    ht.Add 5
    ht.Add 10
    ht.Add 15
    
    ht.Count |> should equal 3

[<Test>]
let ``Contains returns true for existing element`` () =
    let ht = HashTable<int>(10, hashInt)
    
    ht.Add 42
    ht.Add 100
    
    ht.Contains 42  |> should be True
    ht.Contains 100 |> should be True

[<Test>]
let ``Contains returns false for non-existing element`` () =
    let ht = HashTable<int>(10, hashInt)
    
    ht.Add 1
    ht.Add 2
    
    ht.Contains 999 |> should be False

[<Test>]
let ``Remove returns true and decreases count when element exists`` () =
    let ht = HashTable<int>(10, hashInt)
    
    ht.Add 7
    ht.Add 14
    
    ht.Remove 7 |> should be True
    ht.Count |> should equal 1
    ht.Contains 7 |> should be False

[<Test>]
let ``Remove returns false when element does not exist`` () =
    let ht = HashTable<int>(10, hashInt)
    
    ht.Add 5
    
    ht.Remove 999 |> should be False
    ht.Count |> should equal 1

[<Test>]
let ``Adding duplicate element does not increase count`` () =
    let ht = HashTable<int>(10, hashInt)
    
    ht.Add 42
    ht.Add 42
    ht.Add 42
    
    ht.Count |> should equal 1

[<Test>]
let ``Clear removes all elements`` () =
    let ht = HashTable<int>(10, hashInt)
    
    ht.Add 1
    ht.Add 2
    ht.Add 3
    
    ht.Clear()
    
    ht.Count |> should equal 0
    ht.Contains 1 |> should be False
    ht.Contains 2 |> should be False

[<Test>]
let ``HashTable works correctly with strings`` () =
    let hashString (s: string) = s.Length
    let ht = HashTable<string>(8, hashString)
    
    ht.Add "apple"
    ht.Add "banana"
    ht.Add "cherry"
    
    ht.Contains "apple"  |> should be True
    ht.Contains "banana" |> should be True
    ht.Contains "grape"  |> should be False

[<Test>]
let ``HashTable handles negative hash values correctly`` () =
    let badHash (x: int) = x * (-5)
    let ht = HashTable<int>(10, badHash)
    
    ht.Add 10
    ht.Add 20
    ht.Add (-30)
    
    ht.Contains 10  |> should be True
    ht.Contains 20  |> should be True
    ht.Contains (-30) |> should be True