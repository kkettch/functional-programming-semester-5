namespace RedBlackTreeTests

open Xunit
open FsCheck
open RbBag 

module RedBlackTreeTests =

    let testTree =
        Node { value = 10; color = Black; 
                left = Node { value = 5; color = Red; left = Empty; right = Empty }; 
                right = Node { value = 15; color = Red; left = Empty; right = Empty } }
    
    let sampleTree =
        Node { value = 10; color = Black; left = Node { value = 5; color = Red; left = Empty; right = Empty }; right = Node { value = 15; color = Red; left = Empty; right = Empty } }

    let isGreaterThanFive x = x > 5


    [<Fact>]
    let ``Filter should retain nodes greater than 5`` () =
        let filteredTree = filter isGreaterThanFive testTree
        Assert.True(contains filteredTree 10) 
        Assert.False(contains filteredTree 5) 
        Assert.True(contains filteredTree 15)

    [<Fact>]
    let ``Filter should return empty tree for all less than 5`` () =
        let filteredTree = filter (fun x -> x < 5) testTree
        Assert.Equal(Empty, filteredTree) 

    [<Fact>]
    let ``Filter should return same tree for all greater than 0`` () =
        let filteredTree = filter (fun _ -> true) testTree
        Assert.True(contains filteredTree 10) 
        Assert.True(contains filteredTree 5)
        Assert.True(contains filteredTree 15) 

    [<Fact>]
    let ``Filter should return empty tree for all greater than 20`` () =
        let filteredTree = filter (fun x -> x > 20) testTree
        Assert.Equal(Empty, filteredTree)

    [<Fact>]
    let ``Map should double the values in the tree`` () =
        let expected = 
            Node { value = 20; color = Black; left = Node { value = 10; color = Red; left = Empty; right = Empty }; right = Node { value = 30; color = Red; left = Empty; right = Empty } }
        
        let result = map ((*) 2) sampleTree
        Assert.Equal(expected, result)

    [<Fact>]
    let ``FoldLeft should sum the values in the tree`` () =
        let sumFunction acc value = acc + value
        let result = foldLeft sumFunction 0 sampleTree
        Assert.Equal(30, result)

    [<Fact>]
    let ``FoldRight should sum the values in the tree`` () =
        let sumFunction value acc = acc + value
        let result = foldRight sumFunction sampleTree 0
        Assert.Equal(30, result)

    [<Fact>]
    let ``isMember should return true for a value in the tree`` () =
        Assert.True(isMember sampleTree 10)

    [<Fact>]
    let ``isMember should return false for a value not in the tree`` () =
        Assert.False(isMember sampleTree 100)

    [<Fact>]
    let ``Delete an existing element from tree`` () =
        let tree = insertMany [1; 2; 3] empty
        let resultTree = delete 2 tree
        Assert.False(isMember resultTree 2)
        Assert.True(isMember resultTree 1)
        Assert.True(isMember resultTree 3)

    [<Fact>]
    let ``Delete non-existing element should not affect tree`` () =
        let tree = insertMany [1; 2; 3] empty
        let resultTree = delete 4 tree
        Assert.True(isMember resultTree 1)
        Assert.True(isMember resultTree 2)
        Assert.True(isMember resultTree 3)

    [<Fact>]
    let ``Delete root element from single-element tree`` () =
        let tree = insert 1 empty
        let resultTree = delete 1 tree
        Assert.Equal(resultTree, Empty)

    [<Fact>]
    let ``Delete leaf element should maintain balance`` () =
        let tree = insertMany [10; 20; 5; 15] empty
        let resultTree = delete 5 tree
        Assert.False(isMember resultTree 5)
        Assert.True(isMember resultTree 10)
        Assert.True(isMember resultTree 20)
        Assert.True(isMember resultTree 15)
    
    [<Fact>]
    let ``Delete an element and check if remaining elements are still accessible`` () =
        let tree = insertMany [10; 20; 5; 15; 8] empty
        let resultTree = delete 10 tree
        Assert.False(isMember resultTree 10)
        List.iter (fun x -> Assert.True(isMember resultTree x)) [5; 20; 15; 8]