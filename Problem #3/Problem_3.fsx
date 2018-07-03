(*
This problem was asked by Google.
Given the root to a binary tree, implement serialize(root), which serializes the tree into a string, and deserialize(s), which deserializes the string back into the tree.
*)

type 'a tree = 
    | EmptyTree
    | TreeNode of 'a * 'a tree * 'a tree
  

let myTree = TreeNode(2,TreeNode(1,EmptyTree,EmptyTree),TreeNode(3,EmptyTree,EmptyTree))

let rec serialize state root = 
    match root with
    | EmptyTree -> state
    | TreeNode(v,a,b) -> (serialize state a) @ [v] @ (serialize state b)

serialize [] myTree