## AST Utils
A set of tools for walking the compile-time AST to make metaprogramming easier.

See the [demo build file](examples/build.jai) and [demo target code](examples/astTests.jai) for an example of how to use it.

#### `unwrapTypechecked :: (typechecked: []Typechecked($T)) -> []*T`
The compiler metaprogramming message loop message in the `.TYPECHECKED` phase carries `declarations`, `procedure_headers`, `procedure_bodies`, `structs` & `others`, which are a wrapped [subclass of] `Code_Node`.  The below utils need to start with a `*Code_Node`.  This unwraps them for you.

#### `hasNote :: (node: *$T/interface HasNote, noteName: string) -> bool`
A filter helper to check for nodes with a specific `Note`.

#### `hasNotePrefix :: (node: *$T/interface HasNote, notePrefix: string) -> bool`
Returns true if any `Note` attached to `node` begins with `notePrefix`.
Useful for matching groups of notes that share a common metadata prefix.

#### `hasNoteContaining :: (node: *$T/interface HasNote, noteContent: string) -> bool`
Returns true if any `Note` on `node` contains the substring `noteContent`.
This allows fuzzy or partial matching of note text.

#### `filenameIs :: (node: *Code_Node, filename: string) -> bool`
Checks whether the node originates from a file whose base name equals `filename`.
The comparison ignores directory components.

#### `pathContains :: (node: *Code_Node, needle: string) -> bool`
Returns true when the node's enclosing file path contains the path segment `needle`.
Handy for filtering nodes by module or directory names.

#### `filterASTNodes :: (nodes: []*$NodeType, predicate: (node: *$FilteredNodeType) -> bool) -> []*FilteredNodeType`
Filters `nodes`, returning only those of type `FilteredNodeType` for which `predicate` returns true.
The utility builds the mapping from node types to `NodeKind` as needed.

#### `transformAST :: (node: *$T/Code_Node, predicate: (node: *$FilteredNodeType) -> bool, transform: (node: *FilteredNodeType) -> *$TransformedNodeType, log := false) -> *T`
Walks the AST rooted at `node`, applying `transform` to each matching `FilteredNodeType` where `predicate` is true.
Non-matching nodes are left unchanged; the possibly-modified root is returned.
