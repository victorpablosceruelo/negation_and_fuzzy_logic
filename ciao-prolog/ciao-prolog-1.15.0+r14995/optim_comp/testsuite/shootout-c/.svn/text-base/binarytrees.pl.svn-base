:- module(_, [], ['$purest']).
:- include(.(include(common))).

% TODO: the ImProlog is 2% slower because of the '-fno-string-aliasing' option

:- '$native_weak_inline'(include('engine/binarytrees.native.h')).
:- '$native_weak_inline'(include('malloc.h')).

:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).

:- '$improlog_begin'.

:- lowtype(treeNode).
:- class treeNode {
  :- struct.
  :- mut left :: ref1(treeNode).
  :- mut right :: ref1(treeNode).
  :- mut item :: intmach.
}.

:- pred newTreeNode/4 + lowentryfun([ref1(treeNode), ref1(treeNode), intmach], ref1(treeNode), 'NewTreeNode').
newTreeNode(Left, Right, Item, R) :-
	New = ~'$alloc'(malloc, treeNode),
	New.left <- Left,
	New.right <- Right,
	New.item <- Item,
	R = New.

:- pred itemCheck/2 + lowentryfun([ref1(treeNode)], intmach, 'ItemCheck').
itemCheck(Tree, R) :-
	( @Tree.left == ~'$null_ref1'(treeNode) ->
	    R = @Tree.item
	; R1 = @Tree.item,
	  itemCheck(@Tree.left, R2),
	  itemCheck(@Tree.right, R3),
	  R = R1 + R2 - R3
	).

:- pred bottomUpTree/3 + lowentryfun([intmach, intmach], ref1(treeNode), 'BottomUpTree').
bottomUpTree(Item, Depth, R) :-
	( Depth > 0 ->
	    bottomUpTree(2 * Item - 1, Depth - 1, Left),
            bottomUpTree(2 * Item, Depth - 1, Right),
	    newTreeNode(Left, Right, Item, R)
	; newTreeNode(~'$null_ref1'(treeNode), ~'$null_ref1'(treeNode), Item, R)
	).

:- pred deleteTree/1 + lowentry(det, [ref1(treeNode)], 'DeleteTree').
deleteTree(Tree) :-
	( @Tree.left \== ~'$null_ref1'(treeNode) ->
	    deleteTree(@Tree.left),
	    % TODO: do not check if right node is empty?
	    deleteTree(@Tree.right)
	; true
	),
	'$dealloc'(malloc, Tree).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	% TODO: original program used unsigned integers when possible
	MinDepth = 4,

	MaxDepth = ~newmut(intmach),
	( MinDepth + 2 > N ->
	    MaxDepth <- MinDepth + 2
	; MaxDepth <- N
	),

	StretchDepth = @MaxDepth + 1,

	bottomUpTree(0, StretchDepth, StretchTree),
	itemCheck(StretchTree, X),
	printf3("stretch tree of depth %d\t check: %d\n", StretchDepth, X),
	deleteTree(StretchTree),

	bottomUpTree(0, @MaxDepth, LongLivedTree),

	'$for_each'(Depth, ~intrangeclosedstep(MinDepth,@MaxDepth,2), (
          Iterations = 1 << (@MaxDepth - @Depth + MinDepth),
	  Check = ~initmut(intmach, 0),
	  '$for_each'(I, ~intrangeclosed(1, Iterations), (
            bottomUpTree(@I, @Depth, Tree1),
	    itemCheck(Tree1, Tree1Check),
            Check <- @Check + Tree1Check,
            deleteTree(Tree1),
	    %
            bottomUpTree(-(@I), @Depth, Tree2),
	    itemCheck(Tree2, Tree2Check),
            Check <- @Check + Tree2Check,
            deleteTree(Tree2)
          )),
	  printf4("%d\t trees of depth %d\t check: %d\n",
	          Iterations * 2,
		  @Depth,
		  @Check)
        )),
	%  
        itemCheck(LongLivedTree, LongLivedTreeCheck),
	printf3("long lived tree of depth %d\t check: %d\n", @MaxDepth, LongLivedTreeCheck).
:- '$improlog_end'.

