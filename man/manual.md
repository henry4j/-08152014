##### [Algorithm Design Manual](http://www8.cs.umu.se/kurser/TDBAfl/VT06/algorithms/BOOK/BOOK3/NODE131.HTM), [Algorithm Repository](http://www.cs.sunysb.edu/~algorith/), and [Math Symbols](http://math.typeit.org/)

* problems: bandwidth minimization, and FSA optimization.
* http://www.topcoder.com/tc?module=Static&d1=tutorials&d2=alg_index
* lecture http://cs.utsa.edu/~dj/ut/utsa/cs3343/

##### intro. to algorithm vs. heuristics

* the quest for an efficient algorithm to solve optimal robot tour problem is called the **traveling salesman problem (TSP)**.
* there is a fundamental difference between **algorithms**, which always produce the desired result, and 
  * **heuristics**, which may usually do a good job but without providing any guarantee.
* exhaustive job scheduling has to enumerate 2<sup>n</sup> subsets of n things while TSP needs enumerating all n! orders of n things.
  * the optimal job scheduling is accepting jobs from intervals with earliest completion dates.
* seeking counter-examples that break pretender algorithms is an important part of the algorithm design process.
  * counter-examples (against greedy heuristics) bubble up out of extremes (huge, tiny, left, right, few, many, near, and far).
* algorithms should be expressed into **clear ideas** at the heart rather than ill-defined ideas with pseudo code that looks a bit formal.
* common traps in specifying the input and output of a problem are **ill-defined questions**, and **compound goals**.
  * best route between two places? shortest path that doesn't use more than 2x as many turns as necessary?
* modeling a program in terms of well-defined structures and algorithms is the most important single step towards a solution.
  * modeling a problem is describing it in terms of procedures on rigorously defined abstract structures such as combinatorial objects (permutations, subsets, trees, graphs, points, polygons, and strings), and recursive objects.
  * recursive descriptions of objects requires both decomposition rules and basis cases. Side-note: The decision of whether the basis case contains zero or one element is more a question of taste and convenience than any fundamental principle.
* the Big Oh notation and worst-case analysis are tools that greatly simplify our ability to compare the efficiency of algorithms.
* time and space complexities: constant, logarithmic (sublinear), linear, superlinear, quadratic, cubic, exponential, and factorial.
* advantages of contiguously-allocated arrays:
  * contant-time access, space efficiency, and locality of memory access.
* miracle of dynamic arrays efficiently enlarges arrays as needed; each of the elements move only two times on average, so the total work of managing the dynamic arrays is the same O(n) as it would have been if a single array of sufficient size had been allocated in advance!
* dictionary operations: search, insert, delete, maximum, minimum, predecessor, and successor.
* **sum(1..n) = n * (n+1) / 2**; arithmetic series: **sum(i<sup>p</sup>) = Θ(n<sup>p+1</sup>)**, geometric series: **sum(a<sup>i</sup>) = (a<sup>n+1</sup>-1)/(a-1)**

##### 3. Data Structures

* data structures can be neatly classified as either contiguous or linked, depending upon whether they are based on arrays or pointers.
* dynamic array vs. linked list as cache-unfriendly d.s. due to poor locality of references
  * dynamic array has constant-time insertion and removal at the end, constant time access to random elements, and extra space reserved, but unused.
  * dynamic array takes log<sub>2</sub>n doublings until it grows to have n positions; 0..log n  Σ 2<sup>i</sup> <= 2n.
  * linked list has constant time insertion and removal of elements, linear access to elements, and extra space for references.
* stack (LIFO) and queus (FIFO) can be effectively implemented using either arrays or linked lists. the key issue is whether an upper bound on the size of the container is known in advance, thus permitting the use of a statically-allocated array.
* dictionary permits access to data items by key values or content and has basic operations such as put, get, remove, and additional operations such as minimum, maximum, predecessor, and successor.
* dictionary implementations: arrays, linked lists, binary search trees, and hash tables.
  * dictionary design questions: initial capacity & load factor, relative frequencies of basic operations, skewed or clustered patterns of key accesses, and fast invidual operations or the minimum total effort.
  * hashmap design questions: cf. open-addressing vs. chaining, 
     * Java's HashMap class has **0.75** default load factor threshold for table expansion ([dynamic resizing](http://en.wikipedia.org/wiki/Hash_table#Dynamic_resizing)).
     * how do you deal with collisions? Open addressing can lead to more concise tables with better cache performance than bucketing, but performance will be more brittle as the load factor starts to get high.
     * how big should the hash table be? With bucketing, m should be about the same as the maximum number of items you expect to put in the table. With open addressing, make it (say) 30% larger or more.
* BST operations: search, insert, delete, find minimum & maximum, traverse in in-order, pre-order, and post-order.
* balanced search tree implementations: red-black trees, and splay trees.
* how to sort in O(<i>n log n</i>)
  * build a search tree and traverse it in-order.
  * find the minimum, and the successor til the end.
  * find, yield, and delete the minimum til the end.
* priority queue has basic operations such as insert, find-minimum, or -maximum, and delete-minimum, or -maximum.
* building algorithms around data structures such as dictionaries (hashtable, hashmap, treemap) and priority queues (binary heap, Fibonacci heap) leads to both clean structure and good performance.
* String.hashCode method: |S|.times { |i| Σ **α** <sup>|S| - (i + 1)</sup> x char(S<sub>i</sub>) }
* efficient string matching via hashing, H(S, j+1) = **α**(H(S, j) - **α**<sup>m - 1</sup> char(s<sub>j</sub>)) + char(s<sub>j+m</sub>)    
* duplicate detection via hashing: is a document different from all the rest in a large corpus? is a part of a document plagiarized from a document in a large corpus? how can I convince you that a file isn’t changed?
* specialized data structures: suffix trees & arrays, kd-trees, adjacency matrix & list, and set for union find.

##### 4. Sorting

* heapsort competes with quicksort, another very efficient nearly-in-place sort algorithm
  * quicksort is typically somewhat faster due to **better cache behavior** and other factors.
  * the worst-case running time for quicksort is makes it unacceptable for **large data sets**.
* heapsort relies strongly on efficient random access due to its poor locality of reference.
* mergesort is a stable sort, unlike quicksort and heapsort, and can be easily adapted
  * to operate on linked lists and very large set on slow media with long access time.
* [heapsort](http://en.wikipedia.org/wiki/Heapsort#Comparison_with_other_sorts) requires constant extra space, whereas mergesort requires O(n) extra space.
  * quicksort with in-place partitioning and tail recursion runs in only O(log n) space.
* Q: find if two sets of size m and n are disjoint. O((<i>n + m</i>) <i>log n</i>) by sort and scan.

##### 5. Graph Traversal -- <sub>[trees are connected, acyclic, undirected graph](http://en.wikipedia.org/wiki/Tree_%28graph_theory%29)</sub>

** efficient and correct search algorithms must take us through every edge and vertex in a graph.

###### BFS

* for undirected graphs, **non-tree edges** can only link to vertices on the same level as the parent, or vertices on the level directly below the parent.
* for directed graphs, **back-pointing edge (u, v)** can exist when v lies closer to the root than u does.

```ruby
def self.bfs(v, edges, enter_v_iff = nil, exit_v = nil, cross_e = nil)
  q = Queue.new
  q.enq(v)
  until q.empty?
    v = q.deq
    if enter_v_iff.nil? || enter_v_iff.call(v)
      (edges[v] or []).each do |e|
        cross_e and cross_e.call(v, e)
        q.enq(e.y)
      end
      exit_v and exit_v.call(v)
    end
  end
end
```

###### BFS applications such as connected components, and two-colorable?

* **connected components** are peices of a graph such that there is no connection between pieces.
  * starting from one vertex, anything discovered by BFS/DFS becomes part of the same connected component.
  * repeat the search from any undiscovered vertex to define more components until everything is discovered.

##### DFS

* DFS organizes vertices by entry and exit times, and edges into tree and back edges.
* the interval of entry and exit times of vertex v must be nested within ancestor a as we must exit v before a.
  * half the difference between entry and exit times denotes the number of descendants of a vertex.
  * particularly useful for topological sorting and biconnected/<b>strongly-connected & weakly-connected</b> components.
* tree edges vs. back edges of which endpoint is an ancestor of the vertext being expanded as it back-points into the tree.
  * this back edge can't go to a sibling or a cousin instead of an ancestor, as all reachable nodes are expanded before we exit a vertex.

###### DFS applications

* has_cycle? by process_edge(u, v) { raise "This graph has a cycle." unless parents[u] == y }
* find_cut_nodes -- keep track of earlest reachable ancestors by back edges and find cases of root, bridge, and parent cut-nodes.

```ruby
def self.dfs(v, edges, enter_v_iff = nil, exit_v = nil, cross_e = nil)
  if enter_v_iff.nil? || enter_v_iff.call(v)
    (edges[v] or []).each do |e|
      cross_e.call(v, e) if cross_e
      dfs(e.y, edges, enter_v_iff, exit_v, cross_e)
    end
    exit_v.call(v) if exit_v
  end
end
```

###### DFS on directed graphs

* topological sort on DAGs defines a legal schedule given precedence constraints of jobs, as DFS pushes them into a stack of processed vertices.

###### Connectivity on undirected and directed graphs <sub>at the heart of graph applications either DFS/BFS</sub>

* **weakly-connected** if all vertices are reachable when all edges are turned into undirected edges.
* **strongly-contected** if all vertices are reachable from v, and the same is true after reversing all the edges.
* we find strongly-connected components by **reducing vertices on a directed cycle** down to a single vertex representing a component until there is no cycle.
* two-DFS idea for finding strongly-connected components is to do DFS to number vertices in post-vertex processing, and continuouly do DFS on the reversed graph from the highest numbered unvisited vertices.
* is a graph a tree? how to find a cycle if one exists? deadlocked?
  * it is a tree if the graph is connected and has n - 1 edges for n vertices.
  * back edges and DFS trees together define directed cycles; no other such cycle can exist in directed graphs.

##### Weighted Graphs and [MST](http://www.ics.uci.edu/~eppstein//161/960206.html)

* **Dynamic graph algorithms** seek to maintain an graph invariant (the MST, or others) efficiently (in amortized P time) upon edge insertions or deletions.
* Greedy [minimal spanning tree](http://en.wikipedia.org/wiki/Minimum_spanning_tree) algorithms
  * [Prim's algorithm](http://en.wikipedia.org/wiki/Prim's_algorithm) continuously increase the size of a tree, one edge at a time starting with a tree consisting of a single vertex until it spans all vertices; time complexity: O(V<sup>2</sup>) using linear look-up. O(E + V log V) using Fibonacci heap as a priority queue.
  * [Kruskal's algorithm](http://en.wikipedia.org/wiki/Kruskal%27s_algorithm) continuouly merge connected components in O(E log E) time.
* MST applications
  * Maximum spanning trees - negate the weights of all edges and run MST algorithms.
  * Minimum product spanning trees - apply logarithms to edge weights and run MST algorithms.
  * Minimum bottleneck spanning trees - MST has this property, while a naive approach is to delete heavy edges and see if it is still connected by BFS/DFS.
* non-MST variants
  * Minimum Steiner trees
  * Lowest-degree spanning tree (lowest max-degree tree), NP as a spanning tree w/ max-degree 2 is identical to a Hamiltonian path.

* Shortest path algorithms
  * BFS finds single source shortest paths on unweighted graphs -- simpler and faster.
  * greedy [Dijkstra's algorithm](http://en.wikipedia.org/wiki/Dijkstra's_algorithm) finds single source shortest paths on positively weighted graphs.
     * greedily selects unvisited minimum vertices to relax, whereas it runs in **O(V * extractMinsFromQ + E * decreaseKeysOnQ)** time.
  * greedy [Bellman-Ford algorithm](http://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm) finds single source shortest paths on graphs with negative edge weights.
     * relaxes all the edges and does this **V - 1** times so it runs in **O(VE)**.
  * greedy [Johnson's algorithm](http://en.wikipedia.org/wiki/Johnson's_algorithm) finds all pair shortest paths.
     * runs in **O(VE)** time for Bellman-Ford stage of the algorithm, and **O(V log V + E)** for each of V instantiations of Dijkstra's algorithm.
  * dynamic [Floyd-Wharshall algorithm](http://en.wikipedia.org/wiki/Floyd-Warshall_algorithm) runs in **O(V<sup>3</sup>)** time and **O(V<sup>2</sup>)** space.
     * **n.times {|k|** n.times {|i| n.times {|j| distances[i][j] = **[** distances[i][j], distances[i][k] + distances[k][j] **].min** } } **}**

* Shortest path applications -- best route in transportation, or communications.
  * in image segmentation, a grid of pixels can be modeled as a graph with the cost of an edge reflecting the color transitions between neighboring pixels.
  * in speech recognition, a string of recognized sounds can be mapped into possible word interpretations w/ the cost of an edge reflecting the likelihood of transitions.
  * in an informative graph visualization, a center vertex can be defined from shortest path or distance between all pairs of vertices.
  * in a DAG, topological-sort vertices by DFS and process them from left to right in a DP. d(s, j) = min <sub>(x, i) ∈ E</sub>  **d(s, i) + w(i, j)**
  * the shortest simple cycle can be found with all pair shortest paths in P time whereas the longest simple cycle or Hamiltonian cycle is NP-complete.
* Q: how to design natural routes for video-game characters to follow through an obstacle-filled room.
  * A: presumably, the desired route should look like a path that an intelligent being would choose. Since intelligent beings are either lazy, or efficient, this should be modeled as a shortest path problem. One approach is to lay a grid of points in the room. Create a vertex for each grid point that is a valid place for the character to stand; i.e., that does not lie within an obstacle. There will be an edge between any pair of nearby vertices, weighted proportionally to the distance between them.

***

###### Network Flows over Bipartite Matching (work assignments, marriages)

* the network-flow problem asks for the maximum amount of flow from vetices s to t given a weighted graph G.
* for each edge(i, j) in graph G with capacity c(j, j), **residual flow graph R** may contain two edges of flow(i, j), and flow(j, i).
* traditionally, network-flow algorithm continuously finds augmenting paths until it becomes optimal w/ the global maximum flow.
* a variety of practical linear programming problems can be modeled as network-flow problems and solved more efficiently than general-purpose linear programming methods.
* primary classes of problems
  * maximum flow: |V|.times {|i| sum x<sub>it</sub> }
  * minimum cost flow: |V|.times {|i| sum d<sub>it</sub> * x<sub>it</sub> }
* primary classes of algorithms are augmenting path, and preflow-push methods.

###### [A*](http://en.wikipedia.org/wiki/A*_search_algorithm) and [alpha-beta pruning](http://en.wikipedia.org/wiki/Alpha-beta_pruning)
* distance-plus-cost heuristic function := path-cost function g(x) + admissible heuristic estimate h(x).
* a generalization of Dijkstra's algorithm that cuts down on the size of the subgraph that must be explored, if additional information is available that provides a lower bound on the "distance" to the target.

***

##### Combinatorial Search & Heuristics

###### [Backtracking](http://en.wikipedia.org/wiki/Backtracking)
* let's model our combinatorial search state as a vector a = (a1, a2, ..., an) where each element a(i) is selected from a finite ordered set S(i).
  * such a vector might represent an arrangement where a(i) contains the i-th element of the permutation.
  * or, the vector might represent a given subset S, where a(i) is true if the i-th element of the universe is in S.
  * or, the vector might represent a sequence of moves in a game, or a path in a graph, where a(i) contains the i-th event in the sequence.

```ruby
def self.backtrack(candidate, expand_out, reduce_off)
  unless reduce_off.call(candidate)
    expand_out.call(candidate).each do |e|
      candidate.push e
      backtrack(candidate, expand_out, reduce_off)
      candidate.pop
    end
  end
end
```

###### [Dynamic Programming](http://en.wikipedia.org/wiki/Dynamic_programming) through **optimal substructure** and **overlapping subproblem**
* DP is a method for solving complex problems by breaking them down into simpler subproblems, and is applicable to problems exhibiting the properties of everlapping subproblems.
* top-down DP simply means storing, or memo-ing calculations for subproblems, while bottom-up DP involves formulating a complex calculation as a recursive series of simpler calculations.
* **greedy algorithms** that make the best local decision at each step are typically efficient but usually do not guarantee global optimality.
* exhaustive/brute-force **combinatorial search** algorithms always produce the **optimal** result usually **at a prohibitive runtime cost**.
* unbounded knapsack: `m(W) = weights.each_index.map { |i| v[i] + m(W-w[i]) } ].max`
* 0/1 knapsack: `m(i, W) = [ m(i-1, W), v[i] + m(i-1, W-w[i]) ].max`
* coin change: `coins(k) = denominations.map { [d] + coins(k-d) }.min_by { |a| a.size }`

###### TSP <sub>the most notorious NP-complete</sub>

* **find the minimum cost cyclic tour of visiting each vertex of a weighted graph G exactly once.**
* find the most efficient route for a robot arm to solder all the connections on a circuit board.
* flavors: 
  * unweighted like a Hamiltonian cycle? triangle inequality unsatisfied? given n geo-points?
  * vertices to be visited more than once? asymmetric distance exists? seeking optimal tour?
* optimal tours (NP): cutting-plane algorithms of an integer program, or **branch-and-bound algorithms of a combinatorial search**.
* heuristic tours (P):
  * DFS on MST finds a tour that walks over each of n - 1 edges twice -- at most 2x, typically 15% to 20% over optimal.
  * Incremental insertion (of furthest point): max<sub>v ∈ V</sub> |T|.times {|i| min(d(v, v<sub>i</sub>) + d(v, v<sub>i+1</sub>)) } -- typically 5% - 10% over optimal.
  * K-optimal tour - continuously refines an initial arbitrary tour by re-wiring K edges. 3-opt tours are within a few % over optimal.

###### Hamiltonian Cycle <sub>NP-complete</sub>

* **find a cyclic tour of visiting each vertex of an unweighted graph G exactly once.**
* backtrack on a combinatorial search w/ pruning if we really need to find a Hamiltonian cycle (NP).
* approximable?
  * vertices to be visited more than once? seeking the longest/shortest path in a DAG (page 489)?
  * supposed to visit all the vertices, or edges? Hamiltonian cycle (NP) vs. **Eulerian cycle (P)**?
  * bi-connected w/o articulation points? Hamiltonian cycle must be bi-connected w/ back chains.

***

* Heuristic Circuit Board Placement <sub>multi-criterion optimization problems</sub>
  * to place integrated circuit modules w/ dimensions & wires on a circuit board?
  * while minimizing the area or aspect ratio & the total or longest wire length
     * with a constraint that no two rectangles overlap each other in position.
  * reasonable transitions in the state space are moving or swapping rectangles in position.

* Bin Packing (NP) heuristics <sub>-- first-fit, tightest-fit, loosest-fit, or random-fit</sub>
  * how to store a set of n items using a set of m bins with capacity c<sub>1</sub>, ..., c<sub>m</sub>.
  * bin packing arises in a variety of packaging and manufactoring problems to minimize cost and waste.
     * cutting stock of metal sheets, or cloth, fitting boxes into truckes, constraints on orientation and placements.
  * **first-fit decreasing**(the best heuristic) runs in **O(n _log_ n + bn)**, or **O(n _log_ n)** where b <= min(n, m) is the # of bins in actual use.

* Motion Planning (?)

***

##### How to design algorithms <sub>strategy -- tactics</sub>

* strategy of algorithm design: model an app as a graph algorithm problem?
  * tactics: represent a graph as an adjacency list or adjacency matrix.
* specify problem in functions and non-functions: 
  * what are exact input and output (desired results)?
  * what are small examples of input to solve by hand?
  * **how big or small** typical instances of the problem are?
  * whether answers **should be optimal**, or can be **sub-optimal**?
  * what are tradeoffs between time and space, or speed and memory.
  * care to experiment a couple of approaches and choose what's best.
  * which formulation seems easiest? a graph, a string, a set, or a geometric?
* seek an algorithm (brute-force, P, NP), or a heuristic?
  * brute-force runs in P, or NP times, while it's simple and correct?
  * brute-force will suffice, as the problem instance is small enough?
  * heuristic might be first-fit-decreasing, tightest-, loosest-, or random-fit.
     * greedy best local decision at each step w/ no guarantee of global optimality.
     * examples of input on which heuristic works well and badly. always well?
* look up algorithm catalog & design paradigms
  * special-case, or generalize algorithms, or simplify the problem?
  * order a set of items by size or some key that eases the problem?
  * split into two? partition into big and small, or left and right?
  * exploit natural left-to-right order (DP) in input or desired results?
  * trade memory for speed, e.g. a dictionary/hashmap, a heap/prio. queue.
  * direct randomness (simulated annealing) to zoom in on the best answer.
  * try to formulate the problem as a linear program or integer program?
  * try to reduce to satisfiability, TSP, or [other NP-complete problems](http://en.wikipedia.org/wiki/List_of_NP-complete_problems#Flow_problems)?

***

##### P and NP problems

* P can be though of as an exclusive clue for algorithmic problems that a problem can only join after demonstraing that there exists a polynomial-time algorithm to solve it. e.g. MST, and movie scheduling problem.
* NP (not necessarily polynomial time) is a less-exclusive club that welcomes all algorithmic problems whose solution can be verified in polynomial time.
* NP-hard vs. NP-complete problems
  * a problem is NP-hard if like satisfiability it is at least as hard as any problem in NP. 
  * a problem is NP-complete if it is NP-hard and also in NP itself.
  * all the NP-hard problems in the textbook are NP-complete.
* two player games such as chess are examples of problems that are even harder than NP, and not in NP.
* dealing w/ NP-complete problems
  * algorithms fast in the average case, such as backtracking with substantial pruning.
  * heuristic methods such as greedy approaches without guarantee of the best solution.
  * approx. algorithms such as clever, problem-specific heuristics close to the optimal answer.
* ways to get the best of approx. algorithms w/ guarantee and heuristics to run both of them on given problem instances, and pick the solution giving a better result. This way you get a solution that comes with a guarantee and a second chance to do even better.

***

##### Suffix Trees and Arrays

* a suffix tree is a _trie_ of the _n_ suffixes of an n-character string -- often a hero in O(n) time string processing.
* a trie is a tree data structure where each edge represents one character, and the root represents the null string.
* a trie is useful for testing whether a query string q is in the set by traversing from the root by successive chars of q.
* suffix tree applications
  * find all occurrences of q as a substring of S --- walk to node(q) associated with q and then DFS to identify descendants.
  * find longest substring common to a set of strings --- build a collapsed suffix tree of all strings, and then DFS to label each node with both the length of common prefix and the number of distinct strings.
  * find longest palindrome in S --- build a suffix tree of all suffices of S and the reversal of S with each leaf identified by a starting position; a palindrome is identified by a node with forward and reversed children toward the same position.

##### Partitions

Partitioning: Find all, or a random, or the next integer, or set partitions of length n.

- to generate integer partitions by subtracting 1 from the smallest part that is greater than 1 and collecting all 1's to match the new smallest part that is greater than 1.
- to generate set partitions according to the lexicographic order of the restricted growth functions.

15 distinct set partitions of n = 4:

{1234}, {123,4}, {124,3}, {12,34}, 
{12,3,4}, {134,2}, {13,24}, {13,2,4}, 
{14,23}, {1,234}, {1,23,4}, {14,2,3},
{1,24,3}, {1,2,34}, and {1,2,3,4}.

Each set partition is encoded with a restricted growth string,
where a(1) = 0, a(i) <= 1 + max(a(1) .. a(i) for i = 2 .. n.

0000, 0001, 0010, 0011
0012, 0100, 0101, 0102
0110, 0111, 0112, 0120
0121, 0122, 0123

##### approx. string matching

* offering suggestions for misspelled words, searching similar sequences on large databases of DNA sequences, and evaluating OCR output.
* recurrence relation of edit distance, **D[i,j]** = **[** D[i-1,j-1] + substitution cost, D[i-1][j] + deletion cost of P_<sub>i</sub>_, D[i][j-1] + deletion cost of T_<sub>j</sub>_ **].min**

##### [union find](http://www.cs.gmu.edu/~rcarver/cs310/UnionFind.pdf)

Set to represent subsets so as to efficiently ① test whether u(i) ∈ S(j), ② compute the union, or intersection of S(i) and S(j), and ③ insert, or delete members of S. In mathematical terms, a set is an unordered collection of objects drawn from a fixed universal set. But, it is usually useful for implementation to represent each set in a single, canonical order to speed up, or simplify various operations. 

Primary alternatives for representing a subset
* Bit vectors - element insertion and deletion simply flips a bit, intersection, and union are done by and-ing, or or-ing bits, The only drawback is that it takes O(n) time explicitly identify all members of sparse (near empty) subsets.
* Containers, or dictionaries - for sparse subsets, containers can be more time and space efficient than bit vectors; it pays to keep elements sorted, so a linear time traversal through both subsets identifies all duplicates.
* Bloom filters use different hash functions H(1), ... H(k), and set all k bits Hi(e) upon insertion of key e. The probability of false positives can be made arbitrarily low by increasing the number of hash functions k and table size n.

Set partition to maintain changes over time by ① changing elements ② merging two sets, or ③ breaking a set apart.
* Collections of containers
* Dictionary with subset attributes
* Vector with subsets denoted on elements
* Union-find data structure --- Union find is a fast, simple data structure that every programmer should know about. It does not support breaking up subsets created by unions, but usually this is not an issue.

##### edge coloring

* edge coloring can be reduced to vertex coloring with **line graph transformation**.
* for interview loops, we schedule non-conflicting interviews simultaneously where vertices are people (interviewer & interviewee) and edges are interviews. edge coloring defines schedule where color classes (partitioning) represent one hour time periods in the schedule.

##### vertex coloring

* applications: register allocation: each vertex corresponds to a variable with an edge between any two vertices whose variable life spans intersect.
* discussions
  * **chromatic number** is the smallest number of colors sufficient to vertex-color a graph.
  * two colorable, or bipartite? color one vertex and continue color a child opposite of its parent through the DFS, until we ever find an edge (x, y) where both x and y are colored identically.
  * three, or four-colorabe planar graph? efficient four-coloring algorithms are known, but **three-colorability** test is NP.
* heuristic incremental methods
  * experience suggests inserting vertices in non-increasing order of degree, since high-degree vertices have more color constraints and so are most likely to require an additional color if inserted late.
  * Brelaz's heuristic dynamically selects the uncolored vertex of highest color degree and colors it with the lowest-numbered unused color.
* color interchange is a win where we delete all but red and blue vertices and recolor connected components to produce better colorings.

##### job scheduling

* job scheduling: What schedule of tasks completes the job using the minimal amount of time and processors?
* scheduling constraints
  * topological sort can construct a schedule consistent with the precedence constraints.
  * bipartite matching can assign a set of tasks to workers who have appropriate skills.
  * vertex and edge coloring can assign a set of tasks to time slots such that no two interfering jobs are assigned the same time slot.
  * traveling salesman can construct the most efficient route for a delivery person to visit a given set of locations.
  * eulerian cycle can construct the most efficient route for a snowplow or mailman to completely traverse a given set of edges.
* problems:
  * precedence constrained scheduling problems
  * the critical path is the longest path from the start vertex to the completion vertex.
  * the minimum completion time is defined by the critical path.
* job-shop scheduling where each job is assigned the number of hours that it takes to complete, and each machine is represented by a bin with space equal to the number of hours a day.
