[Algorithm Design Manual](http://www8.cs.umu.se/kurser/TDBAfl/VT06/algorithms/BOOK/BOOK3/NODE131.HTM), [Algorithm Repository](http://www.cs.sunysb.edu/~algorith/), and [Math Symbols](http://math.typeit.org/)

##### String Problems

Input: A text string `t` of length `n`. A pattern string `p` of length `m`.  
Problem: Find the first (or all) instances of pattern `p` in the text.  
Follow-ups: How to screen out dirty words from our text (i.e. multiple queries on the same text)? What if our text or pattern contains spelling errors? 

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/string-matching-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/string-matching-R.gif" /></td>
  </tr>
</table>

Input: A text string `t` and a pattern string `p`.  
Problem: What is the minimum-cost way to transform `t` to `p` using insertions, deletions, and substitutions?

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/approximate-pattern-matching-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/approximate-pattern-matching-R.gif" /></td>
  </tr>
</table>

Input: A set S of strings S<sub>1</sub>, ..., S<sub>n</sub>.  
Problem: What is the longest string S' such that all the characters of S' appear as a substring or subsequence of each S<sub>i</sub> (<i>1 ≤ i ≤ n</i>)?

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/longest-common-substring-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/longest-common-substring-R.gif" /></td>
  </tr>
</table>

Input: A set of strings S = {S<sub>1</sub>, ..., S<sub>m</sub>}.  
Problem: Find the shortest string S' that contains each string S<sub>i</sub> as a substring of S'.

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/shortest-common-superstring-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/shortest-common-superstring-R.gif" /></td>
  </tr>
</table>

Discussions
* String matching arises in almost all text-processing applications. Every text editor contains a mechanism to search the current document for arbitrary strings. Pattern-matching programming languages such as Perl and Python derive much of their power from their built-in string matching primitives, making it easy to fashion programs that filter and modify text. Spelling checkers scan an input text for words appearing in the dictionary and reject any strings that do not match.
  * Will you perform multiple queries on the same text? Suppose you are building a program to repeatedly search a particular text database, such as the Bible. Since the text remains fixed, it pays to build a data structure to speed up search queries --- the suffix tree and suffix array data structures.
* Approximate string matching is a fundamental problem because we live in an error-prone world. Spelling correction programs must be able to identify the closest match for any text string not found in a dictionary. By supporting efficient sequence similarity (homology) searches on large databases of DNA sequences, the computer program BLAST has revolutionized the study of molecular biology. Suppose you were interested in a particular gene in man, and discovered that it is similar to the hemoglobin gene in rats. Likely this new gene also produces hemoglobin, and any differences are the result of genetic mutations during evolution.
* Finding the shortest common superstring can easily be reduced to the traveling salesman problem. Create an overlap graph G where vertex v<sub>i</sub> represents string S<sub>i</sub>. Assign edge (v<sub>i</sub>, v<sub>j</sub>) weight equal to the length of S<sub>i</sub> minus the overlap of S<sub>j</sub> with S<sub>i</sub>. Thus, w(v<sub>i</sub>, v<sub>j</sub>) = 1 for S<sub>i</sub> = abc and S<sub>j</sub> = bcd. The minimum weight path visiting all the vertices defines the shortest common superstring. These edge weights are not symmetric; note that w(v<sub>j</sub>, v<sub>i</sub>) = 3 for the example above. Unfortunately, asymmetric TSP problems are much harder to solve in practice than symmetric instances.

##### Graph Problems (P and NP-hard)

Input: A graph G.  
Problem: Represent the graph G using a flexible, efficient data structure.  
Follow-ups: What algorithms favor which data structures? How about all-pair shortest path algorithms?  

--- excerpted from http://www.geeksforgeeks.org/graph-and-its-representations/
<table>
  <tr>
    <td><img src="http://www.geeksforgeeks.org/wp-content/uploads/graph_representation12.png" /></td>
    <td><img src="http://www.geeksforgeeks.org/wp-content/uploads/adjacency_matrix_representation.png" /></td>
    <td><img src="http://www.geeksforgeeks.org/wp-content/uploads/adjacency_list_representation.png" /></td>
  </tr>
</table>

Input: A directed acyclic graph G = (V,E), also known as a partial order (or poset in mathematics).  
Problem: Find a linear ordering of the vertices of V such that for each edge `e(i,j)` ∈ E, vertex i is above (or to the left of) vertex j.

<table>
  <tr>
    <td>DAG: <br><img width="180" height="160" src="http://upload.wikimedia.org/wikipedia/commons/0/08/Directed_acyclic_graph.png" /></td>
    <td>
      A legal schedule; a sequence of tasks based on dependencies (precedence constraints):<br>
      - 7, 5, 3, 11, 8, 2, 9, 10 (visual left-to-right, top-to-bottom)<br>
      - 3, 5, 7, 8, 11, 2, 9, 10 (smallest-numbered available vertex first)<br>
      - 3, 7, 8, 5, 11, 10, 2, 9<br>
      - 5, 7, 3, 8, 11, 10, 9, 2 (fewest edges first)<br>
      - 7, 5, 11, 3, 10, 8, 9, 2 (largest-numbered available vertex first)<br>
      - 7, 5, 11, 2, 3, 8, 9, 10
    </td>
  </tr>
</table>

Input: A directed ~~or undirected~~ graph G.  
Problem: Identify the different pieces or components of G, where vertices x and y are members of different components if no path exists from x to y in G.  

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/dfs-bfs-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/dfs-bfs-R.gif" /></td>
  </tr>
</table>

Input: A graph G = (V,E) with weighted edges.  
Problem: The minimum weight subset of edges E' ⊂ E that form a tree on V.

<table>
  <tr>
    <td align="center"><img src="http://www.cs.sunysb.edu/~algorith/files/minimum-spanning-tree-L.gif" /></td>
    <td align="center"><img src="http://www.cs.sunysb.edu/~algorith/files/minimum-spanning-tree-R.gif" /></td>
  </tr>
  <tr>
    <td><img width="300" height="139" src="http://www.geeksforgeeks.org/wp-content/uploads/Fig-11.jpg" /></td>
    <td><img src="http://www.geeksforgeeks.org/wp-content/uploads/MST5.jpg" /></td>
  </tr>
</table>

Input: An edge-weighted graph G.  
Problem: Find the shortest path from `s` to `t` in G.

<table>
  <tr>
    <td align="center"><img src="http://www.cs.sunysb.edu/~algorith/files/shortest-path-L.gif" /></td>
    <td align="center"><img src="http://www.cs.sunysb.edu/~algorith/files/shortest-path-R.gif" /></td>
  </tr>
  <tr>
    <td><img width="300" height="139" src="http://www.geeksforgeeks.org/wp-content/uploads/Fig-11.jpg" /></td>
    <td><img src="http://www.geeksforgeeks.org/wp-content/uploads/DIJ5.jpg" /></td>
  </tr>
</table>


Input: A directed graph G, where each edge `e(i,j)` has a capacity `c(i,j)`. A source node `s` and sink node `t`.  
Problem: What is the maximum flow you can route from `s` to `t` while respecting the capacity constraint of each edge?

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/network-flow-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/network-flow-R.gif" /></td>
  </tr>
</table>

Input: A (<del>weighted</del>) graph G = (V,E).  
Problem: Find the largest set of edges E' from E such that each vertex in V is incident to at most one edge of E'.

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/matching-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/matching-R.gif" /></td>
  </tr>
</table>

Input: A graph G = (V,E).  
Problem: Find the shortest tour visiting each edge of G at least once.

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/eulerian-cycle-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/eulerian-cycle-R.gif" /></td>
  </tr>
</table>

Input: A weighted graph G.  
Problem: Find the cycle of minimum cost visiting each vertex of G exactly once.

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/traveling-salesman-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/traveling-salesman-R.gif" /></td>
  </tr>
</table>

Input: A graph G = (V,E).  
Problem: Find a tour of the vertices using only edges from G, such that each vertex is visited exactly once.

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/hamiltonian-cycle-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/hamiltonian-cycle-R.gif" /></td>
  </tr>
</table>

Input description: A graph G = (V,E).  
Problem description: Color the vertices of V using the minimum number of colors such that i and j have different colors for all (i, j) ∈ E.

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/vertex-coloring-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/vertex-coloring-R.gif" /></td>
  </tr>
</table>

Input: A graph G = (V,E).  
Problem: What is the smallest set of colors needed to color the edges of G such that no two same-color edges share a common vertex?

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/edge-coloring-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/edge-coloring-R.gif" /></td>
  </tr>
</table>

Discussions
* The two basic data structures for representing graphs are adjacency matrices and adjacency lists.
  * Which algorithms will you be implementing?  
Certain algorithms are more natural on adjacency matrices (such as all-pairs shortest path) and others favor adjacency lists (such as most DFS-based algorithms). Adjacency matrices win for algorithms that repeatedly ask, "Is (i, j) in G?" However, most graph algorithms can be designed to eliminate such queries.
  * Will you be modifying the graph over the course of your application?  
Efficient static graph implementations can be used when no edge insertion/deletion operations will done following initial construction. Indeed, more common than modifying the topology of the graph is modifying the attributes of a vertex or edge of the graph, such as size, weight, label, or color. Attributes are best handled as extra fields in the vertex or edge records of adjacency lists.
* The connected components of a graph represent, in grossest terms, the pieces of the graph. Two vertices are in the same component of G if and only if there exists some path between them. Finding connected components is at the heart of many graph applications. For example, consider the problem of identifying natural clusters in a set of items. We represent each item by a vertex and add an edge between each pair of items deemed "similar." The connected components of this graph correspond to different classes of items. Testing whether a graph is connected is an essential preprocessing step for every graph algorithm. Subtle, hard-to-detect bugs often result when an algorithm is run only on one component of a disconnected graph.
* Topological sorting arises as a subproblem in most algorithms on directed acyclic graphs. Topological sorting orders the vertices and edges of a DAG in a simple and consistent way and hence plays the same role for DAGs that a depth-first search does for general graphs. Topological sorting can be used to schedule tasks under precedence constraints. Suppose we have a set of tasks to do, but certain tasks have to be performed before other tasks. These precedence constraints form a directed acyclic graph, and any topological sort (also known as a linear extension) defines an order to do these tasks such that each is performed only after all of its constraints are satisfied.  
* The minimum spanning tree (MST) of a graph defines the cheapest
subset of edges that keeps the graph in one connected component. Telephone
companies are interested in minimum spanning trees, because the MST of a set of
locations defines the wiring scheme that connects the sites using as little wire as
possible. MST is the mother of all network design problems.
Minimum spanning trees prove important for several reasons:
  * They can be computed quickly and easily, and create a sparse subgraph that
reflects a lot about the original graph.
  * They provide a way to identify clusters in sets of points. Deleting the long
edges from an MST leaves connected components that define natural clusters
in the data set, as shown in the output figure above.
  * They can be used to give approximate solutions to hard problems such as
Steiner tree and traveling salesman.
  * As an educational tool, MST algorithms provide graphic evidence that greedy
algorithms can give provably optimal solutions.
* Applications of network flow go far beyond plumbing. Finding the most cost-effective way to ship goods between a set of factories and a set of stores defines a network-flow problem, as do many resource-allocation problems in communications networks.  
The real power of network flow is (1) that a surprising variety of linear programming problems arising in practice can be modeled as network-flow problems, and (2) that network-flow algorithms can solve these problems much faster than general-purpose linear programming methods. Several graph problems can be solved using network flow, including bipartite matching, shortest path, and edge/vertex connectivity.
* Suppose we manage a group of workers, each of whom is capable of performing a subset of the tasks needed to complete a job. Construct a graph with vertices representing both the set of workers and the set of tasks. Edges link workers to the tasks they can perform. We must assign each task to a different worker so that no worker is overloaded. The desired assignment is the largest possible set of edges where no employee or job is repeated, i.e., a matching.  
Efficient algorithms for constructing matchings work by constructing augmenting paths in graphs. Given a (partial) matching M in a graph G, an augmenting path is a path of edges P that alternate (out-of-M, in-M, . . . , out-of-M). We can enlarge the matching by one edge given such an augmenting path, replacing the even-numbered edges of P from M with the odd-numbered edges of P. Berge’s theorem states that a matching is maximum if and only if it does not contain any augmenting path. Therefore, we can construct maximum-cardinality matchings by searching for augmenting paths and stopping when none exist.
* Suppose you are given the map of a city and charged with designing the routes for garbage trucks, snow plows, or postmen. In each of these applications, every road in the city must be completely traversed at least once in order to ensure that all deliveries or pickups are made. For efficiency, you seek to minimize total drive time, or (equivalently) the total distance or number of edges traversed.  
Alternately, consider a human-factors validation of telephone menu systems. Each “Press 4 for more information” option is properly interpreted as an edge between two vertices in a graph. Our tester seeks the most efficient way to walk over this graph and visit every link in the system at least once.
* The traveling salesman problem is the most notorious NP-complete problem. This is a function both of its general usefulness and the ease with which it can be explained to the public at large. Imagine a traveling salesman planning a car trip to visit a set of cities. What is the shortest route that will enable him to do so and return home, thus minimizing his total driving?
The traveling salesman problem arises in many transportation and routing problems. Other important applications involve optimizing tool paths for manufacturing equipment. For example, consider a robot arm assigned to solder all the connections on a printed circuit board. The shortest tour that visits each solder point exactly once defines the most efficient route for the robot.
* Finding a Hamiltonian cycle or path in a graph G is a special case of the traveling salesman problem G'—one where each edge in G has distance 1 in G'. Non-edge vertex pairs are separated by a greater distance, say 2. Such a weighted graph has TSP tour of cost n in G' iff G is Hamiltonian.  
Closely related is the problem of finding the longest path or cycle in a graph. This arises often in pattern recognition problems. Let the vertices in the graph correspond to possible symbols, with edges linking pairs of symbols that might occur next to each other. The longest path through this graph is a good candidate for the proper interpretation.
* Vertex coloring arises in many scheduling and clustering applications. Register allocation in compiler optimization is a canonical application of coloring. Each variable in a given program fragment has a range of times during which its value must be kept intact, in particular after it is initialized and before its final use. Any two variables whose life spans intersect cannot be placed in the same register. Construct a graph where each vertex corresponds to a variable, with an edge between any two vertices whose variable life spans intersect. Since none of the variables assigned the same color clash, they all can be assigned to the same register.  
No conflicts will occur if each vertex is colored using a distinct color. But computers have a limited number of registers, so we seek a coloring using the fewest colors. The smallest number of colors sufficient to vertex-color a graph is its chromatic number.
* The edge coloring of graphs arises in scheduling applications, typically associated with minimizing the number of noninterfering rounds needed to complete a given set of tasks. For example, consider a situation where we must schedule a given set of two-person interviews, where each interview takes one hour. All meetings could be scheduled to occur at distinct times to avoid conflicts, but it is less wasteful to schedule nonconflicting events simultaneously. We construct a graph whose vertices are people and whose edges represent the pairs of people who need to meet. An edge coloring of this graph defines the schedule. The color classes represent the different time periods in the schedule, with all meetings of the same color happening simultaneously.  
The National Football League solves such an edge-coloring problem each season to make up its schedule. Each team’s opponents are determined by the records of the previous season. Assigning the opponents to weeks of the season is an edge-coloring
problem, complicated by extra constraints of spacing out rematches and making sure that there is a good game every Monday night.  
The minimum number of colors needed to edge color a graph is called its edge-chromatic number by some and its chromatic index by others. Note that an evenlength cycle can be edge-colored with 2 colors, while odd-length cycles have an edge-chromatic number of 3.

##### Geometric Problems

Input: A set S of n points or more complicated geometric objects in k dimensions.  
Problem description: Construct a tree that partitions space by half-planes such that each object is contained in its own box-shaped region.

Input: A set S of n points in d dimensions; a query point q.  
Problem: k nearest neighbors to a query point q in ~~O(k + log n)~~ time?

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/nearest-neighbor-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/nearest-neighbor-R.gif" /></td>
  </tr>
</table>

Input: A set of n items with sizes d<sub>1</sub>, ..., d<sub>n</sub>. A set of m bins with capacity c<sub>1</sub>, ..., c<sub>m</sub>.  
Problem: Store all the items using the smallest number of bins.

<table>
  <tr>
    <td><img src="http://ars.els-cdn.com/content/image/1-s2.0-S0377221711005078-gr1.jpg" /></td>
  </tr>
</table>

Input: A point p and line segment `l`, or two line segments `l1`, `l2`.  
Problem: Does p lie over, under, or on `l` ? Does `l1` intersect `l2`?

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/geometric-primitives-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/geometric-primitives-R.gif" /></td>
  </tr>
</table>

Discussions
* Kd-tree and related spatial data structures hierarchically decompose space into a small number of cells, each containing a few representatives from an input set of points. This provides a fast way to access any object by position. We traverse down the hierarchy until we find the smallest cell containing it, and then scan through the objects in this cell to identify the right one.  
Typical algorithms construct kd-trees by partitioning point sets. Each node in the tree is defined by a plane cutting through one of the dimensions. Ideally, this plane equally partitions the subset of points into left/right (or up/down) subsets.
These children are again partitioned into equal halves, using planes through a different dimension. Partitioning stops after lg n levels, with each point in its own leaf cell.
* Bin packing arises in a variety of packaging and manufacturing problems. Suppose that you are manufacturing widgets cut from sheet metal or pants cut from cloth. To minimize cost and waste, we seek to lay out the parts so as to use as few fixed-size metal sheets or bolts of cloth as possible. Identifying which part goes on which sheet in which location is a bin-packing variant called the cutting stock problem. Once our widgets have been successfully manufactured, we are faced with another bin-packing problem—namely how best to fit the boxes into trucks to minimize the number of trucks needed to ship everything. Even the most elementary-sounding bin-packing problems are NP-complete. Thus, we are doomed to think in terms of heuristics instead of worst-case optimal algorithms.
* Implementing basic geometric primitives is a task fraught with peril, even for such simple tasks as returning the intersection point of two lines. It is more complicated than you may think. What should you return if the two lines are parallel, meaning they don’t intersect at all? What if the lines are identical, so the intersection is not a point but the entire line? What if one of the lines is horizontal, so that in the course of solving the equations for the intersection point you are likely to divide by zero? What if the two lines are almost parallel, so that the intersection point is so far from the origin as to cause arithmetic overflows? These issues become even more complicated for intersecting line segments, since there are many other special cases that must be watched for and treated specially. There are two different issues at work here: geometric degeneracy and numerical stability.

##### Combinatorial Problems

* Generate (1) all, or (2) a random, or (3) the next permutation of length n.
* Generate (1) all, or (2) a random, or (3) the next subset of the integers 1 to n.
* Generate (1) all, or (2) a random, or (3) the next integer or set partitions of length n.

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/generating-permutations-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/generating-permutations-R.gif" /></td>
  </tr>
</table>

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/generating-subsets-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/generating-subsets-R.gif" /></td>
  </tr>
</table>

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/generating-partitions-L.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/generating-partitions-R.gif" /></td>
  </tr>
</table>

Discussions
* There are two different types of combinatorial objects denoted by the word “partition,” namely integer partitions and set partitions. They are quite different beasts, but it is a good idea to make both a part of your vocabulary:
  * Integer partitions are multisets of nonzero integers that add up exactly to n. For example, the seven distinct integer partitions of 5 are {5}, {4,1}, {3,2}, {3,1,1}, {2,2,1}, {2,1,1,1}, and {1,1,1,1,1}. An interesting application I encountered that required generating integer partitions was in a simulation of nuclear fission. When an atom is smashed, the nucleus of protons and neutrons is broken into a set of smaller clusters. The sum of the particles in the set of clusters must equal the original size of the nucleus. As such, the integer partitions of this original size represent all the possible ways to smash an atom.
  * Set partitions divide the elements 1, ..., n into nonempty subsets. There are 15 distinct set partitions of n = 4: {1234}, {123,4}, {124,3}, {12,34}, {12,3,4}, {134,2}, {13,24}, {13,2,4}, {14,23}, {1,234}, {1,23,4}, {14,2,3}, {1,24,3}, {1,2,34}, and {1,2,3,4}. Several algorithm problems return set partitions as results, including vertex/edge coloring and connected components.

##### Numerical Problems

Input: An integer n.  
Problem: Is n a prime number, and if not what are its factors?

<table>
  <tr>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/factoring-integers-R.gif" /></td>
    <td><img src="http://www.cs.sunysb.edu/~algorith/files/factoring-integers-R.gif" /></td>
  </tr>
</table>

Discussions
* The dual problems of integer factorization and primality testing have surprisingly many applications for a problem long suspected of being only of mathematical interest.
The security of the RSA public-key cryptography system is based on the computational intractability of factoring large integers. As a more modest application, hash table performance typically improves when the table size is a prime number. To get this benefit, an initialization routine must identify a prime near the desired table size. Finally, prime numbers are just interesting to play with. It is no coincidence that programs to generate large primes often reside in the games directory of UNIX systems.  
Factoring and primality testing are clearly related problems, although they are quite different algorithmically. There exist algorithms that can demonstrate that an integer is composite (i.e. , not prime) without actually giving the factors. To convince yourself of the plausibility of this, note that you can demonstrate the compositeness of any nontrivial integer whose last digit is 0, 2, 4, 5, 6, or 8 without doing the actual division.  
The simplest algorithm for both of these problems is brute-force trial division. To factor n, compute the remainder of n/i for all 1 < i ≤ √n. The prime factorization of n will contain at least one instance of every i such that n/i = floor(n/i), unless n is prime. Make sure you handle the multiplicities correctly, and account for any primes larger than √n. Such algorithms can be sped up by using a precomputed table of small primes to avoid testing all possible i. Surprisingly large numbers of primes can be represented in surprisingly little space by using bit vectors.
