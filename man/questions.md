##### 0. Pet Peeves

* Given two lines on a Cartesian plane, determine whether the two lines would intersect.
<img src="http://upload.wikimedia.org/math/9/a/6/9a6cc88202ee96f8165c4be5ab42ec00.png" />
<img src="http://upload.wikimedia.org/math/a/c/d/acd2938d1c482f5247654e6822ec06ad.png" />
<img src="http://upload.wikimedia.org/math/6/c/8/6c88e10b6c07b86c33deac72ba33cf6f.png" />
* Given a 2D graph with points on it, find a line, which passes the most number of points.
* discrete math: 
  * a _k_-combination of a set S is a subset of _k_ distinct elements of S, and the # of k-combinations is equals to the binomial coefficient, <b>n! / (k! * (n-k)!)</b>.
  * a _k_-permutation of a set S is an ordered sequence of k distinct elements of S, and the # of _k_-permutation of n objects is denoted variously <sub>n</sub>P<sub>k</sub>, P<sub>n,k</sub>, and P(n,k), and its value is given by <b>n! / (n-k)!</b>.
  * [how many ways are there to merge N companies](http://placementsindia.blogspot.jp/2007/12/solutions-to-few-google-top-interview.html)? `C(n,2)*C(n-1,2)*...*C(2,2) = n!*(n-1)!/(2**(n-1))`

##### 1. Arrays and Strings

1. Implement an algorithm to determine if a string has all unique characters. What if you cannot use additional data structures?  
2. Implement a function `void reverse(char* str)` in C or C++ which reverses a null-terminated string.  
3. Given two strings, write a method to determine if one is a permutation of the other.
4. Given a string and its actual length, write a method to replace all spaces in a string with `%20**`. You may assume that the string has sufficient space at the end of the string to hold the additional characters (note: if implementing in Java, please use a character array so that you can perform this operation in place). e.g.,  
INPUT: "Mr John Smith", and OUTPUT: "Mr%20John%20Smith".
5. Write a method to perform basic string compression using the counts of repeated characters. For example, the string `aabcccccaaa` would become `a2b1c5a3`. If the 'compressed' string would not become smaller than the original string, your method should return the original string.
6. Given an image represented by an NxN matrix, where each pixel in the image is 4 bytes, write a method to rotate the image by 90 degrees. Can you do this in place?
7. Implement an algorithm such that if an element in an NxN matrix is 0, its entire row and column are set to 0.
8. Assume you have a method `isSubstring` which checks if a string is a substring of another. Given two strings, s1 and s2, write a method to check if s2 is a rotation of s1 using only one call to isSubstring, e.g., "waterbottle" is a rotation of "erbottlewat".

##### 2. Linked Lists

1. Write code to remove duplicates from an unsorted linked list. What if you cannot use a temporary buffer?
2. Implement an algorithm to find the k-th to last element of a singly linked list.
3. Given access only to a node, implement an algorithm to delete that node in the middle of a singly linked list. e.g.,  
INPUT: node c from a linked list, `a -> b -> c -> d -> e`  
OUTPUT: nothing is returned, but the new linked list looks like `a -> b -> d -> e`.
4. Write code to partition a linked list around a value x, such that all nodes less than x come before all nodes greater than or equal to x.
5. There are two decimal numbers represented by a linked list, where each node contains a single digit. The digits are stored in reverse order, such that the 1's digit is at the head of the list. Write a function that adds the two numbers and returns the sum as a linked list.
6. Given a circular linked list, implement an algorithm which returns the node at the beginning of the loop. e.g.,  
INPUT: `a -> b -> c -> d -> e -> c`, and OUTPUT: `c`.
7. Implement a method to check if a linked list is a palindrome.

##### 3. Stacks and Queues

1. Implement three stacks using a single array.
2. Design a stack that has a `min` function that returns the minimum element in addition to `push` and `pop`. Push, pop, and min should all operate in O(1) time.
3. Imagine a literal stack of plates. If the stack gets too high, it might topple. Therefore, in real life, we would likely start a new stack when the previous stack exceeds some threshold. Implement a data structure SetOfStacks that mimics this. SetOfStacks should be composed of several stacks and should create a new stack once the previous one exceeds capacity. SetOfStacks.push() and SetOfStacks.pop() should behave identically to a single stack (that is, pop() should return the same values as it would if there were just a single stack). Implement a function popAt(int index) which performs a pop operation on a specific sub-stack.
4. In the classic problem of the towers of Hanoi, you have 3 towers and N disks of different sizes which can slide onto any tower. The puzzle starts with disks sorted in ascending order of size from top to bottom. i.e., each disk sits on top of an even larger one. You have the following constraints:  
   (1) only one disk can be moved a a time.
   (2) a disk is slid off the top of one tower onto the next tower.
   (3) a disk can only be placed on top of a larger disk.
   Write a program to move the disks from the first tower to the last using stacks.
5. Implement a queue using two stacks.
6. Write a program to sort a stack in ascending order with biggest items on top. You may use additional stacks to hold items, but you may not copy the elements into any other dsta structure such as an array. The stack supports the following operations: push, pop, peek, and isEmpty.
7. An animal shelter holds only dogs and cats, and operations on a strictly "first in, first out" basis. People must adopt either the oldest (based on the arrival time) of all animals at the shelter, or they can select whether they would prefer a dog or a cat (and will receive the oldest animal of that type). They cannot select which speicific animal they would like. Create the data structures to maintain this system and implement operations such as enqueue, dequeueAny, dequeueDog, and dequeueCat. You may use the LinkedList data structure.

##### 4. Trees and Graphs

1. Implement a function to check if a binary tree is balanced. For the purposes of this question, a balanced tree is defined to be a tree such that the heights of two subtrees of any node never differ by more than one.
2. Given a directed graph, design an algorithm to find out whether there is a route between two nodes.
3. Given a sorted (increasing order) array, implement an algorithm to create a binary search tree with minimal height.
4. Given a binary tree, design an algorithm which creates a linked list of all the nodes at each depth, e.g., if you have a tree with depth D, you will have D linked lists.
5. Implement a function to check if a binary tree is a binary search tree.
6. Design an algorithm to find the next node (i.e., in-order successor) of a given node in a binary search tree. You may assume that each node has a link to its parents.
7. Design an algorithm to find the first common ancestor of the nodes in a binary tree. Avoid storing additional nodes in a data structure. Note: this is not necessarily a binary search tree.
8. You have two very large binary trees: T1 with millions of nodes, and T2 with hundreds of nodes. Design an algorithm to decide if T2 is a subtree of T1. A tree T2 is a subtree of T1 if there exists a node in T1 such that the subtree of n is identical to T2. i.e., if you cut off the tree at node n, the two trees would be identical.
9. Given a binary tree in which each node contains a value. Design an algorithm to print all paths which sum to a given value. Note that a path can start or end anywhere in the tree.

##### 5. Bit Operations

1. You are given two 32-bit numbers, N and M, and two bit positions, i and j. Write a method to insert M into N such that M starts at bit j and ends at bit i. You can assume that the bits j through i have enough space to fit all of M. That is, if M = 10011, you can assume that there are at least 5 bits between j and i. You would not, for example, have j = 3 and i = 3, because M couldn't be fully fit beween bit 3 and bit 2. e.g., INPUT: n = 10000000000, m = 10011, i = 2, j = 6, and OUTPUT: n = 10001001100.
2. Given a real number between 0 and 1 (e.g. 0.72) that is passed in as a double print the binary representation. If the number cannot be represented accurately in binary with at most 32 characters, print "ERROR".
3. Given a positive integer, print the next smallest and the next largest number that have the same number of 1 bits in their binary representation.
4. Explain what the following code does: ((n & (n - 1)) == 0).
5. Write a funciton to determine the number of bits required to convert integer A to integer B. e.g. INPUT: 31, 14, OUTPUT: 2.
6. Write a program to swap odd and event bits in an integer with as few instructions as possible (e.g. bit 0 and bit 1 are swapped, bit 2 and bit 3 are swapped, and so on).
7. An array A contains all the integers from 0 to n, except for one number which is missing. In this problem, we cannot access an entire integer in A with a single operation. The elements of A are represented in binary, and the only operation we can use to access them is "fetch the jth bit of A[i]" which takes constant time. Write code to find the missing integer. Can you do it in O(n) time?
8. A monochrome screen is stored as a single array of bytes allowing eight consecutive pixels to be stored in one byte. The screen has width w, where w is divisible by 8 (this is no byte will be split across rows). The height of the screen of course can be derived from the length of the array and the width. Implement a function drawHorizontalLine(byte[] screen, int width, int x1, int x2, int y) which draws a horizonal line from (x1, y) to (x2, y).

#### 6. Brain Teasers

1. You have 20 bottles of pills. 19 bottles have 1-gram pills, but one has pills of weight 1.1 grams. Given a scale that provides an exact measurement, how would you find the heavy bottle? You can only use the scale once.
2. There is an 8x8 chessboard in which two diagonally opposite domino can cover exactly two squares. Can you use the 31 dominos to cover the entire board? Prove your answer (by providing an example or showing why it's impossible).
3. You have a five-quart jug, a three-quart jug, and an unlimited supply of water (but no measuring cups). How would you come up with exactly four quarts of water? Note that the jugs are oddly shaped, such that filling up exactly "half" of the jug would be impossible.
4. A bunch of people is living on an island, when a visitor comes with a strange order: all blue-eyed people must leave the island as soon as possible. There will be a flight out at 8:00 pm every evening. Each person can see everyone else’s eye color, but they do not know his or her own (nor is anyone allowed to tell them). Additionally, they do not know how many people have blue eyes, although they do know that at least one person does. How many days will it take the blue-eyed people to leave?
5. There is a building of 100 floors. If an egg drops from the Nth floor or above, it will break. If it's dropped from any floor below, it will not break. You're given two eggs. Find N, while minimizing the number of drops for the worse case.
6. There are 100 closed lockers in a hallway. A man begins by opening all 100 lockers. Next, he closes every second locker. Then, on his third pass, he toggles every third locker (closes it if it is open or opens it if it is closed). This process continues for 100 passes, such that on each pass i, the man toggles every i-th locker. After his 100th pass in the hallway, in which he toggles only locker #100, how many lockers are open?

#### 7. Math and Probability

1. You have a basketball hoop and someone says that you can ply one of two games. Game 1: You get one shot to make the hoop. Game 2: You get three shoots and you have to make two of three shots. If p is the probability of making a particular shot, for which values of p should you pick one game or the other?
2. There are three ants on different vertices of a triangle. What is the probability of collision (between any two or all of them) if they start walking on the sides of the triangle? Assume that each ant randomly picks a direction, which either direction being equally like to be chosen, and that they walk at the same speed. Similarly, find the probability of collision with n ants on an n-vertex polygon.
3. Given two lines on a Cartesian plane, determine whether the two lines would intersect.
4. Write methods to implement the multiply, subtract, and divide operations for integer. Use only the add operator.
5. Given two squares on a 2D plane, find a line that would cut these two squares in half. Assume that the top and bottom sides of the square run parallel to the x-axis.
6. Given a 2D graph with points on it, find a line, which passes the most number of points.
7. Deign an algorithm to find the k-th number such that the only prime factors are 3, 5, and 7.

#### 8. Object-Oriented Design

1. Design the data structure for a generic deck of cards. Explain how you would subclass the data structures to implement blackjack.
2. Imagine you have a call center with three levels of employees: respondent, manager, and director. An incoming telephone call must be first allocated to a respondent who is free. If the respondent can't handle the call, he or she must escalate the call to a manager. If the manager is not free or not able to handle it, then the call should be escalated to a director. Design the classes and data structures for this problem. Implement a method dispatchCall() which assigns a call to the first available employee.
3. Design a musical jukebox using OO principles.
4. Design a parking lot using OO principles.
5. Design the data structures for an online book reader system.
6. Implement a jigsaw puzzle. Design the data structures and explain an algorithm to solve the puzzle. You can assume that you have a fitsWith method which when passed two puzzle pieces return true if the two pieces belong together.
7. Explain how you would design a chat server. In particular, provide details about the various backend components, classes, and methods. What would be the hardest problems to solve?
8. Othello is played as follows. Each Othello piece is white on one side and black on the other. When a piece is surrounded by its opponents on both the left and right sides, or both the top and bottom, it is said to be captured and its color is flipped. On your turn, you must capture at least one of your opponent's pieces. The game ends when either user has no more valid moves. The win is assigned to the person with the most pieces. Implement the OOD for Othello.
9. Explain the data structures and algorithms that you would use to design an in-memory file system. Illustrate with an example in code where possible.
10. Design and implement a hash table, which uses chaining (linked lists) to handle collisions.

#### 9. Recursion and DP

1. A child is running up a staircase with n steps, and can hop either 1 step, 2 steps, or 3 steps at a time. Implement a method to count how many possible ways the child can run up the stairs.
2. Imagine a robot sitting on the upper left corner of an X by Y grid. The robot can only move in two directions: right and down. How many possible paths are there for the robot to go from (0.0) to (x, y)? FOLLOW UP: Imagine certain spots are "off limits", such that the robot cannot step on them. Design an algorithm to find a path for the robot from the top left to the bottom right.
3. A magic index in an array A[0..n-1] is defined to be an index such that A[i] = i. Given a sorted array, write a method to find a magic index, if one exists, in an array A. FOLLOW UP: What if the values are not distinct?
4. Write a method to return all subsets of a set.
5. Write a method to compute all permutations of a string.
6. Implement an algorithm to print all valid (e.g. properly opened and closed) combinations of n-pairs of parenthesis. e.g.  
INPUT: 6, OUTPUT: ((())), (()()), (())(), ()(()), ()()()
7. Implement the paint fill function that one might see on many image editing programs. That is given a screen (represented by a 2D array of colors), a point, and a new color, fill in the surrounding area until the color changes from the original color.
8. Given an infinite number of quarters (25 cents), dimes (10 cents), nickels (5 cents), and pennies (1 cents), write code to calculate the number of ways of representing n cents.
9. Write an algorithm to print all ways of arranging eight queens on an 8x8 chess board so that none of them share the same row, column, or diagonal. In this case, "diagonal" means all diagonals, not just the two that bisect the board.
10. You have a stack of n boxes, with widths w(i), heights h(i), and depths d(i). The boxes cannot be rotated and can only be stacked on top of one another if each box in the stack is strictly larger than the box above it in width, height, and depth. Implement a method to build the tallest stack possible, where the height of a stack is the sum of the heights of each box.
11. Given a boolean expression consisting of the symbols `0, 1, &, |, and ^`, and a desired boolean result value result, implement a function to count the number of ways of parenthesizing the expression such that evalutes to result. e.g.  
INPUT: Expression: `1^0|0|1`, Desired Result: `false(0)`  
OUTPUT: 2 ways: `1^((0|0)|1)` and `1^(0|(0|1))`.

#### 10. Scalability and Memory Limits

1. Imagine you are building some sort of service that will be called by up to 1000 client apps to get simple end-of-day stock price information (open, close, high, low). You may assume that you already have the data, and you can store it in any format you wish. How would you design the client-facing service, which provides the information to client apps? You are responsible for the development, rollout, and ongoing monitoring, and maintenance of feed. Describe the different methods you considered and why you would recommend your approach. Your service can use any technologies you wish, and can distribute the information to the client app in any mechanism you choose.
2. How would you design the data structures for a very large social network like Facebook or LinkedIn? Describe how you would design an algorithm to show the connection, or path, between two people, e.g. Me -> Bob -> Susan -> Jason -> You.
3. Given an input file with four billion non-negative integers, provide an algorithm to generate an integer, which is not contained in the file. Assume you have 1 GB of memory available for this task. FOLLOW UP: What if you have only 10 MB of memory? Assume that all the values are distinct.
4. You have an array with all the numbers from 1 to N, where N is at most 32,000. The array may have duplicate entries and you do not know what N is. With only 4 KB of memory available, how would you print all duplicate elements in the array?
5. If you were designing a web crawler, how would you avoid getting into infinite loops?
6. You have 10 billion URLs. How do you detect the duplicate documents? In this case, assume that "duplicate" means that the URLs are identical.
7. Imagine a web server for a simplified search engine. This system has 100 machines to respond to search queries, which may then call out using processSearch(String query) to another cluster of machines to actually get the result. The machine which responds to a given query is chosen at random, so you cannot guarantee that the same machine will always response to the same request. The method processSearch is very expensive. Design a caching mechanism for the most recent queries. Be sure to explain how you would update the cache when data changes.

#### 11. Sorting and Searching

1. You are given two sorted arrays, A and B, where A has a large enough buffer at the end to hold B. Write a method to merge B into A in sorted order.
2. Write a method to sort an array of strings so that all the anagrams are next to each other.
3. Given a sorted array of n integers that has been rotated an unknown number of times, write code to find an element in the array. You may assume that the array was originally sotrted in increasing order. e.g.  
INPUT: find 5 in {15, 16, 19, 20, 25, 1, 3, 4, 5, 7, 10, 14}  
OUTPUT: 8 (the index of 5 in the array)
4. Imagine you have a 20 GB file with one string per line. Explain how you would sort the file.
5. Given a sorted array of strings, which is interspersed with empty strings, write a method to find the location of a given string. e.g.  
INPUT: find "ball" in {"at", "", "", "", "ball", "", "", "car", "", "", "dad", "", ""}  
OUTPUT: 4
6. Given an M x N matrix in which each row and each column is sorted in ascending order, write a method to find an element.
7. A circus is designing a tower routine consisting of people standing atop one another's shoulders. For practical and aesthetic reasons, each person must be both shorter and lighter than the person below him or her. Given the heights and weights of each person in the circus, write a method to compute the largest possible number of people in such a tower. e.g.  
INPUT: (ht, wt): (65, 100) (90, 150) (50, 120) (56, 90) (75, 190) (60, 95) (68, 110) (80, 92).  
OUTPUT: the longest tower is length 5 and includes from top to bottom: (56, 90), (60, 95), (65, 100), (68, 110), (90, 150).

8. Imagine you are reading in a stream of integers. Periodically, you wish to be able to look up the rank of a number x (number of values less than or equal to x). Implement the data structures and algorithms to support these operations. That is, implement the method track(int x), which is called when each number is generated, and the method getRankOfNumber(int x), which returns the number of values less than or equal to x (not including x itself). e.g.  
Stream (in order of appearance): 5, 1, 4, 4, 5, 9, 7, 13, 3.  
getRankOfNumber(1) = 0  
getRankOfNumber(3) = 1  
getRankOfNumber(4) = 3

#### 12. Test

1. Find the mistake in the following code:  
`1:  unsigned int i;`  
`2:  for (i = 100; i >= 0: --i)`  
`3:  ____printf("%d\n", i);`
2. You are given the source to an app which crashes when it is run. After running it 10 times in a debugger, you find it never crashes in the same place. The app is single threaded, and uses only the C standard library. What programming errors could be causing this crash? How would you test each one?
3. We have the following method used in a chess game: boolean canMoveTo(int x, int y). This method is part of the Piece class and returns whether or not the piece can move to position (x, y). Explain how you would test this method.
4. How would you load test a webpage without using any test tools?
5. How would you test a pen?
6. How would you test an ATM in a distributed banking system?

#### 13. C and C++

1. Write a method to print the last K lines of an input file using C++.
2. Compare and contrast a hash table and an STL map. How is a hash table implemented? If the number of inputs is small, which data structure options can be used instead of a hash table?
3. How do virtual functions work in C++?
4. What is the difference between deep copy and shallow copy? Explain how you would use each.
5. What is the significance of the keyword "volatile" in C?
6. Why does a destructor in base class need to be declared virtual?
7. Write a method that takes a pointer to a Node structure as a parameter and return a complete copy of the passed in data structure. The Node data structure contains two pionts to other Nodes.
8. Write a smart pointer class. A smart pointer is a data type, usually implemented with templates, that simulates a pointer while also providing automatic garbage collection. It automatically counts the number of references to a `SmartPointer<T *>` object and frees the object of type T when the reference count hits zero.
9. Write an aligned malloc and free function that supports allocating memory such that the memory address returned is divisible by a specific power of two. e.g.  
`align_malloc(1000, 128)` will return a memory address that is a multiple of 128 and that points to memory of size 1000 bytes.  
`aligned_free()` will free memory allocated by align_malloc.
10. Write a function in C called my2DAlloc which allocates a 2D array. Minimize the number of calls to malloc and make sure that the memory is accessible by the notation arr[i][j].

#### 14. Java

1. In terms of inheritance, what is the effect of keeping a constructor private?
2. In Java, does the finally block get executed if we insert a return statement inside the try block of a try-catch-finally?
3. What is the difference between final, finally, and finalize?
4. Explain the difference between templates in C++ and generics in Java.
5. Explain what object reflection is in Java and why it is useful.
6. Implement a `CircularArray` class that supports an array-like data structure, which can be effectively rotated. The class should use a generic type, and should support iteration via the standard for (Object o : circularArray) notation.

<table>
  <thead>
    <tr>
      <th colspan="2">Apartments</th>
      <th colspan="2">Buildings</th>
      <th colspan="2">Tenants</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>AppID</td>
      <td>int</td>
      <td>BuildingID</td>
      <td>int</td>
      <td>TenantId</td>
      <td>int</td>
    </tr>
    <tr>
      <td>UnitNumber</td>
      <td>varchar</td>
      <td>ComplexID</td>
      <td>int</td>
      <td>TenantName</td>
      <td>varchar</td>
    </tr>
    <tr>
      <td>BuildingID</td>
      <td>int</td>
      <td>BuildingName</td>
      <td>varchar</td>
      <td>-</td>
      <td>-</td>
    </tr>
    <tr>
      <td>-</td>
      <td>-</td>
      <td>Address</td>
      <td>varchar</td>
      <td>-</td>
      <td>-</td>
    </tr>
  </tbody>
</table>
<table>
  <thead>
    <tr>
      <th colspan="2">Complexes</th>
      <th colspan="2">AptTenants</th>
      <th colspan="2">Requests</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>ComplexID</td>
      <td>int</td>
      <td>TenantID</td>
      <td>int</td>
      <td>RequestID</td>
      <td>int</td>
    </tr>
    <tr>
      <td>ComplexName</td>
      <td>varchar</td>
      <td>AptID</td>
      <td>int</td>
      <td>Status</td>
      <td>varchar</td>
    </tr>
    <tr>
      <td>-</td>
      <td>-</td>
      <td>-</td>
      <td>-</td>
      <td>AptID</td>
      <td>int</td>
    </tr>
    <tr>
      <td>-</td>
      <td>-</td>
      <td>-</td>
      <td>-</td>
      <td>Description</td>
      <td>varchar</td>
    </tr>
  </tbody>
</table>

#### 15. Databases

1. Write a SQL query to get a list of tenants who are renting more than on apartment.
2. Write a SQL query to get a list all buildings and the number of open requests (Requests in which status equals 'Open').
3. Reading #11 is undergoing a major renovation. Implement a query to close all requests from apartments in this building.
4. What are the different types of joins? Please explain how they differ and why certain types are better than in certain situations.
5. What is denormalization? Explain the pros and cons.
6. Draw an entity-relationship diagram for a database with companies, people, and professionals (people who work for companies).
7. Imagine a simple database storing information for students' grade. Design what this database might look like and provide a SQL query to return a list of the honor roll student (top 10%) sorted by their grade point average.

#### 16. Threads & Locks

1. What's the difference between a thread and a process?
2. How would you measure the time spent in a context switch?
3. In the famous dining philosophers problem, a bunch of philosophers are sitting around a circular table with one chopstick between each of them. A philosopher needs both chopsticks to eat and always picks up the left chopstick before the right one. A deadlock could potentially occur if all the philosophers reached for the left chopstick at the same time. Using threads and locks, implement a simulation of the dining philosophers problem that prevents deadlocks.
4. Design a class which provides a lock only if there are no possible deadlocks.
5. Suppose we have the following code:  
`public class Foo {`  
`__public Foo() { ... }`  
`__public void first() { ... }`  
`__public void second() { ... }`  
`__public void third() { ... }`  
`}`
The same instance of Foo will be passed to three different threads. ThreadA will call first, threadB will call second, and threadC will call third. Design a mechanism to ensure that first is called before second, and second is called before third.
6. You are given a class with synchronized method `A` and a normal method `C`. If you have two threads in one instance of a problem, can they both execute A at the same? Can they execute A and C at the same time?

#### 17. Moderate Problems

1. Write a function to swap a number in place (that is without temporary variables).
2. Design an algorithm to figure out if someone has won a game of tic-tac-toe.
3. Write an algorithm, which computes the number of trailing zeros in n factorial.
4. Write a method, which finds the maximum of two numbers. You should not use if-else or any other comparison operator.
5. The Game of Master Mind is played as follows:  
The computer has four slots, and each slot will contain a ball that is red (R), yellow (Y), green (G), or blue (B). For example, the computer might have RGGB (Slot #1 is red, Slot #2 and #3 are green, Slot #4 is blue).  
When you guess the correct color for the correct slot, you get a "hit". If you guess a color that exits but in the wrong slot, you get a "pseudo-hit". Note that a slot that is a hit can never count as a pseudo-hit. e.g.  
If the actual solution is RGBY and you guess GGRR, you have one hit and one pseudo-hit. Write a method that given a guess and a solution, return the number of hits and pseudo-hits.
6. Given an array of integers, write a method to find indices m and n such that if you sorted elements m through n, the entire array would be sorted. Minimize n - m (that is, find the smallest such sequence). e.g.  
INPUT: 1, 2, 4, 7, 10, 11, 7, 12, 6, 7, 16, 18, 19  
OUTPUT: 3, 9
7. Given any integer, print an English phrase that describes the integer (e.g. "One Thousand. Two Hundred Thirty Four").
8. You are given an array of integers (both positive and negative). Find the contiguous sequences with the largest sum. Return the sum. e.g.  
INPUT: 2, -8, 3, -2, 4, -10  
OUTPUT: 5 (or, 3, -2, 4)
9. Design a method to find the frequency of occurrences of any given word in a book.
10. Since XML is very verbose, you are given a way of encoding it where each tag gets mapped to a pre-defined integer value. The language/grammar is as follows:  
<table>
  <tbody>
    <tr>
      <td>Element</td>
      <td>Tag Attributes END Children END</td>
    </tr>
    <tr>
      <td>Attribute</td>
      <td>Tag Value</td>
    </tr>
    <tr>
      <td>END</td>
      <td>0</td>
    </tr>
    <tr>
      <td>Tag</td>
      <td>some predefined mapping to int</td>
    </tr>
    <tr>
      <td>Value</td>
      <td>string value END</td>
    </tr>
  </tbody>
</table>
e.g. the following XML might be converted into the compressed string below (assuming a mapping of family -> 1, person -> 2, firstname -> 3, lastName -> 4, state -> 5).  
`<family lastName="McDowell" state="CA">`  
`__<person firstName="Gayle">Some Message</person>`  
`</family>`  
becomes:  
`1 4 McDowell 5 CA 0 2 3 Gayle 0 Some Message 0 0`.  
Write code to print the encoded version of an XML element (passed in Element and Attribute objects).
11. Implement a method rand7() given rand5(). Given a method that generates a random number between 1 and 5 (inclusive), write a method that generates a random number between 1 and 7 (inclusive).
12. Design an algorithm to find all pairs of integers within an array which sum to a specified value.
13. Consider a simple node-like data structure called BiNode, which has pointers to two other nodes.  
`public class BiNode {`  
`__public BiNode node1, node2`  
`__public int data`  
`}`  
The data structure BiNode could be used to represent both a binary tree (where node1 is the left node and node2 is the right node), or a doubly linked list (where node 1 is the previous node and node2 is the next node). Implement a method to convert a binary search tree (implemented with BiNode) into a doubly linked list. The values should be kept in order and the operation should be performed is place (this is on the original data structure).
14. Oh, no! You have just completed a lengthy document when you have an unfortunate Find/Replace mishap. You have accidently removed all spaces, punctuation, and capitalization in the document. A sentence like "I reset the computer. It still didn't boot!" would become "iresetthecomputeritstillldidntboot". You figure that you can add back in the punctuation and capitalization later, once you get the individual words properly separated. Most of the words will be in a dictionary, but some strings, like proper names, will not.  
Given a dictionary (a list of words) design an algorithm to find the optimal way of un-concatenating a sequence of words. In this case, "optimal" is defined to be the parsing which minimizes the number of unrecognized sequences of characters. e.g.  
the string "jesslookedjustliketimherbrother" would be optimally parsed as "JESS looked just like TIM her brother". This parsing has seven unrecognized characters, which we have capitalized for clarity.

#### 18. Hard Problems

1. Write a function that adds two numbers. You should not use + or any arithmetic operators.
2. Write a method to shuffle a deck of cards. It must be a perfect shuffle -- in other words, each of the 52! permutations of the deck has to be equally likely. Assume that you are given a random number generator which is perfect.
3. Write a method to randomly generate a set of m integers from an array of size n. Each element must have equal probability of being chosen.
4. Write a method to count the number of 2s that appear in all the numbers between 0 and n (inclusive). e.g.  
INPUT: 25  
OUTPUT: 9 (2, 12, 20, 21, 22, 23, 24, and 25. Note that 22 counts for two 2s)
5. You have a large text file containing words. Given any two words, find the shortest distance (in terms of number of words) between them in the file. Can you make the searching operation in O(1) time? What about the space complexity for your solution?
6. Describe an algorithm to find the smallest one million numbers in one billion numbers. Assume that the computer memory can hold all one billion numbers.
7. Given a list of words, write a program to find the longest word made of other words in the list. e.g.  
INPUT: car, banana, dog, nana, walk, walker, dogwalker  
OUTPUT: dogwalker
8. Given a string s and an array of smaller strings T, design a method to search s for each small string in T.
9. Numbers are randomly generated and passed to a method. Write a program to find and maintain the median value as new values are generated.
10. Given two words of equal length that are in a dictionary, write a method to transform one word into another word by changing only one letter at a time. The new word you get in each step must be in the dictionary. e.g.  
INPUT: DAMP, LIKE  
OUTPUT: DAMP -> LAMP -> LIMP -> LIME -> LIKE
11. Imagine you have a square matrix, where each cell (pixel) is either black or white. Design an algorithm to find the maximum subsquare such that all four borders are filled with black pixels.
12. Given an NxN matrix of positive and negatives integers, write code to find the submatrix with the largest possible sum.
13. Given a list of millions of words, design an algorithm to create the largest possible rectangle of letters such that every row forms a word (reading left to right) and every column forms a word (reading top to bottom). The words need not be chosen consecutively from the list, but all rows must be the same length and all columns must be the same height.
