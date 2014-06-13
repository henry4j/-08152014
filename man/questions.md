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
4. Given a string and its actual length, write a method to replace all spaces in a string with `%20`. You may assume that the string has sufficient space at the end of the string to hold the additional characters (note: if implementing in Java, please use a character array so that you can perform this operation in place). e.g.,  
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

17.10 Since XML is very verbose, you are given a way of encoding it where each tag gets mapped to a pre-defined integer value. The language/grammar is as follows:  
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

17.14 Oh, no! You have just completed a lengthy document when you have an unfortunate Find/Replace mishap. You have accidently removed all spaces, punctuation, and capitalization in the document. A sentence like "I reset the computer. It still didn't boot!" would become "iresetthecomputeritstillldidntboot". You figure that you can add back in the punctuation and capitalization later, once you get the individual words properly separated. Most of the words will be in a dictionary, but some strings, like proper names, will not.  
Given a dictionary (a list of words) design an algorithm to find the optimal way of un-concatenating a sequence of words. In this case, "optimal" is defined to be the parsing which minimizes the number of unrecognized sequences of characters. e.g.  
the string "jesslookedjustliketimherbrother" would be optimally parsed as "JESS looked just like TIM her brother". This parsing has seven unrecognized characters, which we have capitalized for clarity.
