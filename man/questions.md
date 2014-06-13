##### 0. Pet Peeves

* Given two lines on a Cartesian plane, determine whether the two lines would intersect.
<img src="http://upload.wikimedia.org/math/9/a/6/9a6cc88202ee96f8165c4be5ab42ec00.png" />
<img src="http://upload.wikimedia.org/math/a/c/d/acd2938d1c482f5247654e6822ec06ad.png" />
<img src="http://upload.wikimedia.org/math/6/c/8/6c88e10b6c07b86c33deac72ba33cf6f.png" />

##### 13. C and C++

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

##### 15. Databases

1. Write a SQL query to get a list of tenants who are renting more than on apartment.
2. Write a SQL query to get a list all buildings and the number of open requests (Requests in which status equals 'Open').
3. Reading #11 is undergoing a major renovation. Implement a query to close all requests from apartments in this building.
4. What are the different types of joins? Please explain how they differ and why certain types are better than in certain situations.
5. What is denormalization? Explain the pros and cons.
6. Draw an entity-relationship diagram for a database with companies, people, and professionals (people who work for companies).
7. Imagine a simple database storing information for students' grade. Design what this database might look like and provide a SQL query to return a list of the honor roll student (top 10%) sorted by their grade point average.

##### 17. Moderate Problems

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
