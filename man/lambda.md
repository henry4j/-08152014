##### [JDK 8 Lambda](http://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html)

* http://jdk8.java.net/lambda/ b79
* http://wiki.netbeans.org/JDK8 netbeans nightly builds
* Java docs: 
  [Stream](http://lambdadoc.net/api/java/util/stream/Stream.html), 
  [Streams](http://lambdadoc.net/api/java/util/stream/Streams.html),
  [Functions](http://lambdadoc.net/api/java/util/function/Functions.html)
* [Java 1 liners](https://github.com/aruld/java-oneliners/wiki)
* http://www.stoyanr.com/2012/12/devoxx-2012-java-8-lambda-and_23.html
* http://java.dzone.com/articles/exploring-java8-lambdas-part-1
* http://blog.dhananjaynene.com/2013/02/exploring-java-8-lambdas-part-1/
* http://frankhinkel.blogspot.com/search/label/Java
* http://learnjavafx.typepad.com/weblog/2013/02/mary-had-a-little-%CE%BB.html
* http://datumedge.blogspot.com/2012/06/java-8-lambdas.html
* http://wiki.eclipse.org/JDT_Core/Java8
* http://openjdk.java.net/projects/jdk8/milestones
* http://mikefroh.blogspot.com/2013/02/more-fun-with-project-lambda.html
* [Java vs. Xtend](http://blog.efftinge.de/2012/12/java-8-vs-xtend.html)

```java
List<String> result = Arrays.asList("Larry", "Moe", "Curly")
                              .stream()
                              .map(s -> "Hello " + s)
                              .collect(Collectors.toList());
```

```java
IntStream str1 = Streams.intRange(1,10);
IntStream str2 = Streams.intRange(21,30);
Stream<Integer> joined = Streams.concat(str1.boxed(), str2.boxed());
System.out.println(joined.collect(Collectors.toList()));
```

```java
Stream<Integer> ints = Streams.intRange(0,5).boxed();
Stream<String> strs = Arrays.asList("foo", "bar", "baz", "qux").stream();
System.out.println(Streams.zip(ints,strs, (i, s) -> i.toString() + s).collect(Collectors.toList()));
```

```java
Stream<String> strings;
strings.map(
    new MapperFunction<String,String>() {
        public String map(String str) {
            return "Hello" + str;
        }
    }
);
```

```java
Arrays.asList(1,2,3,4,5,6)
      .stream()
      .map(n -> n * n) // square the number
      .map(n -> n + 3) // add 3 to the value
      .reduce(0, (i, j) -> i + j)); // add the number to the accumulator
```

```java
List<String> result = Arrays.asList("Larry", "Moe", "Curly")
                              .stream()
                              .map(s -> "Hello " + s)
                              .collect(Collectors.toList());
```

* Many more examples of how to use stream transformations
* Primitives vs. objects. There are a many methods and classes which help work on the primitives alone.
* Closures
* New classes like Optional
* Aspects related to parallelisation
* Hand crafting some of your own classes
