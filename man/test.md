This page has a good coverage of test design strategies and tactics. Beyond best practices, we choose a set of simple guidelines that govern our test code, and consistently apply them. As we are working on a team, the team should agree to them, and all members should comply.

* JUnit vs. TestNG? For new test implementation, we should stick with JUnit the de facto standard for unit test framework, while we keep existing test implementations as-is -- TestNG comes with little value, but great cost of learning a new set of best practices for certain tasks, e.g. parameterized test/data provider/integration with Spring, Mockito, and etc.
* [Testing states vs. interactions](http://googletesting.blogspot.com/2013/03/testing-on-toilet-testing-state-vs.html)? In general, we would stick with state-based testing; just because a test that uses interactions is passing doesn't mean the code is working properly. This is why in most cases, you want to test state, not interactions -- compiled from [State- vs. Interaction-based testing](http://googletesting.blogspot.com/2013/03/testing-on-toilet-testing-state-vs.html), and [Google's little secret on well-tested code(?)](http://googletesting.blogspot.com/2007/01/introducing-testing-on-toilet.html) - no, it's been Mockito's motivation for years.
* [Mockito vs. EasyMock](https://code.google.com/p/mockito/wiki/MockitoVSEasyMock)? Mockito became a big hit with its motivation of testing states, while Java mock was dominated by expect-run-verify libraries like EasyMock and jMock before EasyMock came ith NiceMock. Mockito is still slightly easier to read and modify, while you can optimally stick with NickMock.
* "pure" unit tests or functional tests? In *Test.java (our unit test set), we'll not only put together unit tests, but functional tests that runs a slice of functions beyond units -- this is where we want to test-run & verify most of quality risks on build.amazon.com fleets. In *IntegrationTest.java (our integration test set), we'd put together integration tests that'd span multiple sub-systems, and even external systems.

#### JUnit 4 vs. TestNG 6

* JUnit is an open-source Java Unit Testing Framework designed by Kent Beck, Erich Gamma. It is the de facto standard for Java Unit Testing. JUnit is not included in JDK, but included in most of the IDEs such as Eclipse and NetBeans.
* TestNG is a testing framework inspired from JUnit and NUnit (the xUnit family), but introduces new functionalities like dependency testing, grouping concept to make testing easier and more powerful. TestNG is designed to cover all types of tests: unit, integration, functional/acceptance, and etc.

Discussions:

* JUnit favored over TestNG - best to possible to integrate w/ Mockito, DbUnit, Android, Jenkins, Spring, and Cucumber -- hylee@ -- it has been our company's **de facto unit test framework**, where we break our tests into \*Test and \*IntegrationTest.
* TestNG favored over Junit - from more features aimed at enterprise Java app development to unit testing w/ extensibility (TestRule) -- honesthenry@ -- it'll be perfect for AT&T test team that requires thousands of well-organized functional/acceptance tests on their billing system app.

#### State- vs. Interaction-based testing

honesthenry@ -- in general, we would stick with state-based testing -- compiled from State- vs. Interaction-based testing, and Google's little secret on well-tested code(?) - no, it's Mockito's motivation for years. See Also: Mockito's Goodbye! to expect-run-verify.

**Q**: Why in most cases do we want to test state, not interactions?
**A**: Just because a test that uses interactions is passing doesn't mean the code is working properly. In general, interactions should be tested when correctness doesn't only depend on what the code's output is, but also how the output is determined.
**Q**: What are some cases where you want to test interactions?
**A**: The code under test calls a method where differences in the number or order of calls would cause undesired behavior, such as side effects (e.g. expecting an email to be sent), latency (e.g. expecting a certain number of disk reads to occur) or multithreading issues (e.g. deadlock from acquiring resources in the wrong order). Testing interactions ensures that your tests will fail if these methods aren't called properly.

##### Examples

Testing state means you're verifying that the code under test returns the right results.

```java
public void testSortNumbers() {
  NumberSorter numberSorter = new NumberSorter(quicksort, bubbleSort);
  // Verify that the returned list is sorted. It doesn't matter which sorting
  // algorithm is used, as long as the right result is returned.
  assertEquals(
      new ArrayList(1, 2, 3),
      numberSorter.sortNumbers(new ArrayList(3, 1, 2)));
}
```

Testing interactions means you're verifying that the code under test calls certain methods properly.

```java
public void testSortNumbers_quicksortIsUsed() {
  // Pass in mocks to the class and call the method under test.
  NumberSorter numberSorter = new NumberSorter(mockQuicksort, mockBubbleSort);
  numberSorter.sortNumbers(new ArrayList(3, 1, 2));
  // Verify that numberSorter.sortNumbers() used quicksort. The test should
  // fail if mockQuicksort.sort() is never called or if it's called with the
  // wrong arguments (e.g. if mockBubbleSort is used to sort the numbers).
  verify(mockQuicksort).sort(new ArrayList(3, 1, 2));
}
```
