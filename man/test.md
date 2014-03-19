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

#### Mockito <sub>favored over EasyMock and JMockit</sub>

* Mockito's motivation: Java mocking is dominated by expect-run-verify libraries like EasyMock or jMock. Mockito offers simpler and more intuitive approach: you ask questions about interactions after execution. Using mockito, you can verify what you want (state-based testing). Using expect-run-verify libraries you are often forced to look after irrelevant interactions. See tutorials: Mockito and PowerMock.

Discussions:

* http://code.google.com/p/jmockit/wiki/MockingToolkitComparisonMatrix - feature-rich, but not popular.
* http://java.dzone.com/articles/mockito-pros-cons-and-best - it was compared to old EasyMock v2.
* http://blog.octo.com/en/easymock-facts-fallacies/ - EasyMock's response. But, GoodBye to expect-run-verify.

##### Mackito: Stub in "when thenAnswer" style

Let's see how we mock and stub classes & interfaces:

```java
//You can mock concrete classes, not only interfaces
LinkedList mockedList = mock(LinkedList.class);
 
//stubbing
when(mockedList.get(0)).thenReturn("first");
when(mockedList.get(1)).thenThrow(new RuntimeException());
 
System.out.println(mockedList.get(0)); // prints "first"
System.out.println(mockedList.get(1)); // throws runtime exception
System.out.println(mockedList.get(999)); // prints "null" because get(999) was not stubbed
 
//Although it is possible to verify a stubbed invocation, usually it's just redundant
//If your code cares what get(0) returns then something else breaks (often before even verify()).
//If your code doesn't care what get(0) returns then it should not be stubbed. Not convinced? See here.
verify(mockedList).get(0);
```

##### Mackito: Stub in "doAnswer" method family

You can use doThrow(), doAnswer(), doNothing(), and doReturn() in place of the corresponding call with when(), for any method. It is necessary when you

* stub void methods
* stub the same method more than once, to change the behavior of a mock in the middle of a test.

but you may prefer to use these methods in place of the alternative with when(), for all of your stubbing calls.

```java
when(mock.some("any")).thenReturn("1", "2", "3"); // you may prefer doAnswer method family.
 
Stubber s = null;
for (int i = 0; i < 256; i++) {
    String r = Integer.toString(i);
    s = null == s ? doReturn(r): s.doReturn(r); // necessary to use doReturn method.
}
s = s.doThrow(new RuntimeException("UNCHECKED: this bug should go unhandled due to unexpected invocation(s)."));
s.when(mock).some(anyString());
```

**See Also**: Bimock tool has to stub method invocations dynamically as follows:'

```java
Stubber s = null;
for (Invocation i: invocations) {
    if (null != i.failure()) {
        s = null == s ? doThrow(i.failure()) : s.doThrow(i.failure());
    } else if (Void.TYPE.equals(i.method().getReturnType())) {
        s = null == s ? doNothing() : s.doNothing();
    } else {
        s = null == s ? doReturn(i.success()) : s.doReturn(i.success());
    }
}
```

#### JUnit Best Practices

##### AssertThat w/ Matchers

**Q**: Why use assertThat in place of traditional assertXXX? 
**A**: Compiled from JUnit 4.4 release note:

* More readable and typeable: this syntax allows you to think in terms of subject, verb, object (assert "x is 3") rather than assertEquals, which uses verb, object, subject (assert "equals 3 x")
* Combinations: any matcher statement s can be negated (not(s)), combined (either(s).or(t)), mapped to a collection (each(s)), or used in custom combinations (afterFiveSeconds(s))
* Readable failure messages. (...)
* Custom Matchers. By implementing the Matcher interface yourself, you can get all of the above benefits for your own custom assertions.

**See Also**: JavaDoc: ~~JUnitMatchers~~ (deprecated by CoreMatchers) and Hamcrest CoreMatchers

```java
import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.CoreMatchers.both;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.everyItem;
import static org.hamcrest.CoreMatchers.hasItems;
import static org.junit.Assert.assertThat;
 
import java.util.Arrays;
 
import org.hamcrest.core.CombinableMatcher;
import org.junit.Test;
 
public class AssertTests {
    // DO: assertThat with Core Hamcrest Matchers
    @Test
    public void testAssertThatHamcrestCoreMatchers() {
        assertThat("good", allOf(equalTo("good"), startsWith("good")));
        assertThat("good", not(allOf(equalTo("bad"), equalTo("good"))));
        assertThat("good", anyOf(equalTo("bad"), equalTo("good")));
        assertThat(7, not(CombinableMatcher.<Integer> either(equalTo(3)).or(equalTo(4))));
        assertThat(new Object(), not(sameInstance(new Object())));
    }
 
    @Test
    public void testAssertThatBothContainsString() {
        assertThat("albumen", both(containsString("a")).and(containsString("b")));
    }
 
    @Test
    public void testAssertThatEveryItemContainsString() {
        assertThat(Arrays.asList(new String[] { "fun", "ban", "net" }), everyItem(containsString("n")));
    }
 
   @Test
    public void testMoreMatchers() {
        List<String> list = Arrays.asList("a", "b", "a");
        assertThat(list, CoreMatchers.hasItems("a"));
        assertThat(list, CoreMatchers.hasItems("a", "b", "b", "b"));
        assertThat(list, not(CoreMatchers.hasItems("c")));
 
        assertThat(list, IsIterableContainingInOrder.contains("a", "b", "a"));
        assertThat(list, not(IsIterableContainingInOrder.contains("a", "b")));
 
        assertThat(list, IsIterableContainingInAnyOrder.containsInAnyOrder("a", "b", "a"));
        assertThat(list, IsIterableContainingInAnyOrder.containsInAnyOrder("a", "a", "b"));
        assertThat(list, IsIterableContainingInAnyOrder.containsInAnyOrder("b", "a", "a"));
        assertThat(list, not(IsIterableContainingInAnyOrder.containsInAnyOrder("a", "b")));
 
        String[] set = { "a", "b", "a" };
        assertThat(set, IsArrayContaining.hasItemInArray("b"));
        assertThat(set, IsArrayContainingInOrder.arrayContaining("a", "b", "a"));
        assertThat(set, not(IsArrayContainingInOrder.arrayContaining("b", "a", "a")));
 
        assertThat(set, IsArrayContainingInAnyOrder.arrayContainingInAnyOrder("b", "a", "a"));
        assertThat(set, not(IsArrayContainingInAnyOrder.arrayContainingInAnyOrder("b", "a", "a", "a")));
 
        Map<String, ?> hash = ImmutableMap.<String, Object>of("k1", "v1", "k2", 2);
        assertThat(hash.keySet(), IsIterableContainingInAnyOrder.containsInAnyOrder("k1", "k2"));
        assertThat(hash, IsMapContaining.hasKey("k1"));
        assertThat(hash, not(IsMapContaining.hasEntry("k1", (Object)2)));
        assertThat(hash, IsMapContaining.hasValue((Object)2));
    }
 
    // DON'T: Classic
    @Test
    public void testAssertTrue() {
        org.junit.Assert.assertTrue("failure - should be true", true);
    }
 
    @Test
    public void testAssertArrayEquals() {
        byte[] expected = "trial".getBytes();
        byte[] actual = "trial".getBytes();
        org.junit.Assert.assertArrayEquals("failure - byte arrays not same", expected, actual);
    }
 
    @Test
    public void testAssertEquals() {
        org.junit.Assert.assertEquals("failure - strings not same", 5l, 5l);
    }
 
    @Test
    public void testAssertFalse() {
        org.junit.Assert.assertFalse("failure - should be false", false);
    }
 
    @Test
    public void testAssertNotNull() {
        org.junit.Assert.assertNotNull("should not be null", new Object());
    }
 
    @Test
    public void testAssertNotSame() {
        org.junit.Assert.assertNotSame("should not be same Object", new Object(), new Object());
    }
 
    @Test
    public void testAssertNull() {
        org.junit.Assert.assertNull("should be null", null);
    }
 
    @Test
    public void testAssertSame() {
        Integer aNumber = Integer.valueOf(768);
        org.junit.Assert.assertSame("should be same", aNumber, aNumber);
    }
}
```
