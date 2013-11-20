#### Testing and instrumentation

* Don’t be ignorant. Don’t be arrogant. Good developers write tests.
* Offical testing tools http://developer.android.com/tools/testing/
* Classic testing in Android: unit tests, and functional tests (integration tests, or story test).

##### Testing the Java way (on a standard JVM, not on Dalvik) -- `Run As > JUnit Test`

+ Faster than instrumentation
+ Abundant testing frameworks
+ Mock objects/libraries

##### Testing the Android way -- `Run As > Android JUnit Test`
+ Slow, and technology locked-in w/ the only support of JUnit3

```xml
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.manning.aip.dealdroid.test" ...>
  <application>
    <uses-library android:name="android.test.runner" />
  </application>
  <uses-sdk android:minSdkVersion="4" />
  <instrumentation android:targetPackage="com.manning.aip.dealdroid"
    android:name="android.test.InstrumentationTestRunner" />
</manifest>
```

* AndroidTestCase extends JUnit TestCase -- plain JUnit functionality w/ a bit of Android helper toppings.
  * ApplicationTestCase for android.app.Application
  * ServiceTestCase for android.app.Service
  * ProviderTestCase2 for android.content.ContentProvider
* Android instrumentation and DSLs for test expressiveness
* InstrumentationTestCase extends JUnit TestCase
  * ActivityTestCase
     * ActivityUnitTestCase
     * ActivityInstrumentationTestCase2

#
The instrumentation will not go through the normal runtime lifecycle; only its onCreate method will be called when started in a unit test (more on that in a second).
This is ideally suited for testing an Activity’s internal state, such whether its views are setup correctly or what should happen at its interfaces.
For instance, you could run a test that states that if the Activity isn’t started using a specific kind of Intent, it’ll output an error -- a test for correct input.
Additionally, you could test that it constructs the correct Intent to launch another Activity (but w/o actually launching that other Activity as part of the test!) -- a test for correct output.

##### Beautiful tests w/ Robotium or Calculon

##### mocks and monkeys

* Roboletric to de-fang android.jar
* Monkey spits out ANR stack traces of all threads in /data/anr/traces.txt.
