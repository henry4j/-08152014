#### Java 8, Google Guava, and Lombok 

http://technology.amis.nl/2013/10/05/java-8-collection-enhancements-leveraging-lambda-expressions-or-how-java-emulates-sql/
http://www.javacodegeeks.com/2013/10/java8.html

###### [Install JDK 1.8 B61 for Lambda](http://jdk8.java.net/lambda/) -- [lambda-8-b61.tar.gz](http://download.java.net/lambda/b61/macosx-x86_64/lambda-8-b61-macosx-x86_64-14_oct_2012.tar.gz)

    wget -P /tmp/ http://download.java.net/lambda/b61/macosx-x86_64/lambda-8-b61-macosx-x86_64-14_oct_2012.tar.gz # b61
    mkdir -p /tmp/jdk1.8.0-lambda.jdk/Contents
    tar xvf /tmp/lambda-8-b61-macosx-x86_64-14_oct_2012.tar.gz -C /tmp/jdk1.8.0-lambda.jdk/Contents
    mv /tmp/jdk1.8.0-lambda.jdk/Contents/jdk1.8.0 /tmp/jdk1.8.0-lambda.jdk/Contents/Home
    sudo mv /tmp/jdk1.8.0-lambda.jdk /Library/Java/JavaVirtualMachines/jdk1.8.0-lambda.jdk

###### [Install IntelliJ IDEA 12 EAP](http://confluence.jetbrains.net/display/IDEADEV/IDEA+12+EAP) -- [ideaIC-122.519.dmg](http://download.jetbrains.com/idea/ideaIC-122.519.dmg)

###### mvn archetype:generate

```bash
echo | mvn archetype:generate \
  -DarchetypeGroupId=org.apache.maven.archetypes \
  -DarchetypeArtifactId=maven-archetype-quickstart \
  -DarchetypeVersion=1.1 \
  -DgroupId=com.henry4j \
  -DartifactId=recipes \
  -Dversion=1.0-SNAPSHOT \
  -DpackageName=com.henry4j \
  -DinteractiveMode=false
```

###### Add a dependency to [Google Guava 13.0.1](https://oss.sonatype.org/content/repositories/releases/com/google/guava/guava/13.0.1/)

```xml
<dependency>
  <groupId>com.google.guava</groupId>
  <artifactId>guava</artifactId>
  <version>13.0.1</version>
</dependency>
```

###### Add a dependency to [Lombok-pg 0.11.3](https://oss.sonatype.org/content/repositories/releases/com/github/peichhorn/lombok-pg/0.11.3/)

```xml
<dependency>
  <groupId>com.github.peichhorn</groupId>
  <artifactId>lombok-pg</artifactId>
  <version>0.11.3</version>
</dependency>
<dependency>
  <groupId>com.github.peichhorn</groupId>
  <artifactId>lombok-pg</artifactId>
  <version>0.11.3</version>
  <classifier>runtime</classifier>
  <scope>runtime</scope>
</dependency>
```

###### Mod [lombok-maven-plugin/0.11.4.0](http://awhitford.github.com/lombok.maven/lombok-maven-plugin/dependencies.html) to use lombok-pg 0.11.3

```diff
46,48c46,48
<       <groupId>org.projectlombok</groupId>
<       <artifactId>lombok</artifactId>
<       <version>${lombok.version}</version>
---
>       <groupId>com.github.peichhorn</groupId>
>       <artifactId>lombok-pg</artifactId>
>       <version>0.11.3</version>
```

###### [Lombok features](http://projectlombok.org/features/) and [changelog](http://projectlombok.org/changelog.html)

* Lombok 11.4 has experimental `@Value`, `@Wither`, and `@FieldDefaults` for immutable classes.
* Lombok 11.3 has `@ExtensionMethod` to add extentions to any type with statid methods.
* Lombok 11.2 has a combo of [`@Accessors`](http://projectlombok.org/features/experimental/Accessors.html) (fluent getters and setters), `@Getter`, and `@Data`.
* [Lombok-pg 11.3 - 07/24/2012](https://github.com/peichhorn/lombok-pg/wiki) has [`@Yield`](https://github.com/peichhorn/lombok-pg/wiki/Yield), [`@Action`](https://github.com/peichhorn/lombok-pg/wiki/%40Action), [`@Function`](https://github.com/peichhorn/lombok-pg/wiki/%40Function), [`@Predicate`](https://github.com/peichhorn/lombok-pg/wiki/%40Predicate), [`@ExtensionMethod`](https://github.com/peichhorn/lombok-pg/wiki/%40ExtensionMethod), [`@Synchronized`](http://projectlombok.org/features/Synchronized.html), ...
* [Lombok-pg 11.4 - Snapshot](https://oss.sonatype.org/content/repositories/snapshots/com/github/peichhorn/lombok-pg/0.11.4-SNAPSHOT/) has some bug fixes.
* How-to jumpstart Lombok-pg
  * `java -jar lombok-pg-0.11.3.jar` # install Lombok for Eclipse.
  * `mvn package`, or `lombok:delombok` # build, or delombok source.

###### Lombok basics

* [`@Getter, @Setter`](http://projectlombok.org/features/GetterSetter.html), and [`@Accessors(fluent = true)`](http://projectlombok.org/features/experimental/Accessors.html) are applicable at class or field scopes.
  * [`@Getter(lazy=true)`](http://projectlombok.org/features/GetterLazy.html) uses [double-checked locking](http://en.wikipedia.org/wiki/Double-checked_locking) to reduce locking overhead in lazy init.
* [`@NoArgsConstructor`, `@RequiredArgsConstructor`, and `@AllArgsConstructor`](http://projectlombok.org/features/Constructor.html)
* [`@Cleanup`](http://projectlombok.org/features/Cleanup.html) finally invokes `close()`, or some other no-arg method unless the given resource is `null`.
  * If our main code throws an exception, and the cleanup method call also throws an exception, then the original exception is hidden by the exception thrown by the cleanup -- we should not rely on this, and still need to handle any exception from the clean up method.
* [`@SneakyThrows`](http://projectlombok.org/features/SneakyThrows.html) has come down to two common uses.
  * When throwing a runtime exception from a needlessly strict interface such as [`Runnable`](http://docs.oracle.com/javase/7/docs/api/java/lang/Runnable.html) only obscures the real cause of the issue.
  * When there is no reason throw & catch impossible exceptions, e.g. `UnsupportedEncodingException` from `new String(bytes, "UTF-8")` -- no reason to throw & catch this, as JVM specifies UTF-8 must always be available.
* [`@Synchronized`](http://projectlombok.org/features/Synchronized.html) locks on fields `$lock = Object[0]` and `$Locks = new Object[0]` for instance and static methods.

###### Build & execute a package

```bash
mvn package # or install
mvn exec:java -Dexec.mainClass="com.henry4j.App"
```

###### Create IntelliJ and Eclipse projects

```bash
mvn idea:idea
mvn eclipse:eclipse -DdownloadSources=true -DdownloadJavadocs=true
```
