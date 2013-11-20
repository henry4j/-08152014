##### [Ehcache 2.6](http://ehcache.org/documentation)

* [cache-aside](http://ehcache.org/documentation/get-started/getting-started#cache-aside)
* [cache-as-sor](http://ehcache.org/documentation/get-started/getting-started#cache-as-sor)
* [ehcache-failsafe.xml](http://ehcache.org/documentation/user-guide/configuration#ehcache-failsafexml)

```xml
<ehcache>
  <diskStore path="java.io.tmpdir" />
  <defaultCache
     maxEntriesLocalHeap="10000"
     eternal="false"
     timeToIdleSeconds="120"
     timeToLiveSeconds="120"
     maxEntriesLocalDisk="10000000"
     diskExpiryThreadIntervalSeconds="120"
     memoryStoreEvictionPolicy="LRU"
     <persistence strategy="localTempSwap" />
  />
</ehcache>
```

* [dynamically-changing-cache-configuration](http://ehcache.org/documentation/configuration/configuration#dynamically-changing-cache-configuration)

```java
Cache cache = manager.getCache("sampleCache");
CacheConfiguration config = cache.getCacheConfiguration();
config.setTimeToIdleSeconds(60);
config.setTimeToLiveSeconds(120);
config.setmaxEntriesLocalHeap(10000);
config.setmaxEntriesLocalDisk(1000000);
```

* [EhCacheManagerFactoryBean](http://static.springsource.org/spring/docs/3.2.x/javadoc-api/org/springframework/cache/ehcache/EhCacheManagerFactoryBean.html)
* [EhCacheCacheManager](http://static.springsource.org/spring/docs/3.2.x/javadoc-api/org/springframework/cache/ehcache/EhCacheCacheManager.html)

```xml
<bean id="cacheManager" class="org.springframework.cache.ehcache.EhCacheCacheManager" p:cache-manager-ref="ehcache"/>

<!-- Ehcache library setup -->
<bean id="ehcache" class="org.springframework.cache.ehcache.EhCacheManagerFactoryBean" p:config-location="ehcache.xml"/>
```

##### [Spring Cache Abstraction](http://static.springsource.org/spring/docs/3.2.x/spring-framework-reference/html/cache.html)

###### Cache vs Buffer

The terms **buffer** and **cache** tend to be used interchangeably; note however they represent different things. A buffer is used traditionally as an intermediate temporary store for data between a fast and a slow entity. As one party would have to wait for the other affecting performance, the buffer alleviates this by allowing entire blocks of data to move at once rather then in small chunks. The data is written and read only once from the buffer. Furthermore, the buffers are visible to at least one party which is aware of it.  
A cache on the other hand by definition is hidden and neither party is aware that caching occurs.It as well improves performance but does that by allowing the same data to be read multiple times in a fast fashion.

###### Cache declaration and configuration

There are two integrations available out of the box, for JDK java.util.concurrent.ConcurrentMap and Ehcache ...

```java
@Cacheable("books")
public Book findBook(ISBN isbn)
```

```java
@Cacheable(value="books", key="#isbn")
public Book findBook(ISBN isbn, boolean checkWarehouse, boolean includeUsed)

@Cacheable(value="books", key="#isbn.rawNumber")
public Book findBook(ISBN isbn, boolean checkWarehouse, boolean includeUsed)

@Cacheable(value="books", key="T(someType).hash(#isbn)")
public Book findBook(ISBN isbn, boolean checkWarehouse, boolean includeUsed)
```

```java
@Cacheable(value="book", condition="#name.length < 32")
public Book findBook(String name)
```

Just like its sibling, @CacheEvict requires one to specify one (or multiple) caches that are affected by the action, allows a key or a condition to be specified but in addition, features an extra parameter allEntries which indicates whether a cache-wide eviction needs to be performed rather then just an entry one (based on the key):

```java
@CacheEvict(value = "books", allEntries=true)
public void loadBooks(InputStream batch)
```

```java
@Caching(evict = { @CacheEvict("primary"), @CacheEvict(value = "secondary", key = "#p0") })
public Book importBooks(String deposit, Date date)
```

###### JDK ConcurrentMap-based Cache

```xml
<!-- generic cache manager -->
<bean id="cacheManager" class="org.springframework.cache.support.SimpleCacheManager">
  <property name="caches">
    <set>
      <bean class="org.springframework.cache.concurrent.ConcurrentMapCacheFactoryBean" p:name="default"/>
      <bean class="org.springframework.cache.concurrent.ConcurrentMapCacheFactoryBean" p:name="books"/>
    </set>
  </property>
</bean>
```

###### Ehcache-based Cache

```xml
<bean id="cacheManager" class="org.springframework.cache.ehcache.EhCacheCacheManager" p:cache-manager-ref="ehcache"/>

<!-- Ehcache library setup -->
<bean id="ehcache" class="org.springframework.cache.ehcache.EhCacheManagerFactoryBean" p:config-location="ehcache.xml"/>
```
