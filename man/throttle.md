Apache Camel http://camel.apache.org/throttler.html

* `maximumRequestsPerPeriod` and `timePeriodMillis`
* http://fusesource.com/docs/router/2.8/eip/MsgRout-Throttler.html

[Apache Zookeeper](http://zookeeper.apache.org/doc/r3.4.5/)

* [Barrier and Queue](http://zookeeper.apache.org/doc/r3.4.5/zookeeperTutorial.html)

```java
boolean enter() throws KeeperException, InterruptedException {
    zk.create(root + "/" + name, new byte[0], Ids.OPEN_ACL_UNSAFE, CreateMode.EPHEMERAL_SEQUENTIAL);
    while (true) {
        synchronized (mutex) {
            List<String> list = zk.getChildren(root, true);
            if (list.size() < size) {
                mutex.wait();
            } else {
                return true;
            }
        }
    }
}

boolean leave() throws KeeperException, InterruptedException {
    zk.delete(root + "/" + name, 0);
    while (true) {
        synchronized (mutex) {
            List<String> list = zk.getChildren(root, true);
            if (list.size() > 0) {
                mutex.wait();
            } else {
                return true;
            }
        }
    }
}
```

Google Guava RateLimter

http://docs.guava-libraries.googlecode.com/git/javadoc/com/google/common/util/concurrent/RateLimiter.html

https://list-archive.amazon.com/messages/425325
https://list-archive.amazon.com/messages/1555920
https://github.com/jabley/rate-limit

Kite https://github.com/williewheeler/kite
http://zkybase.org/blog/2012/05/14/moved-kite-from-google-code-to-github/

CircuitBreaker in action http://techblog.netflix.com/2011_12_01_archive.html

