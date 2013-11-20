* Reference:
* Wiki
* Vidoes: [ZooKeeper by NIC](http://www.youtube.com/watch?v=2RfBHqDWa60)

##### mission

* build a distributed lock service
* only one process may own the lock
* must preserve ordering of requests
* ensure proper lock release

##### ZK as a distributed coordination service

* distributed, hierarchical filesystem
* high-availability, fault-tolerant
* performant (i.e. it's fast)
* facilitates loose coupling

##### fallacies of distributed computing http://en.wikipedia.org/wiki/Fallacies_of_Distributed_Computing

##### what problems can it solve?

* group membership
* distributed data structures
* reliable configuration service
* distributed workflow

##### data model

* hierarchical filesystem
* comprised of znodes (< 1MB)
* watchers
* atomic znode access (reads/writes)
* security (authentication, ACLs)

