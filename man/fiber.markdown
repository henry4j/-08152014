#### ToC

```
3. Sharing Objects - lock vs. volatile; atomicity & visibility guarantees
4. Composing Objects - delgating and adding to existing thread-safe classes
5. Building Blocks - synchronized & concurrent collections, and synchronizers
13. Intrinsic & Explicit vs. Read-Write Locks
14. Custom Synchronizers - intrinsic and explicit condition queues, and AQS
15. CAS by atomic variables, and Lock-free stack & queue
16. Java Memory Model - lazy initialization idioms
```

#### 3. Sharing Objects

<table>
  <tbody>
    <tr>
      <th align="left">[x]&nbsp;Thread&#8209;confined</th>
      <td>Be owned exclusively by and confined to one thread, and can be modified by its owning thread.</td>
    </tr>
    <tr>
      <th align="left">[ ] Read-only</th>
      <td>Be immutable or effectively immutable objects that can be accessed concurrently w/o synchronization.</td>
    </tr>
    <tr>
      <th align="left">[ ] Thread-safe</th>
      <td>Be synchronized internally, so freely accessed through its public interface w/o further synchronization.</td>
    </tr>
    <tr>
      <th align="left">[ ] Guarded</th>
      <td>Be accessed only w/ a specific lock held once published.<br/>Be encapsulated within other thread-safe objects.</td>
    </tr>
  </tbody>
</table>

##### Locking guarantees both visibility & atomicity; `volatile` only guarantees visibility.

* DO use a common lock to synchronize reading and writing threads to ensure that all threads see the most up-to-date values out of shared mutable variables -- locking is not just about mutual exclusion; it is also about memory visibility.
* DO use `volatile` variables only when they simplify implementing and verifying your synchronization policy; avoid using volatile variables when verifying correctness would require subtle reasoning about visibility. Good uses of volatile variables include ensuring the visibility of their own state, that of the object they refer to, or indicating that an important lifecycle event (such as initialization or shutdown) has occurred.

> The JVM specification effectively says that entering and exiting synchronized blocks and methods has to be a **"safe barrier"** to these operations. If we read and write to variables inside synchronized blocks from different threads, we do always expect Thread 1 to see the value set by Thread 2; just seeing a locally cached copy in a register isn't correct. So on entry to and exit from a synchronized block, the relevant reads/writes to main memory have to take place, and they have to take place in the correct sequence. We can't re-order the write to take place after we exit the synchronized block, and we can't re-order the read to take place before we enter. In other words, the JVM is not allowed to do this:

```
LOAD R0, [address of some Java variable]   ; Cache a copy of the variable
enter-synchronization
ADD R0, #1                                 ; Do something with the (cached copy) of the variable
```
or this:

```
enter-synchronized-block
LOAD R0, [address of some Java variable]      ; Cache a copy of the variable
MUL R0, #2                                    ; Do something with it
leave-synchronized-block
STORE R0, [address of variable]               ; Write the new value back to the variable
```

It's possible to say all this in a very obtuse way (as I say, see Chapter 17 of the language spec). But at the end of the day it's kind of common sense: if the whole point of synchronization is to make sure all threads see the updated "master" copy of variables, it's no use updating them after you've left the synchronized block.

##### Thread Confinement by local or ThreadLocal for not sharing objects

Confining an object to a thread is an element of your program's design that must be enforced by its implementation.
The language and core libraries provide mechanisms for thread confinement - local variables and the ThreadLocal class.

```java
@SuppressWarnings("serial")
public class DataWarehouseDate extends java.util.Date {
    private static final ThreadLocal<SimpleDateFormat> sdf = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected synchronized SimpleDateFormat initialValue() {
            return new SimpleDateFormat("dd-MMM-yy") {{ setTimeZone(TimeZone.getTimeZone("GMT")); }};
        }
    };

    public DataWarehouseDate(Date date) {
        setTime(date.getTime());
    }

    public static DataWarehouseDate parseDate(String source) throws ParseException {
        return new DataWarehouseDate(sdf.get().parse(source));
    }

    @Override
    public String toString() {
        return sdf.get().format(this);
    }
}
```

#### 4. Composing Objects

##### Delegating and Adding to Existing Thread-Safe Classes? It depends ... relies on delicate reasoning about the possible states.

* If a class is composed of multiple independent thread-safe state variables and has no operations that have any invalid state transitions, then it can delegate thread safety to the underlying state variables.
* If a state variable is thread-safe, does not participate in any invariants that constrain its value, and has no prohibited state transitions for any of its operations, then it can safely be published.

```java
public class DelegatingVehicleTracker {
    private final ConcurrentMap<String, Point> locations;
    private final Map<String, Point> unmodifiableMap;

    public DelegatingVehicleTracker(Map<String, Point> points) {
        locations = new ConcurrentHashMap<String, Point>(points);
        unmodifiableMap = Collections.unmodifiableMap(locations);
    }

    public Map<String, Point> getLocations() {
        return unmodifiableMap;
    }

    public Point getLocation(String id) {
        return locations.get(id);
    }

    public void setLocation(String id, int x, int y) {
        if (locations.replace(id, new Point(x, y)) == null)
            throw new IllegalArgumentException("UNCHECKED: invalid vehicle id ('" + id + "').");
    }
}
```

##### Client-Side Locking, which can break `base` synchronization policy?

* Client-side locking has a lot in common with class extension. Just as extension breaks encapsulation of base implementation, client-side locking breaks encapsulation of synchronization policy.
* ListHelper shows a putIfAbsent operation on a thread-safe List that correctly uses client-side locking. The documentation for Vector and the synchronized wrapper classes states, albeit obliquely, that they support client-side locking, by using the intrinsic lock for the Vector or the wrapper collection.
* ImprovedList adds an additional level of locking using its own intrinsic lock. It does not care whether the underlying List is thread-safe, because it provides its own consistent locking that provides thread safety even if the List is not thread-safe or changes its locking implementation.

```java
public class ListHelper<E> {
    public List<E> list = Collections.synchronizedList(new ArrayList<E>());

    public boolean putIfAbsent(E x) {
        synchronized (list) { // this client-side locking breaks `base` synchronization policy.
            boolean absent = !list.contains(x);
            if (absent)
                list.add(x);
            return absent;
        }
    }
}
```

```java
@ThreadSafe
public class ImprovedList<T> implements List<T> {
    private final List<T> list;
    public ImprovedList(List<T> list) { this.list = list; }

    public synchronized boolean putIfAbsent(T x) { // adds an additional layer of locking.
        boolean absent = !list.contains(x);
        if (absent)
            list.add(x);
        return absent;
    }

    public synchronized void clear() { list.clear(); }
}
```

#### 5. Building Blocks -- <sup>synchronized & concurrent collections and synchronizers</sup>
##### Synchronizers: CountDownLatch for events, CyclicBarrier for threads, FutureTask (result-bearing computation), and Semaphore.

* [CyclicBarrier(parties, barrierAction)](http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/CyclicBarrier.html) allows a set of threads to all wait for each other to reach a common barrier point.
  * ensures that a computation does not proceed until resources it needs have been initialized.
  * ensures that a service does not start until other services on which it depends have started.
  * do wait, not proceed until all parties or players involved in an activity are ready to proceed.

```java
public static long timeTasks(int nThreads, final Runnable task)
        throws InterruptedException {
    final CountDownLatch startGate = new CountDownLatch(1);
    final CountDownLatch endGate = new CountDownLatch(nThreads);

    for (int i = 0; i < nThreads; i++) {
        (new Thread() {
            public void run() {
                try { 
                    startGate.countDown();
                } catch (InterruptedException ignore) { }
                try {
                    task.run();
                } finally {
                    endGate.countDown();
                }
            }
        }).start();
    }

    long start = System.nanoTime();
    startGate.countDown();
    endGate.await();
    return System.nanoTime() - start;
}
```

* [CountDownLatch(count)](http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/CountDownLatch.html) allows one or more threads to wait until a set of operations being performed in other threads completes.
* [Semaphore(permits, fair)](http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Semaphore.html) maintains a set of permits conceptually. `acquire` blocks until a permit is available, and `release` adds a permit.
* [ReentrantLock(fair)](http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/locks/ReentrantLock.html) is a reentrant mutual exclusion Lock with the same basic behavior and semantics as the implicit monitor lock accessed using synchronized methods and statements, but with extended capabilities.

##### Iterators w/ ConcurrentModificationException, CopyOnWriteArrayList, or CopyOnWriteArraySet

```java
private class Itr implements Iterator<E> {
int cursor;       // index of next element to return
int lastRet = -1; // index of last element returned; -1 if no such
int expectedModCount = modCount;

public boolean hasNext() {
    return cursor != size;
}

@SuppressWarnings("unchecked")
public E next() {
    checkForComodification();
    int i = cursor;
    if (i >= size)
        throw new NoSuchElementException();
    Object[] elementData = ArrayList.this.elementData;
    if (i >= elementData.length)
        throw new ConcurrentModificationException();
    cursor = i + 1;
    return (E) elementData[lastRet = i];
}

public void remove() {
    if (lastRet < 0)
        throw new IllegalStateException();
    checkForComodification();
    try {
        ArrayList.this.remove(lastRet);
        cursor = lastRet;
        lastRet = -1;
        expectedModCount = modCount;
    } catch (IndexOutOfBoundsException ex) {
        throw new ConcurrentModificationException();
    }
}

final void checkForComodification() {
    if (modCount != expectedModCount)
        throw new ConcurrentModificationException();
}
```

##### Blocking Q and Producer-consumer Pattern

* Implementations: LinkedBlockingQueue, ArrayBlockingQueue, PriorityBlockingQueue, SynchronousQueue, and LinkedBlockingDeque.
* In a producer-consumer design built around a blocking queue, producers place data onto the queue as it becomes available, and consumers retrieve data from the queue when they are ready to take the appropriate action.
* Work stealing is well suited to problems in which consumers are also producers - when performing a unit of work is likely to result in the identification of more work. For example, processing a page in a web crawler usually results in the identification of new pages to be crawled. Similarly, many graph-exploring algorithms, such as marking the heap during garbage collection, can be efficiently parallelized using work stealing. When a worker identifies a new unit of work, it places it at the end of its own deque (or alternatively, in a work sharing design, on that of another worker); when its deque is empty, it looks for work at the end of someone else's deque, ensuring that each worker stays busy.

##### Concurrent replacements: ConcurrentHashMap, ConcurrentSkipListMap, and ConcurrentSkipListSet.

Lock splitting can sometimes be extended to partition locking on a variable-sized set of independent objects, in which case it is called lock striping. e.g., the implementation of ConcurrentHashMap uses an array of 16 locks, each of which guards 1/16 of the hash buckets; bucket N is guarded by lock N mod 16. Assuming the hash function provides reasonable spreading characteristics and keys are accessed uniformly, this should reduce the demand for any given lock by approximately a factor of 16. It is this technique that enables ConcurrentHashMap to support up to 16 concurrent writers.
One of the downsides of lock striping is that locking the collection for exclusive access is more difficult and costly than with a single lock. Usually an operation can be performed by acquiring at most one lock, but occasionally you need to lock the entire collection, as when ConcurrentHashMap needs to expand the map and **rehash** the values into a larger set of buckets. This is typically done by acquiring all of the locks in the stripe set.

```java
public class StripedMap {
    // Synchronization policy: buckets[n] guarded by locks[n%N_LOCKS]
    private static final int N_LOCKS = 16;
    private final Node[] buckets;
    private final Object[] locks;

    private static class Node {
        Object key;
        Object value;
        Node next;
    }

    public StripedMap(int buckets) {
        this.buckets = new Node[buckets];
        this.locks = new Object[N_LOCKS];
        for (int i = 0; i < N_LOCKS; i++) {
            this.locks[i] = new Object();
        }
    }

    public Object get(Object key) {
        int hash = hash(key);
        synchronized (locks[hash % N_LOCKS]) {
            for (Node m = buckets[hash]; m != null; m = m.next) {
                if (m.key.equals(key)) {
                    return m.value;
                }
            }
        }
        return null;
    }

    public void clear() {
        for (int i = 0; i < buckets.length; i++) {
            synchronized (locks[i % N_LOCKS]) {
                buckets[i] = null;
            }
        }
    }

    private int hash(Object key) {
        return Math.abs(key.hashCode() % buckets.length);
    }
}
```

> Since the primary source of serialization in Java programs is the exclusive resource lock, scalability can often be improved by spending less time holding locks, either by reducing lock granularity, reducing the duration for which locks are held, or replacing exclusive locks with nonexclusive or non-blocking alternatives.

#### 13. Intrinsic & Explicit vs. Read-Write Locks

Explicit Locks offer an extended feature set compared to intrinsic locking, including greater flexibility in dealing with lock unavailability and greater control over queuing behavior. But ReentrantLock is not a blanket substitute for synchronized; use it only when you need features that synchronized lacks.  
Read-write locks allow multiple readers to access a guarded object concurrently, offering the potential for improved scalability when accessing read-mostly data structures.

```java
public interface Lock { // implemented by ReentrantLock
    void lock();
    void lockInterruptibly() throws InterruptedException;
    boolean tryLock();
    boolean tryLock(long timeout, TimeUnit unit) throws InterruptedException;
    void unlock();
    Condition newCondition();
}
```

```java
public interface Condition {
    void await() throws InterruptedException;
    boolean await(long time, TimeUnit unit) throws InterruptedException;
    long awaitNanos(long nanosTimeout) throws InterruptedException;
    void awaitUninterruptibly();
    boolean awaitUntil(Date deadline) throws InterruptedException;
    void signal();
    void signalAll();
}
```

```java
@Getter @Accessors(fluent = true)
public class SemaphoreReadWriteLock implements ReadWriteLock {
    private final Lock readLock;
    private final Lock writeLock;

    public SemaphoreReadWriteLock(Semaphore semaphore) {
        this.readLock = new SemaphoreLock(semaphore);
        this.writeLock = new SemaphoreWriteLock(semaphore);
    }

    private static class SemaphoreWriteLock implements Lock {
        private final Semaphore semaphore;
        private final int permits;

        SemaphoreWriteLock(Semaphore semaphore) {
            this.semaphore = semaphore;
            this.permits = semaphore.availablePermits();
        }

        public void unlock() {
            semaphore.release(permits);
        }

        public void lock() {
            int drained = drainPermits();
            if (drained < permits) {
                semaphore.acquireUninterruptibly(permits - drained);
            }
        }

        public boolean tryLock() {
            return semaphore.tryAcquire(permits);
        }

        private int drainPermits() {
            return semaphore.isFair() ? 0 : semaphore.drainPermits();
        }
    }
}

@RequiredArgsConstructor
public class SemaphoreLock implements Lock {
    private final Semaphore semaphore;

    public void unlock() {
        this.semaphore.release();
    }

    public void lock() {
        this.semaphore.acquireUninterruptibly();
    }

    public boolean tryLock() {
        return this.semaphore.tryAcquire();
    }
}
```

#### 14. Custom Synchronizers

* If you need to implement a state-dependent class - one whose methods must block if a state-based precondition does not hold - the best strategy is usually to build upon an existing library class such as **Semaphore**, **BlockingQueue**, or **CountDownLatch**.
* Occasionally you should build your own synchronizers using intrinsic condition queues, explicit Condition objects, or AbstractQueuedSynchronizer.
  * **Intrinsic condition queues** are tightly bound to intrinsic locking, since the mechanism for managing state dependence is necessarily tied to the mechanism for ensuring state consistency.
  * **Explicit Conditions** are tightly bound to explicit Locks, and offer an extended feature set, including multiple wait sets per lock, interruptible or uninterruptible condition waits, fair queuing, and deadline-based waiting.

##### Condition Queue - when using condition waits (Object.wait or Condition.await): 

* Always have a condition predicate - some test of object state that must hold before proceeding; 
* Always test the condition predicate before calling wait, and again after returning from wait; 
* Always call wait in a loop; 
* Ensure that the state variables making up the condition predicate are guarded by the lock associated with the 
condition queue; 
* Hold the lock associated with the the condition queue when calling wait, notify, or notifyAll; and 
* Do not release the lock after checking the condition predicate but before acting on it. 

```java
public class SemaphoreOnLock {
    private final Lock lock = new ReentrantLock();
    // CONDITION PREDICATE: permitsAvailable (permits > 0)
    private final Condition permitsAvailable = lock.newCondition();
    private int permits;

    public SemaphoreOnLock(int initialPermits) { permits = initialPermits; }

    public void acquire() throws InterruptedException {
        lock.lock();
        try {
            while (permits <= 0)
                permitsAvailable.await();
            --permits;
        } finally {
            lock.unlock();
        }
    }

    public void release() {
        lock.lock();
        try {
            ++permits;
            permitsAvailable.signal();
        } finally {
            lock.unlock();
        }
    }
}
```

##### AQS (abstract queue synchronizer)

* For a class to be state-dependent, it must have some state. AQS takes on the task of managing some of the state for the synchronizer class: it manages a single integer of state information that can be manipulated through the protected getState, setState, and compareAndSetState methods. This can be used to represent arbitrary state; for example, ReentrantLock uses it to represent the count of times the owning thread has acquired the lock, Semaphore uses it to represent the number of permits remaining, and FutureTask uses it to represent the state of the task (not yet started, running, completed, cancelled). Synchronizers can also manage additional state variables themselves; for example, ReentrantLock keeps track of the current lock owner so it can distinguish between reentrant and contended lock-acquisition requests.
* For example, returning a negative value from TRyAcquireShared indicates acquisition failure; returning zero indicates the synchronizer was acquired exclusively; and returning a positive value indicates the synchronizer was acquired nonexclusively. The TRyRelease and TRyReleaseShared methods should return true if the release may have unblocked threads attempting to acquire the synchronizer.
* In the AQS-based implementation of ReentrantReadWriteLock, a single AQS subclass manages both read and write locking. 
ReentrantRead-WriteLock uses 16 bits of the state for the write-lock count, and the other 16 bits for the read-lock count. Operations on the read lock use the shared acquire and release methods; operations on the write lock use the exclusive acquire and release methods. Internally, AQS maintains a queue of waiting threads, keeping track of whether a thread has requested exclusive or shared access. In ReentrantRead-WriteLock, when the lock becomes available, if the thread at the head of the queue was looking for write access it will get it, and if the thread at the head of the queue was looking for read access, all queued threads up to the first writer will get it.

##### Bounded Buffer by Semaphore and Condition Q

```java
public static class BoundedArrayBuffer<E> { // comparable to Java Concurrency in Practice 12.1
    private final Semaphore empty;
    private final Semaphore full = new Semaphore(0);
    private final E[] items;
    private int head = 0;
    private int tail = 0;

    public BoundedArrayBuffer(int capacity) {
        empty = new Semaphore(capacity);
        items = (E[])new Object[capacity];
    }

    public boolean offer(E e) throws InterruptedException {
        empty.acquire();
        synchronized (this) {
            items[tail] = e;
            tail = (++tail == items.length) ? 0 : tail;
        }
        full.release();
        return true;
    }

    public E poll() throws InterruptedException {
        full.acquire();
        E e;
        synchronized (this) {
            e = items[head];
            head = (++head == items.length) ? 0 : head;
        }
        empty.release();
        return e;
    }

    public boolean isEmpty() {
        return full.availablePermits() == 0;
    }

    public boolean isFull() {
        return empty.availablePermits() == 0;
    }
}
```

```java
public static class BoundedLinkedList<E> {
    private final Queue<E> q = new LinkedList<>();
    private final Semaphore use = new Semaphore(1);
    private final Semaphore empty;
    private final Semaphore full = new Semaphore(0);

    public BoundedLinkedList(int capacity) {
        empty = new Semaphore(capacity);
    }

    public boolean offer(E e) throws InterruptedException {
        boolean added = false;
        empty.acquire();
        try {
            use.acquire();
            try {
                if (added = q.add(e)) {
                    full.release();
                }
            } finally {
                use.release();
            }
        } finally {
            if (!added) {
                empty.release();
            }
        }
        return added;
    }

    public E poll() throws InterruptedException {
        boolean removed = false;
        full.acquire();
        try {
            use.acquire();
            try {
                E e = q.remove(); // instead of poll();
                if (null != e) {
                    removed = true;
                    empty.release();
                }
                return e;
            } finally {
                use.release();
            }
        } finally {
            if (!removed) {
                full.release();
            }
        }
    }
}
```

```java
public static class ConditionBoundedBuffer<E> // comparable to Java Concurrency in Practice 14.11
        extends ConditionBoundedBufferBase<E> {
    private final Lock lock = new ReentrantLock();
    private final Condition notFull = lock.newCondition();
    private final Condition notEmpty = lock.newCondition();

    public ConditionBoundedBuffer(int capacity) {
        super(capacity);
    }

    @Override
    public boolean offer(E e) throws InterruptedException {
        lock.lock();
        try {
            while (isFull())
                notFull.await();
            items[tail] = e;
            tail = (++tail == items.length) ? 0 : tail;
            notEmpty.signal();
        } finally {
            lock.unlock();
        }
        return true;
    }

    @Override
    public E poll() throws InterruptedException {
        lock.lock();
        try {
            while (isEmpty())
                notFull.await();
            E e = items[head];
            head = (++head == items.length) ? 0 : head;
            notEmpty.await();
            return e;
        } finally {
            lock.unlock();
        }
    }
}
```

```java
public static class ConditionBoundedBufferBase<E> { // comparable to Java Concurrency in Practice 14.6
    protected final E[] items;
    protected int head = 0;
    protected int tail = 0;

    public ConditionBoundedBufferBase(int capacity) {
        items = (E[])new Object[capacity + 1];
    }

    public synchronized boolean offer(E e) throws InterruptedException {
        while (isFull())
            wait();
        boolean wasEmpty = isEmpty();
        items[tail] = e;
        tail = (++tail == items.length) ? 0 : tail;
        if (wasEmpty)
            notifyAll();
        return true;
    }

    public synchronized E poll() throws InterruptedException {
        while (isEmpty()) {
            wait();
        }
        boolean wasFull = isFull();
        E e = items[head];
        head = (++head == items.length) ? 0 : head;
        if (wasFull)
            notifyAll();
        return e;
    }

    public boolean isEmpty() {
        return head == tail;
    }

    public boolean isFull() {
        return (head == tail + 1) || (head == 0 && tail == items.length);
        // return head == (tail + 1) % q.length;
    }
}
```

##### AQS-based Synchronizers

```java
public static class SimpleLatchOnAQS { // Java Concurrency in Practice 14.14
    private final Sync sync = new Sync();

    public void await() throws InterruptedException {
        sync.acquireSharedInterruptibly(0);
    }

    public void signal() {
        sync.releaseShared(0);
    }

    private class Sync extends AbstractQueuedSynchronizer {
        @Override
        protected int tryAcquireShared(int ignored) {
            // acquired(1) if the latch is open, or not acquired (-1)
            return getState() == 1 ? 1 : -1;
        }

        @Override
        protected boolean tryReleaseShared(int ignored) {
            setState(1); // the latch is open
            return true; // other threads may now to able to acquire
        }
    }
}
```

```java
class Sync4ReentrantLockOnAQS extends AbstractQueuedSynchronizer {
    Thread owner;

    @Override
    protected boolean tryAcquire(int ignored) {
        final Thread current = Thread.currentThread();
        int c = getState();
        if (0 == c) {
            if (compareAndSetState(0, 1)) {
                owner = current;
                return true;
            }
        } else if (current == owner) {
            setState(c+1);
            return true;
        }
        return false;
    }
}
```

```java
class Sync4SemaphoreOrCountDownLatchOnAQS extends AbstractQueuedSynchronizer {
    @Override
    protected int tryAcquireShared(int acquires) {
        while (true) {
            int available = getState();
            int remaining = available - acquires;
            if (remaining < 0
                || compareAndSetState(available, remaining)) {
                return remaining;
            }
        }
    }

    @Override
    protected boolean tryReleaseShared(int releases) {
        while (true) {
            int p = getState();
            if (compareAndSetState(p, p + releases)) {
                return true;
            }
        }
    }
}
```

#### 15. CAS by atomic variables and Lock-free algorithms by ConcurrentLinkedQueue

* A non-blocking algorithm ensures that threads competing for a shared resource do not have their execution indefinitely postponed by mutual exclusion.
  * A non-blocking algorithm is **lock-free** if there is guaranteed **process-wide progress**; **wait-free** if there is also guaranteed **per-thread progress**.
* Non-blocking algorithms derive their thread safety from the fact that, like locking, **compareAndSet** provides **both atomicity and visibility guarantees**. When a thread changes the state of a stack or queue, it does so with a compareAndSet, which has the memory effects of a volatile write.
* Non-blocking algorithms maintain thread safety by using low-level concurrency primitives such as **compare-and-swap** instead of locks. These low level primitives are exposed through the atomic variable classes, which can also be used as "better volatile variables" providing atomic update operations for integers and object references.  
* Non-blocking algorithms are difficult to design and implement, but can offer **better scalability under typical conditions** and **greater resistance to liveness failures**. Many of the advances in concurrent performance from one JVM version to the next come from the use of non-blocking algorithms, both within the JVM and in the platform libraries.

```java
public static class LockFreeStack<E> {
    AtomicReference<Node<E>> top = new AtomicReference<>();

    public void push(E item) {
        Node<E> newHead = new Node<>(item);
        Node<E> oldHead;
        do {
            oldHead = top.get();
            newHead.next = oldHead;
        } while(!top.compareAndSet(oldHead, newHead));
    }

    public E pop() {
        Node<E> oldHead;
        Node<E> newHead;
        do {
            oldHead = top.get();
            if (null == oldHead) {
                return null;
            }
            newHead = oldHead.next;
        } while (!top.compareAndSet(oldHead, newHead));
        return oldHead.item;
    }

    private static class Node<E> {
        public final E item;
        public Node<E> next;

        public Node(E item) {
            this.item = item;
        }
    }
}
```

```java
public static class LockFreeQueue<E> {
    private final Node<E> nu11 = new Node<>(null, null);
    private final AtomicReference<Node<E>> head = new AtomicReference<>(nu11);
    private final AtomicReference<Node<E>> tail = new AtomicReference<>(nu11);

    public boolean put(E item) {
        Node<E> newNode = new Node<>(item, null);
        while (true) {
            Node<E> oldTail = tail.get();
            Node<E> newTail = oldTail.next.get();
            if (oldTail == tail.get()) {
                if (newTail != null) {
                    // Queue in intermediate state, advance tail
                    tail.compareAndSet(oldTail, newTail);
                } else {
                    // In quiescent state, try inserting new node
                    if (oldTail.next.compareAndSet(null, newNode)) {
                        // Insertion succeeded, try advancing tail
                        tail.compareAndSet(oldTail, newNode);
                        return true;
                    }
                }
            }
        }
    }

    private static class Node<E> {
        public final E item;
        public AtomicReference<Node<E>> next;

        public Node(E item, Node<E> next) {
            this.item = item;
            this.next = new AtomicReference<>(next);
        }
    }
}
```

#### 16. Java Memory Model

* JMM specifies when the actions of a thread on memory are guaranteed to be visible to other threads.
* w/o resorting to the low-level details of happens-before,
  * Synchronization (piggybacking) or safe publication can be used to ensure thread safety.
  * Safe initialization idioms: lazy-initialization holder class, or double-checked locking.

```java
public class ResourceFactory {
    private static class ResourceHolder {
        public static Resource resource = new Resource();
    }

    public static Resource getResource() {
        return ResourceHolder.resource ;
    }
}
```

```java
public class DoubleCheckedLocking { // DCL has been an anti-pattern. Don't do this.
    private static volatile Resource resource; // JMM Java 5.0 and later has enabled DCL.

    public static Resource getInstance() {
        if (resource == null) {
            synchronized (DoubleCheckedLocking.class) {
                if (resource == null)
                    resource = new Resource();
            }
        }
        return resource;
    }
}
```

#### Resources

* [class SemaphoreReadWriteLock implements ReadWriteLock](https://github.com/ha-jdbc/ha-jdbc/blob/master/src/main/java/net/sf/hajdbc/lock/semaphore/SemaphoreReadWriteLock.java)
* [5 things you didn't know about Java multithreaded programming](http://www.ibm.com/developerworks/library/j-5things15/index.html)
* [Java theory and practice: Managing volatility](http://www.ibm.com/developerworks/java/library/j-jtp06197/index.html)
