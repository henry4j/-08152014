##### Python Tricks

```python
fib = lambda n: n > 1 and fib(n-1) + fib(n-2) or 1
fib(0)
fib(1)
fib(2)

def fib(n, memos = {0:1, 1:1}):
  return memos.get(n) or memos.setdefault(n, fib(n - 1, memos) + fib(n - 2, memos))
fib(2)
fib(3)
fib(4)

def fib(n):
  prev = 1; curr = 1
  for i in range(1, n+1):
    curr = curr + prev
    prev = curr
  return curr
```

##### Python Core

* http://eradman.com/posts/ruby-python.html
* http://grschafer.com/guides/2013/08/20/ruby-and-python-by-example/
* http://refcardz.dzone.com/refcardz/core-python
* http://speakerdeck.com/astreal/best-practices-and-coding-style-for-python
* http://speakerdeck.com/erik/writing-clean-python-code
* http://speakerdeck.com/mleone/python-for-ruby-programmers
* http://wit.io/posts/the-ugliness-of-python
* http://www.diveintopython3.net/porting-code-to-python-3-with-2to3.html
* http://www.senktec.com/2013/06/ruby-vs-python/
* [One way to do it? (Ruby vs Python)](http://www.senktec.com/2013/09/one-way-to-do-it/)
* http://www.slideshare.net/DrTrucho/python-solid
* http://www.youtube.com/watch?v=PvMDPYSlki4

#### Resources

* http://josepferrandiz.blogspot.com/2012/12/bigger-data-with-ruby-python-and-r.html

#### Rubyist

* http://batsov.com/articles/2013/09/03/a-couple-of-useful-extensions-to-rubys-enumerable-module/
* http://batsov.com/articles/2013/09/26/the-elements-of-style-in-ruby-number-11-invoking-lambdas-slash-procs/
* http://batsov.com/articles/2013/12/04/using-rubys-each-with-object/
* http://batsov.com/articles/2014/02/17/the-elements-of-style-in-ruby-number-13-length-vs-size-vs-count/
