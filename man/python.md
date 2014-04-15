##### Python Tricks

```python
# In Ruby, def fib(n, memo={0=>1, 1=>1}) memo[n] ||= fib(n-1, memo) + fib(n-2, memo) if n >= 0 end
fib = lambda n, memo={0:1, 1:1}: n >= 0 and (memo.get(n) or memo.setdefault(n, fib(n-1, memo) + fib(n-2, memo))) or None
fib(-1) == None

fib = lambda n, memo={0:1, 1:1}: memo.get(n) or memo.setdefault(n, fib(n-1, memo) + fib(n-2, memo)) if n >= 0 else None
fib(-1) == None

def fib(n):
  prev = 1; curr = 1
  for i in range(1, n):
    next = curr + prev
    prev, curr = curr, next
  return curr
fib(0)
fib(1)
fib(2)
fib(3)
```

```python
# list comprehension
[i + 1 for i in a if i % 3 == 0] # select and map
[i for i in [i + 1 for i in a] if i % 3 == 0] # map and select
```

```python
sorted_items = sorted(items, key=lambda item: item['width'])
```

```python
' '.join([word.capitalize() for word in reversed(sentence.split())])
```

```python
letters = ['a', 'b', 'c']
[a.capitalize() for a in letters]

capitalize = lambda x: x.capitalize()
list(map(capitalize, letters))

list(map(str.capitalize, ['a', 'b', 'c']))
```

```python
s = 'string-with-palindromes-like-abbalabba'
l = len(s)
[s[x:y] for x in range(l) for y in range(x,l+1) if p(s[x:y])] 
```

##### Python Core

* http://eradman.com/posts/ruby-python.html
* http://grschafer.com/guides/2013/08/20/ruby-and-python-by-example/
* http://refcardz.dzone.com/refcardz/core-python
* [Code like Pythonista](http://speakerdeck.com/astreal/best-practices-and-coding-style-for-python)
* [Python for Ruby Programmer](http://speakerdeck.com/mleone/python-for-ruby-programmers)
* [The 'ugliness' of Python](http://wit.io/posts/the-ugliness-of-python)
* [Port to Python 3](http://www.diveintopython3.net/porting-code-to-python-3-with-2to3.html)
* [Ruby vs Python](http://www.senktec.com/2013/06/ruby-vs-python/)
* [One way to do it? (Ruby vs Python)](http://www.senktec.com/2013/09/one-way-to-do-it/)
* [SOLID principles in Python](http://www.slideshare.net/DrTrucho/python-solid)
* http://www.youtube.com/watch?v=PvMDPYSlki4

#### Resources

* http://josepferrandiz.blogspot.com/2012/12/bigger-data-with-ruby-python-and-r.html

#### Rubyist

* http://batsov.com/articles/2013/09/03/a-couple-of-useful-extensions-to-rubys-enumerable-module/
* http://batsov.com/articles/2013/09/26/the-elements-of-style-in-ruby-number-11-invoking-lambdas-slash-procs/
* http://batsov.com/articles/2013/12/04/using-rubys-each-with-object/
* http://batsov.com/articles/2014/02/17/the-elements-of-style-in-ruby-number-13-length-vs-size-vs-count/
