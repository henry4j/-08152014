##### Software Engineering Puzzle Binge

* movie theater seating
* escaping from velociraptors
* [liar, liar](http://www.kelvinjiang.com/2010/09/facebook-puzzles-and-liar-liar.html) -- 2-color graph
* [gattaca](http://www.kelvinjiang.com/2010/10/facebook-puzzles-gattaca.html) -- DP

```ruby
map = ->(n) { 
  [ # maps n ranges to a max gain.
    ranges[n-1].prize + possibles[n-1] ? map.(possibles[n-1]+1) : 0, 
    map.(n-1)
  ].max
}
```
* small world -- kd-tree
* hacker cup http://clickedyclick.blogspot.com/2011/01/facebook-haker-cup-round-1a-that-wasnt.html