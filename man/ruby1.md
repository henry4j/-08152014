Reference: http://benoithamelin.tumblr.com/ruby1line

```ruby
ruby -ne 'print; puts' # double-spaces lines
ruby -ne 'print; puts unless ~/^$/' # double-spaces non-blank lines

ruby -pe 'print $<.file.lineno, "\t"' # precedes file line #.
ruby -pe 'print $., "\t"' # precedes line # precedes overall line #.

ruby -pe 'printf "%5d: ", $.' # numbers lines at left, right-aligned.
ruby -ne 'BEGIN{$n=0}; if ~/^$/; print; else $n += 1; printf "%5d: %s", $n, $_; end' # numbers non-blank lines.

ruby -ane 'puts $F.map(&:to_i).reduce(:+)' # sums fields of each line.
ruby -ane 'puts $F.map(&:to_i).map(&:abs).join(" ")' # changes to absolute values.

```
