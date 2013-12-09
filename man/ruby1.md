Reference:
* http://benoithamelin.tumblr.com/ruby1line
* http://zenspider.com/Languages/Ruby/QuickRef.html#pre-defined-variables
* https://gist.github.com/KL-7/1590797
* https://github.com/JoshCheek/Play/blob/master/ruby-one-liners/Readme.md
* http://www.ruby-doc.org/core-1.9.2/ARGF.html

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

```bash
# ruby -E windows-1250 -ane 'BEGIN{$; = ","}; p $F[3].chomp' rrc_pro.csv | sort -f | uniq | tee rrc_pro_labels.json

ruby -E windows-1250 -ane 'BEGIN{$; = ","}; puts $F[1,2].join("; ")' rrc_pro_5286_2764_c.csv > rrc_pro_5286_2764_c.txt
tokenize | tee rrc_pro_5286_2764_c_tokens.txt

ruby -E windows-1250 -ane 'BEGIN{$; = ","}; puts $F[3].chomp' \
  rrc_pro_5286_2764_c.csv | tee rrc_pro_5286_2764_c_labels.txt

ruby -ne 'BEGIN{
  %w{open-uri json}.each { |e| require e }
  l = JSON[open("https://raw.github.com/henry4j/-/master/paste/rrc_pro_25_labels.json").read];
  l = l.each_with_index.reduce({}) { |h, (e, i)| h[e] = i; h }
}; puts l[$_.chomp]' \
  rrc_pro_5286_2764_c_labels.txt | tee rrc_pro_5286_2764_c_label_ids.txt
```
