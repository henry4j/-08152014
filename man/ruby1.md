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
#### Build a model & evaluate it using the non-other dataset (91.5% accuracy)

corpus='rrc_pro_5286_c'

[ ! -e $corpus.csv ] && s3cmd get s3://${S3_BUCKET}-private/resources/$corpus.csv $corpus.csv

ruby -e '
  srand 1234;
  l = ARGF.readlines; 
  r = 0...(n = l.size);
  r.each { |i| j = i + rand(n - i); l[i], l[j] = l[j], l[i] };
  puts l' $corpus.csv | tee $corpus-r.csv

corpus=$corpus-r

ruby -E windows-1250 -ne 'puts $_.split(",").values_at(1, 2).join(";")' $corpus.csv |
  tokenize | tee $corpus.tokens

ruby -E windows-1250 -ne 'puts $_.split(",")[3].chomp' $corpus.csv | ruby -ne '
  BEGIN{
    %w{open-uri json}.each { |e| require e }
    l = JSON[open("https://goo.gl/HLT94O").read];
    l = l.each_with_index.reduce({}) { |h, (e, i)| h[e] = i; h }
  }; 
  puts l[$_.chomp]' | tee $corpus.label_ids

paste -d ',' $corpus.label_ids $corpus.tokens |
  ruby -pe '$_ = $_.split(",").join(" | ")' |
  tee $corpus-vw.in
  
vw --oaa 24 --ngram 2 $corpus-vw.in -f $HOME/Downloads/$corpus.model
vw -t -i $HOME/Downloads/$corpus.model $corpus-vw.in -p $corpus-vw.out

paste $corpus.label_ids $corpus-vw.out |
  ruby -ane 'BEGIN{c = 0}; c += 1 if $F[0].to_i == $F[1].to_i; END{p c/(`wc -l $corpus.csv`.to_f)}' # 91.5%

s3cmd put $HOME/Downloads/$corpus.model s3://${S3_BUCKET}-private/resources
```

```bash
#### Build a model & evaluate it using the non-other dataset, and then the all dataset (77.5% accuracy)

corpus='rrc_pro_5286_2764_c'

[ ! -e $corpus.csv ] && s3cmd get s3://${S3_BUCKET}-private/resources/$corpus.csv $corpus.csv

ruby -E windows-1250 -ne 'puts $_.split(",").values_at(1, 2).join(";")' $corpus.csv |
  tokenize | tee $corpus.tokens

ruby -E windows-1250 -ne 'puts $_.split(",")[3].chomp' $corpus.csv |
  tee $corpus.labels

ruby -ne '
  BEGIN{
    %w{open-uri json}.each { |e| require e }
    l = JSON[open("https://goo.gl/HLT94O").read];
    l = l.each_with_index.reduce({}) { |h, (e, i)| h[e] = i; h }
  }; 
  puts l[$_.chomp]' $corpus.labels | tee $corpus.label_ids

paste -d ',' $corpus.label_ids $corpus.tokens |
  ruby -pe '$_ = $_.split(",").join(" | ")' |
  tee $corpus-vw.in

vw -t -i rrc_pro_5286_r.model rrc_pro_5286_2764_c_vw.in -r rrc_pro_5286_2764_c_vw.raw

ruby -ane '
  BEGIN{
    def sigmoid(x) 1/(1+Math.exp(-x)) end; 
    def normalize(a) s = a.reduce(:+); a.map { |e| e/s } end
  };
  p normalize($F.map { |e| sigmoid(e.split(":")[1].to_f) })' rrc_pro_5286_2764_c_vw.raw |
  tee rrc_pro_5286_2764_c_vw.norm

ruby -ne 'ei = eval($_).each_with_index.max; p ei[0] > 0.071 ? ei[1] + 1 : 0' rrc_pro_5286_2764_c_vw.norm |
  tee rrc_pro_5286_2764_c_vw.out

paste rrc_pro_5286_2764_c_label_ids.txt rrc_pro_5286_2764_c_vw.out |
  ruby -ane 'BEGIN{c = 0}; c += 1 if $F[0].to_i == $F[1].to_i; END{p c/(`wc -l rrc_pro_5286_2764_c.csv`.to_f)}' # 77.5%
```

```bash
corpus='rrc_pro_oct_nov'

ruby -E windows-1250 -ne 'puts $_.split(",").values_at(4, 11).join(";")' $corpus.csv |
  tokenize | tee $corpus.tokens

ruby -pe '$_ = " | " + $_' $corpus.tokens | tee $corpus-vw.in

[ ! -e $HOME/Downloads/rrc_pro_5286_c-r.model ] &&
  s3cmd get s3://${S3_BUCKET}-private/resources/rrc_pro_5286_c-r.model $HOME/Downloads/rrc_pro_5286_c-r.model

vw -t -i $HOME/Downloads/rrc_pro_5286_c-r.model $corpus-vw.in -r $corpus-vw.raw

ruby -ane '
  BEGIN{
    def sigmoid(x) 1/(1+Math.exp(-x)) end; 
    def normalize(a) s = a.reduce(:+); a.map { |e| e/s } end
  };
  p normalize($F.map { |e| sigmoid(e.split(":")[1].to_f) })' $corpus-vw.raw |
  tee $corpus-vw.norm

ruby -ne 'ei = eval($_).each_with_index.max; p ei[0] > 0.071 ? ei[1] + 1 : 0' $corpus-vw.norm |
  tee $corpus-vw.out

ruby -ne '
  BEGIN{
    %w{open-uri json}.each { |e| require e }
    l = JSON[open("https://goo.gl/HLT94O").read];
  }; 
  puts l[$_.chomp.to_i]' \
  $corpus-vw.out | tee $corpus-vw.labels
```
