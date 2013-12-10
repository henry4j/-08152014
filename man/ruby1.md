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

ruby -e '
  srand 1234;
  l = ARGF.readlines; 
  r = 0...(n = l.size);
  r.each { |i| j = i + rand(n - i); l[i], l[j] = l[j], l[i] };
  puts l' rrc_pro_5286_c.csv | tee rrc_pro_5286_r.csv

ruby -E windows-1250 -ane 'BEGIN{$; = ","; $, = "; "}; puts $F[1,2].join' rrc_pro_5286_r.csv |
  tokenize | tee rrc_pro_5286_r_tokens.txt

ruby -E windows-1250 -ane 'BEGIN{$; = ","}; puts $F[3].chomp' \
  rrc_pro_5286_r.csv | tee rrc_pro_5286_r_labels.txt

curl -o /tmp/rrc_pro_25_labels.json -ksL http://goo.gl/HLT94O

ruby -ne 'BEGIN{
  %w{open-uri json}.each { |e| require e }
  l = JSON[open("/tmp/rrc_pro_25_labels.json").read];
  l = l.each_with_index.reduce({}) { |h, (e, i)| h[e] = i; h }
}; puts l[$_.chomp]' \
  rrc_pro_5286_r_labels.txt | tee rrc_pro_5286_r_label_ids.txt

paste -d ',' rrc_pro_5286_r_label_ids.txt rrc_pro_5286_r_tokens.txt |
  ruby -ape 'BEGIN{$; = ","; $, = " | "}; $_ = $F.join' |
  tee rrc_pro_5286_r_vw.in
  
vw --oaa 24 --ngram 2 rrc_pro_5286_r_vw.in -f rrc_pro_5286_r.model
vw -t -i rrc_pro_5286_r.model rrc_pro_5286_r_vw.in -p rrc_pro_5286_r_vw.out

paste rrc_pro_5286_r_label_ids.txt rrc_pro_5286_r_vw.out |
  ruby -ane 'BEGIN{c = 0}; c += 1 if $F[0].to_i == $F[1].to_i; END{p c/(`wc -l rrc_pro_5286_r.csv`.to_f)}' # 91.5%
```

```bash
#### Build a model & evaluate it using the non-other dataset, and then the all dataset (77.5% accuracy)

ruby -E windows-1250 -ane 'BEGIN{$; = ","; $, = "; "}; puts $F[1,2].join' rrc_pro_5286_2764_c.csv |
  tokenize | tee rrc_pro_5286_2764_c_tokens.txt

ruby -E windows-1250 -ane 'BEGIN{$; = ","}; puts $F[3].chomp' \
  rrc_pro_5286_2764_c.csv | tee rrc_pro_5286_2764_c_labels.txt

curl -o /tmp/rrc_pro_25_labels.json -ksL http://goo.gl/HLT94O

ruby -ne '
  BEGIN{
    %w{open-uri json}.each { |e| require e }
    l = JSON[open("/tmp/rrc_pro_25_labels.json").read];
    l = l.each_with_index.reduce({}) { |h, (e, i)| h[e] = i; h }
  }; 
  puts l[$_.chomp]' \
  rrc_pro_5286_2764_c_labels.txt | tee rrc_pro_5286_2764_c_label_ids.txt

paste -d ',' rrc_pro_5286_2764_c_label_ids.txt rrc_pro_5286_2764_c_tokens.txt |
  ruby -ape 'BEGIN{$; = ","; $, = " | "}; $F[0] = "" if $F[0] == "0"; $_ = $F.join' |
  tee rrc_pro_5286_2764_c_vw.in

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
corpus='rrc_pro_q3_2013'
# corpus='rrc_pro_5286_2764_c'

ruby -E windows-1250 -ane 'BEGIN{$; = ","; $, = "; "}; puts $F[1,2].join' $corpus.csv |
  tokenize | tee $corpus.tokens

ruby -pe '$_ = " | " + $_' $corpus.tokens | tee $corpus-vw.in

curl -o /tmp/rrc_pro_5286_r.model -ksL http://goo.gl/yyUfTe

vw -t -i /tmp/rrc_pro_5286_r.model $corpus-vw.in -r $corpus-vw.raw

ruby -ane '
  BEGIN{
    def sigmoid(x) 1/(1+Math.exp(-x)) end; 
    def normalize(a) s = a.reduce(:+); a.map { |e| e/s } end
  };
  p normalize($F.map { |e| sigmoid(e.split(":")[1].to_f) })' $corpus-vw.raw |
  tee $corpus-vw.norm

ruby -ne 'ei = eval($_).each_with_index.max; p ei[0] > 0.071 ? ei[1] + 1 : 0' $corpus-vw.norm |
  tee $corpus-vw.out

curl -o /tmp/rrc_pro_25_labels.json -ksL http://goo.gl/HLT94O

ruby -ne '
  BEGIN{
    %w{open-uri json}.each { |e| require e }
    l = JSON[open("/tmp/rrc_pro_25_labels.json").read];
  }; 
  puts l[$_.chomp.to_i]' \
  $corpus-vw.out | tee $corpus-vw.labels
```
