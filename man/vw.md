<sup>[VW wiki](https://github.com/JohnLangford/vowpal_wabbit/wiki)</sup>

##### install VW

```bash
brew install boost
git clone git://github.com/JohnLangford/vowpal_wabbit.git
cd vowpal_wabbit
# git checkout -b v7.0
make
make install
vw --version
```

##### script VW and R

```bash
#### Build a model & evaluate it using the non-other dataset (91.5% accuracy)

export corpus='rrc_pro_6215'

[ ! -e $corpus.csv ] && s3cmd get s3://${S3_BUCKET}-private/resources/$corpus.csv $corpus.csv

ruby -e '
  srand 1234;
  l = ARGF.readlines; 
  r = 0...(n = l.size);
  r.each { |i| j = i + rand(n - i); l[i], l[j] = l[j], l[i] };
  puts l' $corpus.csv | tee $corpus-r.csv

export corpus=$corpus-r

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

s3cmd put $HOME/Downloads/$corpus.model s3://${S3_BUCKET}-private/resources/

:)
```

```bash
#### Build a model & evaluate it using the non-other dataset, and then the all dataset (77.5% accuracy)

export corpus='rrc_pro_5286_2764_c'

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

paste $corpus.label_ids $corpus-vw.out |
  ruby -ane 'BEGIN{c = 0}; c += 1 if $F[0].to_i == $F[1].to_i; END{p c/(`wc -l $corpus.csv`.to_f)}' # 77.5%
  
:)
```

```bash
export corpus='rrc_pro_oct_nov'

[ ! -e $corpus.csv ] && s3cmd get s3://${S3_BUCKET}-private/resources/$corpus.csv $corpus.csv

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

ruby -ne 'ei = eval($_).each_with_index.max; puts ei[0] > 0.071 ? ei[1] + 1 : 0' $corpus-vw.norm |
  tee $corpus-vw.out

ruby -ne 'ei = eval($_).each_with_index.max; puts "%f,%d" % [ei[0], ei[0] > 0.071 ? ei[1] + 1 : 0]' $corpus-vw.norm |
  tee $corpus-vw-label-ids.csv

ruby -F, -ane '
  BEGIN{
    %w{open-uri json}.each { |e| require e }
    l = JSON[open("https://goo.gl/HLT94O").read];
  };
  puts 1 == $. ? "p_max,labeled" : "%f,%s" % [$F[0], l[$F[1].to_i]]' $corpus-vw-label-ids.csv | 
  tee $corpus-vw-labels.csv

grep -v Others $corpus-vw-labels.csv | tee $corpus-vw-non-other-labels.csv

# 7569 cases := 4299 top + 3270 other questions.
```

```
> vw1 <- read.csv('/tmp/exp/rrc_pro_oct_nov-vw-labels.csv', head = TRUE)
> summary(vw1)
                                             label          p_max        
 Others                                         :3270   Min.   :0.04371  
 What is your return/refund policy?             : 835   1st Qu.:0.05900  
 How do cancellations work?                     : 727   Median :0.07870  
 What do I do if I suspect customer fraud?      : 528   Mean   :0.10666  
 How do I verify if a refund has been processed?: 335   3rd Qu.:0.12049  
 What do I do if the item is undeliverable?     : 331   Max.   :0.92382  
 (Other)                                        :1543                    

> quantile(vw1$p_max, c(0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95))
      25%       50%       75%       80%       85%       90%       95% 
0.0589990 0.0786950 0.1204930 0.1359842 0.1543038 0.1870690 0.2639390 

> library('ggplot2') # requires `install.packages('ggplot2')`
> qplot(data=vw1, x=p_max*100, main="histogram of p_max", binwidth=1)
```

![](https://dl.dropboxusercontent.com/u/47820156/img/p_max.png "p_max histogram")

```
> vw2 <- read.csv('/tmp/exp/rrc_pro_oct_nov-vw-non-other-labels.csv', head = TRUE)
> summary(vw2)
                                             label          p_max        
 What is your return/refund policy?             : 835   Min.   :0.07105  
 How do cancellations work?                     : 727   1st Qu.:0.08859  
 What do I do if I suspect customer fraud?      : 528   Median :0.11213  
 How do I verify if a refund has been processed?: 335   Mean   :0.14409  
 What do I do if the item is undeliverable?     : 331   3rd Qu.:0.15857  
 How do I process a return/refund?              : 233   Max.   :0.92382  
 (Other)                                        :1310                    

> quantile(vw2$p_max, c(0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95))
      25%       50%       75%       80%       85%       90%       95% 
0.0885895 0.1121340 0.1585715 0.1771966 0.2038631 0.2469032 0.3283119 

> library('ggplot2') # requires `install.packages('ggplot2')`
> qplot(data=vw2, x=p_max*100, main="histogram of p_max", binwidth=1)
```

![](https://dl.dropboxusercontent.com/u/47820156/img/non_other_p_max.png "p_max histogram")

#### Build a model & evaluate it using the non-other dataset (91.5% accuracy)

```
export corpus='rrc_pro_6215'

[ ! -e $corpus.csv ] && s3cmd get s3://${S3_BUCKET}-private/resources/$corpus.csv $corpus.csv

ruby -e '
  srand 1234;
  l = ARGF.readlines; 
  r = 0...(n = l.size);
  r.each { |i| j = i + rand(n - i); l[i], l[j] = l[j], l[i] };
  puts l' $corpus.csv | tee $corpus-r.csv

export corpus=$corpus-r

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

# vw --oaa 24 --ngram 2 $corpus-vw.in -f $HOME/Downloads/$corpus.model
train-sgd -k 24 -w 2048 $corpus-vw.in -m $HOME/Downloads/$corpus.sgd

vw -t -i $HOME/Downloads/$corpus.sgd $corpus-vw.in -p $corpus-vw.out

paste $corpus.label_ids $corpus-vw.out |
  ruby -ane 'BEGIN{c = 0}; c += 1 if $F[0].to_i == $F[1].to_i; END{p c/(`wc -l $corpus.csv`.to_f)}' # 91.5%

s3cmd put $HOME/Downloads/$corpus.sgd s3://${S3_BUCKET}-private/resources/

:)
```
