<sup>[VW](https://github.com/henry4j/-/blob/master/man/vw.md), ["true" L-LDA](https://github.com/henry4j/-/blob/master/man/l-lda.md), [NB](http://chimpler.wordpress.com/2013/03/13/using-the-mahout-naive-bayes-classifier-to-automatically-classify-twitter-messages/), [SGD](https://github.com/henry4j/-/blob/master/man/sgd.md), [ML-workflow](ml-workflow.md), [text-mining/twitter](http://practicalquant.blogspot.com/2010/04/text-mining-and-twitter.html)</sup>

### ML SDE requirement -- <sub>[1](http://www.amazon.com/gp/jobs/232194/), [2](http://www.amazon.com/gp/jobs/180727/), [3](https://jobs.groupon.com/careers/engineering/software-development-engineer-senior-goods-seattle-wa-united-states/), [4](https://twitter.com/jobs/positions?jvi=oSAqXfwz,Job), [5](https://twitter.com/jobs/positions?jvi=oTAqXfwA,Job), [6](http://www.linkedin.com/jobs?viewJob=&jobId=6907673), [7](http://www.linkedin.com/jobs?viewJob=&jobId=6865716)</sub>

* ML basics: Supervised- & Unsupervised learning, Collaborative filtering, TF/TFIDF, n-gram, tokenizer, vectorizer, ...
* Cloud Computing/SOA patterns/Java Concurrency: 
  * Hadoop/<sub>Map Reduce/HDFS/Pig/Hive</sub>, Mahout/R, Lucene/Solr/<sub>search</sub>, ZooKeeper/<sub>distributed coordination</sub>, ...
  * Amazon EMR/SWF/SQS/DynamoDB/S3, ORM/noSQL, Barrier/AQS/PQ, Leader Election/<sub>Paxos</sub>, ...
* Scripting/open source tools: GIT/Subversion, Maven/POM/Eclipse, awk/sed, Shell/Ruby/Python, Scala/JRuby/Jython, ...
* How to formulate ML problems? How to parallelize ML algorithms? How to select or reduce features? For your reading:  
[![x](http://akamaicovers.oreilly.com/images/0636920028529/thumb.gif)](http://www.amazon.com/Doing-Data-Science-Straight-Frontline/dp/1449358659/)
![x](http://www.manning.com/holmes/holmes_cover150.jpg)
![x](http://www.manning.com/ingersoll/ingersoll_cover150.jpg)
![x](http://www.manning.com/owen/owen_cover150.jpg)
![x](http://www.manning.com/pharrington/pharrington_cover150.jpg)

### Table of Contents / Machine Learning SDE

* Set up machine learning SDE environment (Hadoop & Mahout)
* Build LDA models from a corpus using Apache Mahout
* Test and evaluate LDA models for topic matching
* Under Construction - to be wired
* Discussions

### Set up ML SDE environment on Mac OSX <sub>(forked from [Android bootcamp](https://github.com/henry4j/-/blob/master/man/android-bootcamp.mkd))</sub>

#### Install Xcode, Command Line Tools, and then [homebrew](http://brew.sh/)

* Xcode, or CLI tools
  * Command Line tools only https://developer.apple.com/downloads/index.action?=command%20line%20tools
     * Install FileMerge w/ minimal Xcode
         * `sudo bash < <(curl -ksL http://raw.github.com/henry4j/-/master/paste/install_filemerge)`
  * or, Xcode -- Launch **[App Store]** | Search by **'Xcode'** | **[FREE]** and then **[Install APP]**
     * Applications | Xcode | Install | Start Using Xcode  
       Xcode | **Preference...** | Downloads | Install **[Command Line Tools]**
* `ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go/install)"` # at the terminal.

#### Set up password-less SSH and optional git repository accounts

* Setup [passphraseless ssh](http://hadoop.apache.org/docs/stable/single_node_setup.html#Setup+passphraseless)
  * `ssh-keygen -t rsa`; `cat "$HOME/.ssh/id_rsa.pub" >> "$HOME/.ssh/authorized_keys"`
* System Preferences | Sharing | 'Remote Login' service | All users and then lock the change(?)
* `ssh localhost` # should not require a password.
* `cat "$HOME/.ssh/id_rsa.pub"` # good for [github](http://github.com/) and [assembla](http://assembla.com/) git repositories.
  * http://www.github.com → Account Settings → SSH Public Keys → Add SSH Public key
  * http://www.assembla.com → Edit Profile → Manage SSH Keys → Add key

#### Install CLI utilities, and Ruby environments

```bash
brew install git ruby jruby rbenv ruby-build maven colordiff wget unrar p7zip s3cmd
sudo gem install test-unit debugger rake
rbenv install 1.9.3-p448 && rbenv global 1.9.3-p448

echo '[ -d $HOME/.rbenv ] && eval "$(rbenv init -)"' >> "$HOME/.profile"
curl -o "$HOME/.irbrc" -ksL http://raw.github.com/henry4j/-/master/paste/.irbrc
```

##### Install Mac VIM (optional)

```bash
curl -o /tmp/MacVim-snapshot-72.tbz -kL https://github.com/b4winckler/macvim/releases/download/snapshot-72/MacVim-snapshot-72-Mavericks.tbz
tar xvf /tmp/MacVim-snapshot-72.tbz -C /tmp/
mv /tmp/MacVim-snapshot-72/MacVim.app /Applications/
curl -o "$HOME/.vim/colors/molokai.vim" -ksL http://www.vim.org/scripts/download_script.php?src_id=9750 --create-dirs

cat <<EOF >> "$HOME/.profile"
alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'
alias less=/Applications/MacVim.app/Contents/Resources/vim/runtime/macros/less.sh
EOF
```

#### Set up workspaces for hadoop and mahout, and S3 credentials

##### CHECKLIST

[x] `ruby -v # must be 1.9.3, 2.0.0, or newer`  
[x] `jruby -v # must be 1.7.4, or newer`  
[x] DO get S3 creds from https://console.aws.amazon.com/iam/home?#security_credential

```bash
s3cmd --configure # or, curl -o "$HOME/.s3cfg" -kL http://tiny/fuyiy5sx/impramazs3fis3getext # within our corp. net.
```

```bash
cat <<EOF >> "$HOME/.profile"

[ ! -e "$HOME/.s3bucket" ] && curl -o "$HOME/.s3bucket" -ksL http://tiny/1bbxr2hio/impramazs3fis3getext
export S3_BUCKET=$(<"$HOME/.s3bucket")
export HADOOP_BASE=$(ruby -e "puts File.dirname(File.dirname(File.realpath(%x(which hadoop).chomp)))")
export HADOOP=$HADOOP_BASE/bin/hadoop
export MAHOUT_BASE=/workspace/mahout
export MAHOUT=$MAHOUT_BASE/bin/mahout
export PATH=$MAHOUT_BASE/bin:$PATH

export HADOOP_WORK=/workspace/hadoop-work
export MAHOUT_WORK=/workspace/mahout-work
[ ! -d $MAHOUT_WORK ] && mkdir -p $MAHOUT_WORK
export PATH=$MAHOUT_WORK:$PATH
EOF

source "$HOME/.profile"
```

```bash
sudo mkdir /workspace; sudo chown $USER /workspace
```

#### Install Hadoop <sub>(requires passphraseless ssh)</sub>, <sub>see also [Hadoop on Ubuntu Linux](http://www.michael-noll.com/tutorials/running-hadoop-on-ubuntu-linux-single-node-cluster/)</sub>

* Setup `libexec/conf/hadoop-env.sh` and [pseudo-distributed operation](http://hadoop.apache.org/docs/r1.2.1/single_node_setup.html#PseudoDistributed)
* Bring up the [name node](http://localhost:50070/) and [job tracker](http://localhost:50030/) (n/a if running locally)
* Try-upload & download local files to DFS by [`hadoop dfs -put`](http://hadoop.apache.org/docs/stable/file_system_shell.html#put), [`hadoop dfs -get`](http://hadoop.apache.org/docs/stable/file_system_shell.html#get), and then [`hadoop dfs -ls`](http://hadoop.apache.org/docs/stable/file_system_shell.html#ls)

```bash
brew install hadoop
export HADOOP_BASE=$(ruby -e "puts File.dirname(File.dirname(File.realpath(%x(which hadoop).chomp)))")
curl -o "$HADOOP_BASE/libexec/conf/#1" \
  -kL 'http://raw.github.com/henry4j/-/master/paste/{hadoop-env.sh,core-site.xml,hdfs-site.xml,mapred-site.xml}'
rm -rf $HADOOP_WORK; mkdir -p $HADOOP_WORK
stop-all.sh; hadoop namenode -format
```

```bash
start-all.sh # starts up hadoop daemons: name, data, 2nd name nodes, and job & task tracker.
ps aux | grep hadoop | grep -o 'org.apache.[^ ]\+$' # sees 3 lines for hadoop daemons.
hadoop jar $HADOOP_BASE/libexec/hadoop-examples-1.2.1.jar pi 10 100 # same as ruby -e 'p Math::PI'
```

```bash
cat <<EOF >> /usr/local/bin/restart-all
#!/bin/bash
stop-all.sh
ps -ef | grep 'org.apache.hadoop.[^ ]\+$' | ruby -ane 'puts $F[1]' | xargs kill
start-all.sh; hadoop dfsadmin -safemode leave
EOF
chmod +x /usr/local/bin/restart-all
```

#### Install [Mahout 0.8 Release](http://search.maven.org/#search%7Cga%7C1%7Cmahout)

```bash
v='0.8'; e="mahout-distribution-$v"
[ ! -e "$HOME/Downloads/$e.zip" ] &&
  curl -o "$HOME/Downloads/$e.zip" -kL "http://mirrors.ibiblio.org/apache/mahout/$v/$e.zip"
rm -rf "/workspace/$e" && unzip -o "$HOME/Downloads/$e.zip" -d /workspace/
rm -rf /workspace/mahout && ln -sf "/workspace/$e" /workspace/mahout
curl -o "$MAHOUT_BASE/bin/#1" -kL 'http://raw.github.com/henry4j/-/master/paste/{mahout,mahout-d}'
chmod +x $MAHOUT_BASE/bin/mahout*
```

#### Checkout Mahout 0.9 Source (optionally, the last stable build)

```bash
svn checkout http://svn.apache.org/repos/asf/mahout/trunk /workspace/mahout-trunk
svn checkout http://svn.apache.org/repos/asf/mahout/tags/mahout-0.8 /workspace/mahout-tags/0.8
```

#### Set up utilities for Mahout work

* [prep-comm-text](http://github.com/henry4j/-/blob/master/paste/prep-comm-text), [stop-comm-text](http://github.com/henry4j/-/blob/master/paste/stop-comm-text) -- to prep a corpus.
* [resplit](http://github.com/henry4j/-/blob/master/paste/resplit) -- to resplit splits into 1 that is easier to put into Amazon S3.
* [p-topics](http://github.com/henry4j/-/blob/master/paste/p-topics) -- to see topic probability distribution of an arbitrary text document against a model.

```bash
for e in prep-comm-text stop-comm-text prep-corpus split-comm-text resplit \
    pp-w,z pp-z,d p-topics p-topics.rb vectors.rb exam-comm-text \
    tame-hadoop tame-corpus tame-topics tame-topics-l; do
  curl -o /usr/local/bin/$e -kL https://raw.github.com/henry4j/-/master/paste/$e;
  chmod +x /usr/local/bin/$e;
done
```

#### ~~Set up utilities for Mahout work~~ -- no longer in use.

```bash
for e in prep-comm-text stop-comm-text prep-corpus split-comm-text resplit \
    pp-w,z pp-z,d p-topics p-topics.rb vectors.rb exam-comm-text \
    tame-hadoop tame-corpus tame-topics tame-topics-l; do
  rm -f /usr/local/bin/$e; ln -s /workspace/gits/henry4j/paste/$e /usr/local/bin/$e;
done
```

### Building LDA models

* three macro steps: `tame-hadoop`, `tame-corpus`, and `tame-topics`
  * feature selection/reduction: [`stop-phrases`](http://raw.github.com/henry4j/-/master/paste/stop-comm-text), and [`text analyzer`](https://github.com/henry4j/-/blob/master/sources/text/src/main/java/com/henry4j/text/CommTextAnalyzer.java) (w/ [english.stop](http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop))
  * series of LDA tune ups w/ max n-gram and LLR, min & max DF%, [perflexity](http://en.wikipedia.org/wiki/Perplexity), and the like.

#### [`tame-hadoop`](https://raw.github.com/henry4j/-/master/paste/tame-hadoop), or step-by-step at the terminal

* this script installs a custome text analyzer [`comm text analyzer`](http://github.com/henry4j/-/blob/master/sources/text/src/main/java/com/henry4j/text/CommTextAnalyzer.java), that builds a token stream through a pipeline of
  * html stripper char filter, standard tokenizer, lower case filter, stop-word filter for english, and porter-stemmer.
  * fyi: for n-gram analysis (n >= 1), [shingle filter](http://lucene.apache.org/core/4_0_0/analyzers-common/org/apache/lucene/analysis/shingle/ShingleFilter.html) takes this token stream and returns a token stream of max. n-grams.

```bash
#!/usr/bin/env jruby # called `tame-hadoop`
require 'rake' # sudo /usr/local/bin/gem install rake

def x!(*cmd, &blk) block_given? ? (sh cmd.join(' ') do |*a| blk.call(a) end) : (sh cmd.join(' ')) end

jar = 'text-1.0-SNAPSHOT.jar'
x! "curl -o ${HADOOP_BASE}/libexec/lib/#{jar} -ksL http://raw.github.com/henry4j/-/master/paste/#{jar}"
x! "ln -sf ${HADOOP_BASE}/libexec/lib/#{jar} ${MAHOUT_BASE}/lib/#{jar}"
 
x! 'stop-all.sh' do end # rescue on errors.
x! 'ps -ef | grep "org.apache.hadoop.[^ ]\+$" | ruby -ane "puts $F[1]" | xargs kill' do end
x! 'start-all.sh'
x! '$HADOOP dfsadmin -safemode leave' do end
```

#### [`tame-corpus 6`](http://raw.github.com/henry4j/-/master/paste/tame-corpus), or step-by-step at the terminal

* `6` is the id for the latest corpus or the excel spreadsheet.

```bash
#!/usr/bin/env jruby # called `tame-corpus`
require 'rake' # sudo gem install rake

sources = %w(rrc_pro_22110.csv rrc_ind_31771.csv rrc_pro_2285_labeled_1.0.csv rrc_pro_2285_labeled_0.75.csv rrc_pro_2285_others_1.0.csv rrc_pro_3055_799.csv rrc_pro_3492_876.csv)
corpora = %w(corpus-0.csv corpus-1.csv corpus-l-1.0.csv corpus-l-0.75.csv corpus-l-2285+others.csv corpus-3055+799.csv corpus-3492-876.csv).map { |e| File.join(ENV['MAHOUT_WORK'], e) }
options = ["-i 3 -f 4,11 -m 200", "-i 3 -f 4,11", "-i 4 -l 8 -f 1,3", "-i 4 -l 8 -f 1,3", "-i 0 -l 8 -f 4,5", "-i 0 -l 8 -f 4,5"]
extract = '${MAHOUT_WORK}/comm-text-ext'

def x!(*cmd, &blk) block_given? ? (sh cmd.join(' ') do |*a| blk.call(a) end) : (sh cmd.join(' ')) end

fail "'corpus' must be specified." unless ARGV[0]
c = Integer(ARGV[0]) rescue -1
unless c == -1
  x! "s3cmd get s3://${S3_BUCKET}-private/resources/#{sources[c]} #{corpora[c]}" unless File.exist?(corpora[c])
else
  corpora << ARGV[0]
  options << options[0]
end

x! "$HADOOP dfs -rmr #{extract} ${MAHOUT_WORK}/comm-text-seq" do end # rescue on errors.
x! "prep-comm-text #{corpora[c]} --out-dir #{extract} --excludes Others --overwrite #{options[c]}"
x! "$HADOOP dfs -put #{extract}/corpus ${MAHOUT_WORK}/comm-text-ext/corpus"
x! "$HADOOP dfs -put #{extract}/doc-topic-priors ${MAHOUT_WORK}/comm-text-ext/doc-topic-priors" if File.exist?("#{extract}/doc-topic-priors")
x! "$HADOOP dfs -put #{extract}/labels.json ${MAHOUT_WORK}/comm-text-ext/labels.json" if File.exist?("#{extract}/labels.json")
x! "$MAHOUT seqdirectory -i #{extract}/corpus -o ${MAHOUT_WORK}/comm-text-seq -ow -chunk 5"
```

* **see also:** `doc-topics` and `labels` that `tame-corpus 6` had yielded.

```bash
hadoop dfs -ls /workspace/mahout-work/comm-text-ext
Found n items
-rw-r--r--   1 hylee   74137 2013-10-15 09:35 /workspace/mahout-work/comm-text-ext/doc-topic-priors
-rw-r--r--   1 hylee     819 2013-10-15 09:35 /workspace/mahout-work/comm-text-ext/labels.json
```

* this is a similar script that compiles a corpus of reuter mail archives that is well-known for lucene benchmark program.

```bash
if [ ! -e ${MAHOUT_WORK}/reuters-ext ]; then
  if [ ! -e ${MAHOUT_WORK}/reuters-sgm ]; then
    if [ ! -f ${MAHOUT_WORK}/reuters21578.tar.gz ]; then
      echo "Downloading Reuters-21578"
      curl -o ${MAHOUT_WORK}/reuters21578.tar.gz -kL http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.tar.gz
    fi
    echo 'Extracting tar.gz'
    mkdir -p ${MAHOUT_WORK}/reuters-sgm
    tar xzf ${MAHOUT_WORK}/reuters21578.tar.gz -C ${WORK_DIR}/reuters-sgm
  fi

  # Safe to ignore WARN: driver.MahoutDriver:
  #   [No org.apache.lucene.benchmark.utils.ExtractReuters.props found on classpath]
  #   (http://lucene.apache.org/core/4_4_0/benchmark/org/apache/lucene/benchmark/utils/ExtractReuters.html)
  echo "Extracting reuters sgm"
  $MAHOUT org.apache.lucene.benchmark.utils.ExtractReuters ${MAHOUT_WORK}/reuters-sgm ${MAHOUT_WORK}/reuters-ext

  echo "Copying Reuters data to Hadoop"
  $HADOOP dfs -rmr ${MAHOUT_WORK}/reuters-ext ${MAHOUT_WORK}/reuters-seq || true
  $HADOOP dfs -put ${MAHOUT_WORK}/reuters-ext ${MAHOUT_WORK}/reuters-ext

  echo "Converting to Sequence Files"
  $MAHOUT seqdirectory -i ${MAHOUT_WORK}/reuters-ext -o ${MAHOUT_WORK}/reuters-seq -ow -c UTF-8 -chunk 5
fi
```

#### [`tame-topics`](https://raw.github.com/henry4j/-/master/paste/tame-topics) <sub>-o -w l-lda-6</sub>

* [!] this script is fragile, and subject to change without any notice; `*-advanced` script is coming soon.
* args: work-id (default: bigram-k), max-ngram (default: 2), LLR, and analyzer (default: CommTextAnalyzer)
* e.g. `tame-topics -o -w l-lda-6 # uses 22109 rows and 864 columns`; see [tame-topics-bigram.log](https://raw.github.com/henry4j/-/master/modeling/tame-topics-bigram.log).
* [?] [`mahout seq2sparse --help`](http://raw.github.com/henry4j/-/master/man/seq2sparse.mkd), [`mahout cvb --help`](https://raw.github.com/henry4j/-/master/man/l-lda-cvb.mkd)

```bash
#!/usr/bin/env jruby # called `tame-topics`
%w{rake optparse open-uri csv json open3}.each { |e| require e }

$options = {}
OptionParser.new do |p|
  p.on('-a', '--analyzer STRING', String, "Specifies the analyzer (default: 'com.henry4j.text.CommTextAnalyzer')") { |v| $options[:analyzer] = v }
  p.on('-k', '--min-llr INTEGER', Integer, 'Specifies the min-LLR (default: 120)') { |v| $options[:min_llr] = v }
  p.on('-g', '--max-ngram INTEGER', Integer, 'Specifies the max N gram (default: 1)') { |v| $options[:max_ngram] = v }
  p.on('-w', '--work-id STRING', String, "Specifies the topic modeling work id (default: 'true-l-lda')") { |v| $options[:work_id] = v }
  p.on('-o', '--overwrite', 'Whether to overwrite existing corpus and corpus-priors.') { |v| $options[:overwrite] = v }
end.parse!

analyzer = $options[:analyzer] || 'com.henry4j.text.CommTextAnalyzer'
min_llr = $options[:min_llr] || 120
max_ngram = $options[:max_ngram] || 1
work_id = $options[:work_id] || 'true-l-lda'
x! "$HADOOP dfs -rmr '${MAHOUT_WORK}/#{work_id}'" do end if $options[:overwrite]
vectors = $options[:vectors] || 'tf-vectors'

def x!(*cmd, &blk) block_given? ? (sh cmd.join(' ') do |*a| blk.call(a) end) : (sh cmd.join(' ')) end

def store_topic_term_priors(work_id, vectors)
  Vectors.write vectors, "#{ENV['MAHOUT_WORK']}/#{work_id}/topic-term-priors"
end

def doc_topic_priors_exist?
  %x($HADOOP dfs -ls ${MAHOUT_WORK}/comm-text-ext/doc-topic-priors) && 0 == $?
end

def load_doc_topic_priors(work_id)
  Vectors.read "#{ENV['MAHOUT_WORK']}/comm-text-ext/doc-topic-priors"
end

def load_doc_vectors(work_id)
  Vectors.read "#{ENV['MAHOUT_WORK']}/#{work_id}/matrix"
end

def patch_mahout
  %w(core-0.8 core-0.8-job examples-0.8 examples-0.8-job)
    .map { |e| File.join(ENV['MAHOUT_BASE'], "mahout-%s.jar" % e) }
    .each { |e| FileUtils.mv(e, "#{File.dirname(e)}/#{File.basename(e)}.bak") if File.exist?(e) }
  %w(core-0.8.2 core-0.8.2-job examples-0.8.2 examples-0.8.2-job)
    .map { |e| File.join(ENV['MAHOUT_BASE'], "mahout-%s.jar" % e) }
    .reject { |e| File.exist?(e) }
    .each { |e| x! 'curl -o %s -kL http://dl.dropbox.com/u/47820156/mahout/l-lda/%s' % [e, File.basename(e)] }
end

if %x($HADOOP dfs -test -e "${MAHOUT_WORK}/#{work_id}/matrix") && 0 != $?.exitstatus
  x! [
    'export HADOOP_CLASSPATH=${MAHOUT_BASE}/lib/text-1.0-SNAPSHOT.jar:${MAHOUT_BASE}/lib/lucene-analyzers-common-4.3.0.jar:${MAHOUT_BASE}/lib/lucene-core-4.3.0.jar',
    "$MAHOUT seq2sparse -i ${MAHOUT_WORK}/comm-text-seq/ -o ${MAHOUT_WORK}/#{work_id} -ow --namedVector -s 20 -md 10 -x 65 -ng %s -ml %s -a %s" % [max_ngram, min_llr, analyzer]
  ].join('; ') # excludes terms of 80- DF & 70+ DF%
  # -a org.apache.mahout.text.MailArchivesClusteringAnalyzer \
  # -a org.apache.lucene.analysis.en.EnglishAnalyzer \
  # -a org.apache.lucene.analysis.standard.StandardAnalyzer \

  x! "$MAHOUT rowid -i ${MAHOUT_WORK}/#{work_id}/#{vectors} -o ${MAHOUT_WORK}/#{work_id}"
  %w(df-count tf-vectors tfidf-vectors tokenized-documents).each { |e| x! "resplit ${MAHOUT_WORK}/#{work_id}/#{e}" do end }
  x! "$MAHOUT seqdumper -i ${MAHOUT_WORK}/#{work_id}/tokenized-documents-0 -o /tmp/#{work_id}-tokenized-documents.txt"
  x! "$HADOOP dfs -put /tmp/#{work_id}-tokenized-documents.txt ${MAHOUT_WORK}/#{work_id}/tokenized-documents.txt"
end

io = %w(matrix dictionary.file-0 model doc-topics modeling).map { |e| "$MAHOUT_WORK/#{work_id}/#{e}" }
x! "$HADOOP dfs -rmr #{io[-3..-1].join(' ')}" do end

if doc_topic_priors_exist? 
  require_relative 'vectors'
  doc_topic_priors = load_doc_topic_priors(work_id)
  doc_vectors = load_doc_vectors(work_id)
  rows, columns = doc_topic_priors[0].size, doc_vectors[0].size
  topic_term_priors = org.apache.mahout.math.SparseRowMatrix.new(rows, columns, true) # true for random access
  doc_vectors.each do |(d, v)|
    doc_topic_priors[d].non_zeroes.each do |z_d|
      row = topic_term_priors.view_row(z_d.index)
      v.non_zeroes.each { |w| row.set_quick(w.index, row.get_quick(w.index) + w.get * z_d.get) }
    end
  end
  store_topic_term_priors(work_id, topic_term_priors)
  x! "$HADOOP dfs -cp ${MAHOUT_WORK}/comm-text-ext/labels.json ${MAHOUT_WORK}/#{work_id}"
  x! "$HADOOP dfs -cp ${MAHOUT_WORK}/#{work_id}/topic-term-priors #{io[-1]}/model-0/part-r-00000" do end
  patch_mahout
  cvb_opts = "-k #{rows} -pidt -dtp ${MAHOUT_WORK}/comm-text-ext/doc-topic-priors -cd 6e-24"
else
  cvb_opts ='-k 20 -pidt -cd 6e-4'
end

x! "rm -rf ${MAHOUT_WORK}/#{work_id}/modeling" do end
x! "$MAHOUT cvb -i %s -dict %s -ow -o %s -dt %s -mt %s -x 35 -block 2 -tf 0.25 -seed 777 #{cvb_opts}" % io
x! "resplit #{io[2, 2].join(' ')}"

x! "$MAHOUT vectordump -i %s-0 -sort %s-0 -d %s -o /tmp/#{work_id}-w,z-dump.txt -p true -vs 25 -dt sequencefile" % io.values_at(-3, -3, 1)
x! "$MAHOUT vectordump -i %s-0 -o /tmp/#{work_id}-z,d-dump.txt" % io[-2]
x! "$HADOOP dfs -put /tmp/#{work_id}-*-dump.txt ${MAHOUT_WORK}/#{work_id}" do end
 
x! "pp-w,z /tmp/#{work_id}-w,z-dump.txt       | tee /tmp/#{work_id}-w,z-topic-terms.txt"
x! "pp-z,d /tmp/#{work_id}-z,d-dump.txt -n 30 | tee /tmp/#{work_id}-z,d-doc-topics.txt"
%w(w,z z,d).map { |e| x! "$HADOOP dfs -put /tmp/#{work_id}-#{e}-*.txt ${MAHOUT_WORK}/#{work_id}" do end }
 
x! "rm -rf ${MAHOUT_WORK}/#{work_id}"
x! "$HADOOP dfs -get $MAHOUT_WORK/#{work_id} ${MAHOUT_WORK}/#{work_id}"
%w(df-count-0 dictionary.file-0 model-0 labels.json tf-vectors-0 tfidf-vectors-0 tokenized-documents.txt topic-0).each { |e| x! "s3cmd put $MAHOUT_WORK/#{work_id}/#{e} s3://${S3_BUCKET}/#{work_id}/" }
%w(w,z-dump.txt w,z-topic-terms.txt doc-topics.txt z,d-dump.txt).each { |e| x! "s3cmd put $MAHOUT_WORK/#{work_id}/#{work_id}-#{e} s3://${S3_BUCKET}/#{work_id}/" }
x! "s3cmd setacl -r --acl-public s3://${S3_BUCKET}/#{work_id}"
```

### Test and evaluate LDA models <sub>using  [exam-comm-text](https://raw.github.com/henry4j/-/master/paste/exam-comm-text) & [p-topics](https://raw.github.com/henry4j/-/master/paste/p-topics) (jruby script)</sub>

* Dependencies: `jruby`, `mvn`, `S3_BUCKET` shell variable, the Internet to S3; works locally once getting models from S3.
  * this test & eval. scripts do not depend on any external Java web services or databases; no need for VPN/corporate net.
* DO RUN `p-topics` before `exam-comm-text` to get Java dependencies from maven repositories.
* `p-topics -s -m l-lda-6 'how do i cancel refund since the item is shipped?'`
  * output: `{"6":45.4,"14":32.8,"15":18.0,"4":2.2}` -- it belongs to topic 6 with 45.4% probability.
* `exam-comm-text --model-id l-lda-6 --threshold 0.39 | tail -n 2 # 53.4% (2332 / 4368)`
* `exam-comm-text --model-id l-lda-6 --excludes Others --threshold 0.01 | tail -n 2 # 65.0% (2271 / 3492)`
* `exam-comm-text --model-id l-lda-7 --threshold 0.42 | tail -n 2 # 56.0% (2445 / 4368)`
* `exam-comm-text --model-id l-lda-7 --excludes Others --threshold 0.01 | tail -n 2 # 66.6% (2326 / 3492)`
* `exam-comm-text -m unigram-rrc-pro-22k ###### 52.3% accuracy`; see [unigram-rrc-pro-22k.log](http://raw.github.com/henry4j/-/master/modeling/unigram-rrc-pro-22k.log)
  * feeds p-topics with test docs (a line per doc); IPC happens through STDIN & STDOUT.
* `exam-comm-text -m bigram-rrc-pro-22k-107ml # 52.3% accuracy`; see [bigram-rrc-pro-22k-107ml.log](http://raw.github.com/henry4j/-/master/modeling/bigram-rrc-pro-22k-107ml.log)
* `exam-comm-text -m true-l-lda # 78.8%`; see [true-l-lda.log](http://raw.github.com/henry4j/-/master/modeling/bigram-rrc-pro-22k-107ml.log)

![Bob the builder?](http://upload.wikimedia.org/wikipedia/en/thumb/c/c5/Bob_the_builder.jpg/220px-Bob_the_builder.jpg "Under Construction")

### Tuning in Progress

*

### Eclipse, M2, Lombok, DLTK - Ruby, ...

###### Install [Eclipse Kepler (4.3)](http://www.eclipse.org/downloads/)

```bash
curl -o "$HOME/Downloads/eclipse-4.3.1.tar.gz" -kL http://ftp.osuosl.org/pub/eclipse//technology/epp/downloads/release/kepler/SR1/eclipse-standard-kepler-SR1-macosx-cocoa-x86_64.tar.gz
tar xvf "$HOME/Downloads/eclipse-4.3.1.tar.gz" -C /Applications/
open /Applications/eclipse/Eclipse.app # and then keep this in dock
```

##### Install Eclipse plugins

1. Eclipse | Help | Install New Software... | Add...
2. Enter `m2e` and `http://download.eclipse.org/technology/m2e/releases/` into Add Repository | OK
3. Select All | Next | Next | Accept EULA | Finish | Restart Now  

#
* **[More plugins](http://marketplace.eclipse.org/metrics/installs/last30days)**: MouseFeed at `http://update.mousefeed.com/`, JAutoDoc at `http://jautodoc.sourceforge.net/update/`

##### Install/Update Lombok

```bash
mvn dependency:get -DremoteRepositories=http://download.java.net/maven2 -Dartifact=org.projectlombok:lombok:0.12.0
sudo java -jar "$HOME/.m2/repository/org/projectlombok/lombok/0.12.0/lombok-0.12.0.jar"
```

##### Install Dynamic Lanauges Toolkit - Ruby

1. Eclipse | Help | Install New Software...
2. Work with: `Kepler - http://download.eclipse.org/releases/kepler`
3. Expand `Programming Languages`, and then select `Dynamic Lanauges Toolkit - Ruby`
4. Next | Next | Accept EULA | Finish, and then **[Restart Now]**
5. Eclipse | Preferences... | Ruby
   * Debug | Engines, and then select `Ruby Built-in Debugger`  
     # [x] `/usr/local/bin/gem install test-unit debugger`
   * Interpreters | Add... | Interpreter executable: `/usr/local/bin/ruby` as `ruby 2`

##### Config M2, P4merge, SVN and Git with aliases

```bash
curl -o "$HOME/.m2/settings.xml" -ksL https://raw.github.com/henry4j/-/master/paste/.m2-settings.xml

bash < <(curl -ksL http://raw.github.com/henry4j/-/master/paste/install-p4merge)

git config --global user.name  henry4j # your name instead
git config --global user.email henry4js@gmail.com # your email address instead
curl -ksL http://raw.github.com/henry4j/-/master/paste/.gitconfig >> "$HOME/.gitconfig" # uses p4merge
curl -o "$HOME/.gitignore" -ksL http://raw.github.com/henry4j/-/master/paste/.gitignore

[ ! -d "$HOME/.subversion" ] && mkdir -p "$HOME/.subversion"
curl -ksL http://raw.github.com/henry4j/-/master/paste/.svn-config >> "$HOME/.subversion/config" # uses p4merge

curl -o "$HOME/.git-alias" -ksL http://raw.github.com/henry4j/-/master/paste/.git-aliases
echo 'source .git-aliases' >> "$HOME/.profile"
```

##### Clone Text project that contains text analyzer, and topic modeling.

```bash
git clone git@github.com:henry4j/text.git /workspace/gits/text
cd /workspace/gits/text/sources/text
mvn install
mvn eclipse:eclipse # [spawns](http://www.spawn.com/comics/series.aspx?series_id=1) an eclipse project w/ sources
```

* Eclipse | Import... | General | Existing Projects into Workspace
* Enter `/workspace/gits/text/sources/text` for root directory
* Refresh | Finish

### Intro to Bayesian Classifier

```java
public static class Classifier {
    Map<String, Category> categories = new HashMap<String, Category>();
    int totalNumDocuments = 0;

    public void train(String document) {
        String[] split = StringUtils.split(document);
        String categoryId = split[0]; // e.g. "SPAM", or "HAM"
        Category category = categories.get(categoryId);
        if (null == category) {
            categories.put(categoryId, category = new Category(categoryId));
        }
        category.train(Arrays.copyOfRange(split, 1, split.length));
        totalNumDocuments++;
    }

    public void classify(String document) {
        String[] words = StringUtils.split(document);
        for (Category c : categories.values()) {
            double p = 1.0;
            for (String word : words) {
                p *= c.wordProbability(word);
            }
            System.out.print(format("Probability of document '%s' for category '%s' is %f.", document, c, p * c.categoryProbability(totalNumDocuments)));
        }
    }
}

@RequiredArgsConstructor
public static class Category {
    final String id;
    int numDocuments;
    double categoryProbability;
    Map<String, MutableInt> features = new HashMap<String, MutableInt>();

    void train(String[] words) {
        numDocuments++;
        for (String w : words) {
            MutableInt c = features.get(w);
            if (null == c) {
                features.put(w, new MutableInt(1));
            } else {
                c.increment();
            }
        }
    }

    double wordProbability(String word) {
        MutableInt i = features.get(word);
        return null == i ? 0.1 : i.doubleValue() / numDocuments;
    }

    double categoryProbability(int totalNumDocuments) {
        return numDocuments / (double)totalNumDocuments;
    }
}
```

### Intro to Clustering & Analyzer -- [Mahout algorithms](https://cwiki.apache.org/confluence/display/MAHOUT/Algorithms)

* text clustering: having a text processing tool that can automatically group similar items and present the results with summarizing labels is a good way to wade through large amount of text or search results without having to read all, or even most, of the content.
* cluster is an unsupervised task w/ no human interaction, such as annotating training text, required that can automatically put related content into buckets; in some cases, it also can assign **labels** to buckets and even give **summaries** of what's in each bucket.
* carrot vs. mahout
  * merits and demerits.
* how clustering can be appied at the word- or topic-level to identify keywords, or topics in documents (called topic modeling) using LDA (latent dirichlet allocation).
* performance: how fast? and how good?
* e.g. Google News, not just good algos, but **scalability**
  * factors: title, text, and publication time; they use various clustering algorithms to identify closely related stories.
  * there is more than running clustering algorithms - grouping news content on a near-real-time basis - designed to be scale.
  * quickly cluster large # of documents, determine representative documents or labels for display, and deal with new, incoming documents.

##### Types of clustering

* clustering is also useful for many other things besides text, like grouping users or data from a series of sensors, but those are outside the scope of this book.
* clustering documents is usually an offline batch processing job, so it's often worthwhile to spend extra time to get better results.
  * descriptions for clusters are often generated by looking at the most important terms by some weighting mechanism (such as TF-IDF) in documents closest to the centroid.
  * using n-grams (or identifying phrases) may also be worth experimenting with when testing description(?) approaches.
* clustering words into topics (also called topic modeling) is an effective way to quickly find topics that are covered in a large set of documents.
  * we assume that documents often cover several different topics and that words related to a given topic often found near each other -- quickly find which words appearch near each other and what documents are attached those words; in a sense, topic modeling also does document clustering.
  * topics themselves lack names, so naming it is the job of the person generating the topics. generating topics for a collection is one more way to aid users in browsing the collection and discovering interests, while we don't even know what documents contain which topic.

<table>
  <thead>
    <tr>
      <td>Topic 0</td>
      <td>Topic 1</td>
    </th>
  </thread>
  <tbody>
    <tr>
      <td>win saturday time game know nation u more after two take over back has from texa first day man offici 2 high one sinc some sunday</td>
      <td>yesterday game work new last over more most year than two from state after been would us polic peopl team run were open five american</td>
    </tr>
  </tbody>
</table>

* choosing clustering algorithms
  * hieararchical approach that runs non-linear time vs. flat approach that runs in linear time.
  * in hard- vs. soft-membership, required to rebuild clusters with new documents or not.
  * adaptive with user feedback, required to specify the number of clusters, performance and quality.
* similarity is implemented as a measure of distance between two documents that are represented as sparse vectors in p-norm with TF-IDF weights.
  * Euclidean or Cosine distance measures are appropriate for 2-norm, while Manhattan distance measure is for 1-norm.
* labeling clustering results involves utilizing concept/topic modeling by Latent Dirichlet Allocation, or
  * picking representative documents from a cluster (randomly, near from centroid, or by membership likelihood).
  * picking good labels by important terms or phrases in a cluster, a weighted list of terms by TF-IDF, a list of phrases by n-grams.

##### Analyzer Basics

Solr applies an analysis process to fields being indexed to stem words, remove stopwords, and otherwise alter the tokens to be indexed. The Lucene Analyzer class controls this process, and consists of an optional **CharFilter**, a required **Tokenizer**, and zero or more **TokenFilter**s.  
* A `CharFilter` can be used to remove content while maintaining correct offset information (such as stripping HTML tags) for things like highlighting. In most cases, you won’t need a `CharFilter`.
* A `Tokenizer` produces Tokens, which in most cases correspond to words to be indexed.
* A `TokenFilter` then takes Tokens from the `Tokenizer` and optionally modifies or removes the Tokens before giving them back to Lucene for indexing.
* e.g., Solr's `WhitespaceTokenizer` breaks words on whitespace, and its `StopFilter` removes common words from search results.

##### Analyzers

* MailArchivesClusteringAnalyzer (more aggresive than the default StandardAnalyzer) uses a broader set of stopwords, excludes nonalphanumeric tokens, and applies porter stemming.
* WhitespaceAnalyzer performs simpletokenization of the input data. The data will have stemming performed and stopwords removed using Lucene’s EnglishAnalyzer later as a part of the training and test process, so there’s no need to perform anything other than whitespace tokenization at this point. Other classifiers such as Mahout's Bayes classifier benefit from performing stemming and stopword removal as a part of the data preparation phase.

#
* [o.a.l.analysis.coreStopAnalyzer](http://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/core/StopAnalyzer.html)
  -- [svn](http://svn.apache.org/viewvc/lucene/dev/trunk/lucene/analysis/common/src/java/org/apache/lucene/analysis/core/StopAnalyzer.java?view=markup)
  * filters LetterTokenizer with LowerCaseFilter and StopFilter.
* [o.a.l.analysis.core.WhitespaceAnalyzer extends Analyzer](http://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/core/WhitespaceAnalyzer.html)
  -- [svn](http://svn.apache.org/viewvc/lucene/dev/trunk/lucene/analysis/common/src/java/org/apache/lucene/analysis/core/WhitespaceAnalyzer.java?view=markup)
  * uses [o.a.l.analysis.core.WhitespaceTokenizer](http://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/core/WhitespaceTokenizer.html)
* [o.a.l.analysis.en.EnglishAnalyzer extends StopwordAnalyzerBase](http://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/en/EnglishAnalyzer.html)
  -- [svn](http://svn.apache.org/viewvc/lucene/dev/trunk/lucene/analysis/common/src/java/org/apache/lucene/analysis/en/EnglishAnalyzer.java?view=markup)
  * filters StandardTokenizer with StandardFilter, EnglishPossessiveFilter, PorterStemFilter w/ a stem exclusion list, LowerCaseFilter and StopFilter, using a list of English stop words.
  * uses [o.a.l.analysis.en.EnglishPossessiveFilter for trailing `'s`](http://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/en/EnglishPossessiveFilter.html)
* [o.a.l.analysis.standard.StandardAnalyzer extends StopwordAnalyzerBase](http://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/standard/StandardAnalyzer.html)
  -- [svn](http://svn.apache.org/viewvc/lucene/dev/trunk/lucene/analysis/common/src/java/org/apache/lucene/analysis/standard/StandardAnalyzer.java?view=markup)
  * filters StandardTokenizer with StandardFilter, LowerCaseFilter and StopFilter, using a list of English stop words.
  * uses [o.a.l.analysis.standard.StandardTokenizer](http://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/standard/StandardTokenizer.html)
     * A grammar-based tokenizer constructed with JFlex.
     * As of Lucene version 3.1, this class implements the Word Break rules from the Unicode Text Segmentation algorithm, as specified in Unicode Standard Annex #29.
     * Many applications have specific tokenizer needs. If this tokenizer does not suit your application, please consider copying this source code directory to your project and maintaining your own grammar-based tokenizer.
  * standard tokenizer w/ 255 max token length, filters: lower-case, and stop.
  * 33 stop-words: ["a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "if", "in", "into", "is", "it", "no", "not", "of", "on", "or", "such", "that", "the", "their", "then", "there", "these", "they", "this", "to", "was", "will", "with"]
* [o.a.m.text.MailArchivesClusteringAnalyzer extends StopwordAnalyzerBase](http://www.java2s.com/Open-Source/Java-Open-Source-Library/Data-Mnining/mahout/org/apache/mahout/text/MailArchivesClusteringAnalyzer.java.java-doc.htm)
  -- [svn](http://svn.apache.org/viewvc/mahout/trunk/integration/src/main/java/org/apache/mahout/text/MailArchivesClusteringAnalyzer.java?view=markup)
  * custom Lucene Analyzer designed for aggressive feature reduction for clustering the ASF Mail Archives using an extended set of stop words, excluding non-alpha-numeric tokens, and porter stemming.
  * standard tokenizer, filters: lower-case, ascii-folding, alpha-numeric (2 - 40 chars long), stop, and porter-stem.
  * 471 stop-words w/o "a".
* [o.a.l.analysis.standard.UAX29URLEmailAnalyzer extends StopwordAnalyzerBase](http://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/standard/UAX29URLEmailAnalyzer.html)
  -- [svn](http://svn.apache.org/viewvc/lucene/dev/trunk/lucene/analysis/common/src/java/org/apache/lucene/analysis/standard/UAX29URLEmailAnalyzer.java?view=markup)
  * filters UAX29URLEmailTokenizer with StandardFilter, LowerCaseFilter and StopFilter, using a list of English stop words.
  * uses [o.a.l.analysis.standard.UAX29URLEmailTokenizer extends Tokenizer](https://lucene.apache.org/core/4_4_0/analyzers-common/org/apache/lucene/analysis/standard/UAX29URLEmailTokenizer.html)
* [o.a.m.text.wikipedia.WikipediaAnalyzer extends StopwordAnalyzerBase](http://svn.apache.org/viewvc/mahout/trunk/integration/src/main/java/org/apache/mahout/text/wikipedia/WikipediaAnalyzer.java?view=markup)
  * [o.a.l.analysis.wikipedia.WikipediaTokenizer](http://lucene.apache.org/core/3_6_2/api/all/org/apache/lucene/analysis/wikipedia/WikipediaTokenizer.html)
     * Extension of StandardTokenizer that is aware of Wikipedia syntax. It is based off of the Wikipedia tutorial available at http://en.wikipedia.org/wiki/Wikipedia:Tutorial, but it may not be complete.

##### [Labeled LDA](http://markmail.org/message/cm2a6rnxblj5azuh) -- not published, but a close variant in http://github.com/twitter/mahout

* In 0.7, we can specify for training a seed model to start with (i.e. a matrix of latent topic to term counts);  
  we are welcome to build up a matrix of "informed priors" on term distributions on each topic, or a random one if unspecified. 
* This twitter fork http://github.com/twitter/mahout allows you to specify priors on the document/topic distribution:  
  you take your set of input documents, and if each one has some known set of labels associated with it;  
  you take as a prior for p(topic) for this document to be not random (or uniform across all topics) but uniform across the known labels.
  * L-LDA futher constraints that while training it forces p(topic | doc(i)) = 0 for all topics outside the labeled set for doc(i),  
    whereas this fork allows drifting freely after the initial prior is applied, which leads to an intermediate algorithm between regular LDA and L-LDA.
* TODO to get "true" L-LDA: 
  * modify [CVB0PriorMapper.java](http://github.com/twitter/mahout/blob/master/core/src/main/java/org/apache/mahout/clustering/lda/cvb/CVB0PriorMapper.java).
  * before the train() is called (line 108), you'd want to keep a copy of the docTopicPrior vector, 
  * keeping note of which topics had zero probability, and then 
  * before the final line in the map() method, you'd want to zero-out the entries
  * in the updated docTopicPrior vector that should be zero and renormalize it before emitting.

```bash
opendiff /workspace/mahout-tag-0.8/core/src/main/java/org/apache/mahout/clustering/lda/cvb \
  /workspace/mahout-twitter/core/src/main/java/org/apache/mahout/clustering/lda/cvb  
```

##### R

* http://davidsimpson.me/2013/02/26/installing-r-on-os-x/
* R http://cran.rstudio.com/bin/macosx/, and R Studio http://www.rstudio.com/ide/

```bash
curl -o /tmp/income_train_data_10.csv -ksL http://dl.dropboxusercontent.com/u/47820156/r/income_train_data_10.csv
```

```bash
R
install.packages('ggplot2') # qplot() for quick plots; ggplot() for fine, granular control for everything.
install.packages('ROCR')
```

```bash
R
income <- read.csv('/tmp/income_train_data_10.csv', header=T, strip.white=TRUE)
income$class <- as.factor(income$class)
? read.csv
summary(income)
str(income)
library('ggplot2')
qplot(data=income, x=age,main="Histogram of Age",binwidth=3)
```

### Experiments

```bash
cat 1.csv | ruby -e '
  srand 1234; a = readlines; (1...n = a.size).each { |i| j = i + rand(n - i); a[i], a[j] = a[j], a[i] }; puts a
' | tee 1-rand.csv

cat 2.txt | ruby -e 'a = readlines; n = s.size; sum = ARGV.'
```

### References

* [Mahout on Amazon EMR: Elastic MapReduce](https://cwiki.apache.org/confluence/display/MAHOUT/Mahout+on+Elastic+MapReduce)
* [Yahoo Dev. Network - Hadoop Tutorial](http://developer.yahoo.com/hadoop/tutorial/)
