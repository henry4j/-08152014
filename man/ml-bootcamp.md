<sup>["true" L-LDA](https://github.com/henry4j/-/blob/master/man/l-lda.mkd), [VW](http://en.wikipedia.org/wiki/Vowpal_Wabbit), [NB](http://chimpler.wordpress.com/2013/03/13/using-the-mahout-naive-bayes-classifier-to-automatically-classify-twitter-messages/), [SGD bootcamp](http://tiny/4b1766v4/wamazindeMach), [ML-workflow](ml-workflow.mkd), [text-mining/twitter](http://practicalquant.blogspot.com/2010/04/text-mining-and-twitter.html)</sup>

### ML SDE requirement -- <sub>[1](http://www.amazon.com/gp/jobs/232194/), [2](http://www.amazon.com/gp/jobs/180727/), [3](https://jobs.groupon.com/careers/engineering/software-development-engineer-senior-goods-seattle-wa-united-states/), [4](https://twitter.com/jobs/positions?jvi=oSAqXfwz,Job), [5](https://twitter.com/jobs/positions?jvi=oTAqXfwA,Job), [6](http://www.linkedin.com/jobs?viewJob=&jobId=6907673), [7](http://www.linkedin.com/jobs?viewJob=&jobId=6865716)</sub>

* ML basics: Supervised- & Unsupervised learning, Collaborative filtering, TF/TFIDF, n-gram, tokenizer, vectorizer, ...
* Cloud Computing/SOA patterns/Java Concurrency: 
  * Hadoop/<sub>Map Reduce/HDFS/Pig/Hive</sub>, Mahout/R, Lucene/Solr/<sub>search</sub>, ZooKeeper/<sub>distributed coordination</sub>, ...
  * Amazon EMR/SWF/SQS/DynamoDB/S3, ORM/noSQL, Barrier/AQS/PQ, Leader Election/<sub>Paxos</sub>, ...
* Scripting/open source tools: GIT/Subversion, Maven/POM/Eclipse, awk/sed, Shell/Ruby/Python, Scala/JRuby/Jython, ...
* How to formulate ML problems? How to parallelize ML algorithms? How to select or reduce features? For your reading:  
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
* `ruby -e "$(curl -fsSL http://raw.github.com/mxcl/homebrew/go)"` # at the terminal.

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
gem install test-unit debugger
rbenv install 1.9.3-p448 && rbenv global 1.9.3-p448

echo '[ -d $HOME/.rbenv ] && eval "$(rbenv init -)"' >> "$HOME/.profile"
curl -o "$HOME/.irbrc" -ksL http://raw.github.com/henry4j/-/master/paste/.irbrc
```

##### Install Mac VIM (optional)

```bash
curl -o /tmp/MacVim-snapshot-70-Lion.tbz -kL https://macvim.googlecode.com/files/MacVim-snapshot-70-Lion.tbz
tar xvf /tmp/MacVim-snapshot-70-Lion.tbz -C /tmp/
mv /tmp/MacVim-snapshot-70/MacVim.app /Applications/
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

* Setup `libexec/conf/hadoop-env.sh` and [pseudo-distributed operation](http://hadoop.apache.org/docs/stable/single_node_setup.html#PseudoDistributed)
* Bring up the [name node](http://localhost:50070/) and [job tracker](http://localhost:50030/) (n/a if running locally)
* Try-upload & download local files to DFS by [`hadoop dfs -put`](http://hadoop.apache.org/docs/stable/file_system_shell.html#put), [`hadoop dfs -get`](http://hadoop.apache.org/docs/stable/file_system_shell.html#get), and then [`hadoop dfs -ls`](http://hadoop.apache.org/docs/stable/file_system_shell.html#ls)

```bash
brew install hadoop
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
ln -sf "/workspace/$e" /workspace/mahout
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
for e in prep-comm-text stop-comm-text resplit pp-w,z pp-z,d tame-hadoop tame-corpus tame-topics-l p-topics p-topics.rb exam-comm-text; do
  curl -o /usr/local/bin/$e -kL https://raw.github.com/henry4j/-/master/paste/$e;
  chmod +x /usr/local/bin/$e;
done
```

#### ~~Set up utilities for Mahout work~~ -- no longer in use.

```bash
for e in prep-comm-text stop-comm-text resplit pp-w,z pp-z,d tame-hadoop tame-corpus tame-topics-l p-topics exam-comm-text; do
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
#!/bin/bash # called `tame-hadoop`
curl -o ${HADOOP_BASE}/libexec/lib/text-1.0-SNAPSHOT.jar -ksL http://raw.github.com/henry4j/-/master/paste/text-1.0-SNAPSHOT.jar
ln -sf ${HADOOP_BASE}/libexec/lib/text-1.0-SNAPSHOT.jar ${MAHOUT_BASE}/lib/text-1.0-SNAPSHOT.jar
 
stop-all.sh
ps -ef | grep 'org.apache.hadoop.[^ ]\+$' | ruby -ane 'puts $F[1]' | xargs kill
start-all.sh
$HADOOP dfsadmin -safemode leave
```

#### [`tame-corpus 1`](http://raw.github.com/henry4j/-/master/paste/tame-corpus), or step-by-step at the terminal

```bash
#!/usr/bin/env jruby # called `tame-corpus`
require 'rake'
sources = %w(rrc_pro_22110.csv rrc_ind_31771.csv rrc_pro_2285_labeled_typos.csv)
corpora = %w(corpus-1.csv corpus-2.csv corpus-l.csv).map { |e| File.join(ENV['MAHOUT_WORK'], e) }
options = [%w(-i 3 -f 4,11), %w(-i 3 -f 4,11), %w(-i 4 -l 8 -f 1,3)]
c = Integer(ARGV[0]) - 1 # expects 1 or greater.

def x!(*cmd, &blk) block_given? ? (sh cmd.join(' ') do |*a| blk.call(a) end) : (sh cmd.join(' ')) end

x! %W(s3cmd get s3://${S3_BUCKET}-private/resources/#{sources[c]} #{corpora[c]}) unless File.exist?(corpora[c])
x! %W(prep-comm-text #{corpora[c]} -d ${MAHOUT_WORK}/comm-text-ext) + options[c]
x! '$HADOOP dfs -rmr ${MAHOUT_WORK}/comm-text-ext ${MAHOUT_WORK}/comm-text-seq' do end # rescue on errors.
x! '$HADOOP dfs -put ${MAHOUT_WORK}/comm-text-ext/corpus-priors ${MAHOUT_WORK}/comm-text-ext/corpus-priors'
x! '$HADOOP dfs -put ${MAHOUT_WORK}/comm-text-ext/corpus ${MAHOUT_WORK}/comm-text-ext/corpus'
x! '$MAHOUT seqdirectory -i ${MAHOUT_WORK}/comm-text-ext/corpus -o ${MAHOUT_WORK}/comm-text-seq -ow -chunk 5'
```

* this is a similar script that compiles a corpus of reuter mail archives that is well-known for lucene benchmark program.

```
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

#### [`tame-topics-l`](https://raw.github.com/henry4j/-/master/paste/tame-topics-l)

* [!] this script is fragile, and subject to change without any notice; `*-advanced` script is coming soon.
* args: work-id (default: bigram-k), max-ngram (default: 2), LLR, and analyzer (default: CommTextAnalyzer)
  * e.g. `tame-topics bigram-rrc-pro-22k-200ml 2 200 # uses 22109 rows and 864 columns`; see [tame-topics-bigram.log](https://raw.github.com/henry4j/-/master/modeling/tame-topics-bigram.log).
* [?] [`mahout seq2sparse --help`](http://raw.github.com/henry4j/-/master/man/seq2sparse.mkd), [`mahout cvb --help`](http://raw.github.com/henry4j/-/master/man/lda-cvb.mkd)

```bash
#!/bin/bash # called `tame-topics`
if [ -z $4 ]; then ANALYZER='com.henry4j.text.CommTextAnalyzer'; else ANALYZER=$4; fi
if [ -z $3 ]; then MIN_LLR='120'; else export MIN_LLR=$3; fi
if [ -z $2 ]; then MAX_NGRAM='1'; else export MAX_NGRAM=$2; fi
if [ -z $1 ]; then WORK_ID='true-l-lda'; else export WORK_ID=$1; fi

export HADOOP_CLASSPATH=${MAHOUT_BASE}/lib/text-1.0-SNAPSHOT.jar:${MAHOUT_BASE}/lib/lucene-analyzers-common-4.3.0.jar:${MAHOUT_BASE}/lib/lucene-core-4.3.0.jar

if ! $($HADOOP dfs -test -e "${MAHOUT_WORK}/${WORK_ID}/matrix"); then
  $MAHOUT seq2sparse \
    -i ${MAHOUT_WORK}/comm-text-seq/ -o ${MAHOUT_WORK}/${WORK_ID} -ow --namedVector \
    -s 80 -md 40 -x 65 \
    -ng $MAX_NGRAM -ml $MIN_LLR \
    -a $ANALYZER
  
    # excludes terms of 80- DF & 70+ DF%
    # -a com.henry4j.text.CommTextAnalyzer \
    # -a org.apache.mahout.text.MailArchivesClusteringAnalyzer \
    # -a org.apache.lucene.analysis.en.EnglishAnalyzer \
    # -a org.apache.lucene.analysis.standard.StandardAnalyzer \

  for e in matrix docIndex; do $HADOOP dfs -rm ${MAHOUT_WORK}/${WORK_ID}/$e; done
  $MAHOUT rowid -i ${MAHOUT_WORK}/${WORK_ID}/tfidf-vectors -o ${MAHOUT_WORK}/${WORK_ID}
  for e in df-count tokenized-documents tfidf-vectors; do resplit ${MAHOUT_WORK}/${WORK_ID}/$e; done
  $MAHOUT seqdumper -i ${MAHOUT_WORK}/${WORK_ID}/tokenized-documents-0 -o /tmp/${WORK_ID}-tokenized-docs.txt
  $HADOOP dfs -put /tmp/${WORK_ID}-tokenized-docs.txt ${MAHOUT_WORK}/${WORK_ID}
fi

# $HADOOP dfs -test -e "${MAHOUT_WORK}/comm-text-ext/corpus-priors" && CVB_OPTS="-k 14 -pidt -dtp ${MAHOUT_WORK}/comm-text-ext/corpus-priors -cd 6e-10" || CVB_OPTS='-k 20 -cd 6e-4'
if $($HADOOP dfs -test -e "${MAHOUT_WORK}/comm-text-ext/corpus-priors"); then
  CVB_OPTS="-k 14 -pidt -dtp ${MAHOUT_WORK}/comm-text-ext/corpus-priors -cd 6e-10"
  for e in core-0.8   core-0.8-job   examples-0.8   examples-0.8-job;
    do [ -e "$MAHOUT_BASE/mahout-$e.jar" ] && mv $MAHOUT_BASE/mahout-$e.jar $MAHOUT_BASE/mahout-$e.jar.bak;
  done
  for e in core-0.8.2 core-0.8.2-job examples-0.8.2 examples-0.8.2-job;
    do [ ! -e "$MAHOUT_BASE/mahout-$e.jar" ] && curl -o "$MAHOUT_BASE/mahout-$e.jar" -kL "http://dl.dropboxusercontent.com/u/47820156/mahout/l-lda/mahout-$e.jar"; 
  done
else
  CVB_OPTS='-k 20 -cd 6e-4'
fi

rm -rf ${MAHOUT_WORK}/${WORK_ID}-*
$HADOOP dfs -rmr ${MAHOUT_WORK}/${WORK_ID}/model
$HADOOP dfs -rmr ${MAHOUT_WORK}/${WORK_ID}/topics
$HADOOP dfs -rmr ${MAHOUT_WORK}/${WORK_ID}-modeling
$MAHOUT cvb \
  -dict ${MAHOUT_WORK}/${WORK_ID}/dictionary.file-0 \
  -i  ${MAHOUT_WORK}/${WORK_ID}/matrix \
  -o  ${MAHOUT_WORK}/${WORK_ID}/model -ow \
  -mt ${MAHOUT_WORK}/${WORK_ID}-modeling \
  -dt ${MAHOUT_WORK}/${WORK_ID}/topics \
  -x 35 -block 2 -tf 0.25 -seed 777 \
  $CVB_OPTS

for e in model topics; do resplit ${MAHOUT_WORK}/${WORK_ID}/$e; done
for e in matrix docIndex wordcount frequency.file-0 tf-vectors; do $HADOOP dfs -rmr ${MAHOUT_WORK}/${WORK_ID}/$e; done

$MAHOUT vectordump \
  -i ${MAHOUT_WORK}/${WORK_ID}/model-0 -o /tmp/${WORK_ID}-w,z-dump.txt \
  -p true -sort ${MAHOUT_WORK}/${WORK_ID}/model-0 -vs 25 \
  -d ${MAHOUT_WORK}/${WORK_ID}/dictionary.file-0 -dt sequencefile
$MAHOUT vectordump -i ${MAHOUT_WORK}/${WORK_ID}/topics-0 -o /tmp/${WORK_ID}-z,d-dump.txt
$HADOOP dfs -put /tmp/${WORK_ID}-?,?-dump.txt ${MAHOUT_WORK}/${WORK_ID}

pp-w,z /tmp/${WORK_ID}-w,z-dump.txt | tee /tmp/${WORK_ID}-w,z-topic-terms.txt
pp-z,d /tmp/${WORK_ID}-z,d-dump.txt -n 10 | tee /tmp/${WORK_ID}-z,d-doc-topics.txt
for e in w,z z,d; do $HADOOP dfs -put /tmp/${WORK_ID}-$e-*.txt ${MAHOUT_WORK}/${WORK_ID}; done

rm -rf ${MAHOUT_WORK}/${WORK_ID}
$HADOOP dfs -get ${MAHOUT_WORK}/${WORK_ID} ${MAHOUT_WORK}/${WORK_ID}
s3cmd put -r ${MAHOUT_WORK}/${WORK_ID} s3://${S3_BUCKET}
s3cmd setacl -r --acl-public s3://${S3_BUCKET}/${WORK_ID}
```

### Test and evaluate LDA models <sub>using  [exam-comm-text](https://raw.github.com/henry4j/-/master/paste/exam-comm-text) & [p-topics](https://raw.github.com/henry4j/-/master/paste/p-topics) (jruby script)</sub>

* Dependencies: `jruby`, `mvn`, `S3_BUCKET` shell variable, the Internet to S3; works locally once getting models from S3.
  * this test & eval. scripts do not depend on any external Java web services or databases; no need for VPN/corporate net.
* DO RUN `p-topics` before `exam-comm-text` to get Java dependencies from maven repositories.
* `p-topics -s -m unigram-rrc-pro-22k 'how do i cancel refund since the item is shipped?'`
  * output: `{"2":61.4,"4":32.4,"17":6.0}` -- it belongs to topic 2 with 61.4% probability.
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
curl -o "$HOME/Downloads/eclipse.tar.gz" -kL http://mirrors.ibiblio.org/eclipse/technology/epp/downloads/release/kepler/R/eclipse-jee-kepler-R-macosx-cocoa-x86_64.tar.gz
tar xvf "$HOME/Downloads/eclipse.tar.gz" -C /Applications/
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
