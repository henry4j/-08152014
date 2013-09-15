# color prompts, e.g. 32;40, or 30;46
if [ '3c075447ffae' = `hostname | cut -d. -f1` ]; then
  export PS1='\[\e]2;\u@\h:\@:\w\a\e]1;$(basename $(dirname $(pwd)))/\W\a\e[32;40m\]\t:$(basename $(dirname $(pwd)))/\W>\[\e[0m\] '
else
  export PS1='\[\e]2;\u@\h:\@:\w\a\e]1;$(basename $(dirname $(pwd)))/\W\a\e[30;46m\]\t:$(pwd)>\[\e[0m\] '
fi
export TERM='xterm-color'
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export DOMAINNAME=`hostname | rev | cut -d. -f1,2 | rev`
# export CHROMIUM_USER_FLAGS=disk-cache-dir=/tmp --disk-cache-size=50000000

# homes and paths
export ANDROID_HOME=/workspace/android-sdk
export GROOVY_HOME=/usr/lib/groovy
export JAVA_HOME=/Library/Java/Home
export M3_HOME=/usr/local/Cellar/maven/3.0.4 # previously, /usr/share/maven
export SWIFTMQ_HOME=/workspace/swiftmq-7.5.3
export PATH=$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH
export PATH=$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$PATH
export PATH=/workspace/mahout/bin:$PATH

# app settings
export GREP_OPTIONS='--color=auto --exclude=*\.svn --exclude=*\.svn-base --binary-files=without-match'
export CATALINA_OPTS='-Xmx1536m -XX:MaxPermSize=512m -Dlog.dir=/tmp/ -Xdebug -Xrunjdwp:transport=dt_socket,address=5005,suspend=n,server=y'
export USE_CCACHE=1
ulimit -S -n 1024 # sets the file descriptor limit to 1024.

# aliases for git
source .git-aliases

# misc. aliases
alias diff='colordiff'
alias gmail='openssl s_client -crlf -quiet -connect imap.gmail.com:993'
alias l='ls -alv'
alias ll='ls -alv'
alias ls='ls -av'
alias m2eclipse='mvn eclipse:eclipse -DdownloadSources=true'
alias sdf='svn diff --diff-cmd=svn-diff'
alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'
alias less='/Applications/MacVim.app/Contents/Resources/vim/runtime/macros/less.sh'
# alias less='less -Mi'
alias wget='wget --no-check-certificate'

# ssh-aliases
alias hylee="ssh -X hylee.desktop.$DOMAINNAME"
alias mbp13="ssh -X 3c075447ffae.ant.$DOMAINNAME"
alias alpha="ssh jaschen-1.desktop.$DOMAINNAME"
alias gamma="ssh acme-snapshot-gamma-na-1a-i-13e86d71.us-east-1.$DOMAINNAME"
alias prod="ssh acme-snapshot-na-1a-i-263f645f.us-east-1.$DOMAINNAME"

# cd aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'
alias ........='cd ../../../../../../..'
alias .........='cd ../../../../../../../..'
alias gits='cd /workspace/gits'
alias henry='cd /workspace/gits/henry4j'
alias workspace='cd /workspace'
alias ws='cd /workspace'
alias trunk='cd /workspace/mahout-trunk/core/src/main/java/org/apache/mahout/clustering/lda/cvb'
alias 0.8='cd /workspace/gits/mahout-0.8/core/src/main/java/org/apache/mahout/clustering/lda/cvb'
alias tweet='cd /workspace/gits/mahout-twitter/core/src/main/java/org/apache/mahout/clustering/lda/cvb'

[ -d $HOME/.rbenv ] && eval "$(rbenv init -)"

[ ! -e $HOME/.s3bucket ] && curl -o $HOME/.s3bucket -ksL http://tiny/1bbxr2hio/impramazs3fis3getext
export S3_BUCKET=$(<$HOME/.s3bucket)
export HADOOP_BASE=$(ruby -e "puts File.dirname(File.dirname(File.realpath(%x(which hadoop).chomp)))")
export HADOOP=$HADOOP_BASE/bin/hadoop
export MAHOUT_BASE=/workspace/mahout
export MAHOUT=$MAHOUT_BASE/bin/mahout
export PATH=$MAHOUT_BASE/bin:$PATH

export HADOOP_WORK=/workspace/hadoop-work
export MAHOUT_WORK=/workspace/mahout-work
[ ! -d $MAHOUT_WORK ] && mkdir -p $MAHOUT_WORK
export PATH=$MAHOUT_WORK:$PATH

