### Android & Java SDE Bootcamp

* Learn Unix command-line tools
* Setup Java & Android dev. tools
* Learn Git repository
* Jump-start Desktop Java project
* Jump-start Android Java project
* Reverse com.facebook.katana.apk

##### Learn Unix command-line tools

* [Unix file system & commands](http://computing.fnal.gov/unixatfermilab/html/filesys.html)
* [Unix command crash course](http://www.csoft.net/docs/course.html.en)
* [50 Most Frequently Used UNIX / Linux commands](http://www.thegeekstuff.com/2010/11/50-linux-commands/)
* [10 Awesome Examples for Viewing Huge Log Files in Unix](http://www.thegeekstuff.com/2009/08/10-awesome-examples-for-viewing-huge-log-files-in-unix/)
* [15 Practical Grep Commands](http://www.thegeekstuff.com/2009/03/15-practical-unix-grep-command-examples/)

##### Install Mac OS X applications

[iterm2](http://www.iterm2.com/), [menu meters](http://www.ragingmenace.com/software/menumeters/), fraise, [better touch tool](http://blog.boastr.net/), adium, iStat[,](http://dl.dropbox.com/u/47820156/iStat%20Menus%203.21.zip) textmate, [sublime](http://www.sublimetext.com/2), [StartUpSound.PrefPane](http://www5e.biglobe.ne.jp/~arcana/software.en.html), 
little snitch, [sequel.pro](http://www.sequelpro.com/download/), [oracle.sql.developer](http://dl.dropbox.com/u/47820156/sqldeveloper-3.0.04.34-macosx.tar.gz), [Day-O](http://shauninman.com/archive/2011/10/20/day_o_mac_menu_bar_clock), and [UTC bar](https://itunes.apple.com/us/app/utc-bar/id525372278)

##### Install Baselines - ~~[Homebrew w/o Xcode](http://blog.strug.de/2011/09/homebrew-without-xcode-save-15-gb/)~~

* ~~[GCC for Mac OS X](http://kennethreitz.com/xcode-gcc-and-homebrew.html) → [10.7+ v2 with X11 headers and bux fixes](https://github.com/downloads/kennethreitz/osx-gcc-installer/GCC-10.7-v2.pkg)~~
* [Command Line Tools for Xcode](https://developer.apple.com/downloads/index.action?=command%20line%20tools), or Preferences > Downloads > Components and install "Command Line Tools"
* `ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"`

##### Install workspaces and dot resource files

    sudo ln -s /Volumes/workspace /workspace
    sudo mkdir /workspace
    sudo chown $USER /workspace
    mkdir /workspace/gits/ # home for git repositories
    git clone git@github.com:henry4j/-.git /workspace/gits/henry4j

    cp /workspace/gits/henry4j/bin/.profile ~
    cp /workspace/gits/henry4j/bin/.git-aliases ~
    cp /workspace/gits/henry4j/bin/.vimrc ~
    cp -R /workspace/gits/henry4j/bin/.vim ~
    source ~/.profile # or open a new shell
    sudo mdutil -a -i off # (optional) disables spotlight.

##### Install Ruby through rbenv, [DLTK bug](https://bugs.eclipse.org/bugs/show_bug.cgi?id=345976)

    brew install rbenv
    brew install ruby-build
    rbenv uninstall 1.9.3-p448
    rbenv install 1.9.3-p448
    rbenv global 1.9.3-p448
    rbenv which ruby # $HOME/.rbenv/versions/1.8.7-p371/bin/ruby
    gem install ruby-debug

    wget -P /tmp/ http://rubyforge.org/frs/download.php/75414/linecache19-0.5.13.gem
    wget -P /tmp/ http://rubyforge.org/frs/download.php/75415/ruby-debug-base19-0.11.26.gem

    gem install /tmp/linecache19-0.5.13.gem
    gem install /tmp/ruby-debug-base19-0.11.26.gem -- --with-ruby-include=$HOME/.rbenv/source/ruby-1.9.3-p0

##### [Install JDK for Mac OS X](https://developer.apple.com/downloads/)

^-^;;;;; -- install JDK 6, [JDK 7](http://supportdownload.apple.com/download.info.apple.com/Apple_Support_Area/Apple_Software_Updates/Mac_OS_X/downloads/041-1941.20111108.6wtg7/JavaForMacOSX10.7.dmg), or both in order to debug-step into JDK sources.

* JDK-7u15 http://www.oracle.com/technetwork/java/javase/downloads/index.html
* JDK-8-lambda http://jdk8.java.net/lambda/

##### [Install Android SDK](http://developer.android.com/sdk/index.html)

    curl -o /tmp/android-sdk_r22.3-macosx.zip -ksL https://dl.google.com/android/android-sdk_r22.3-macosx.zip
    unzip -o /tmp/android-sdk_r22.3-macosx.zip -d /workspace/
    mv /workspace/android-sdk-macosx /workspace/android-sdk-r22.3
    ln -s /workspace/android-sdk-r22.3 /workspace/android-sdk
    android update sdk --no-ui --no-https # takes forever on sluggish network connections!!!

##### Set up env. variables for Android SDK and Maven

    # see if .profile already has these by `grep 'HOME\|PATH' ~/.profile`
    cat <<EOF >> ~/.profile
    export ANDROID_HOME=/workspace/android-sdk
    export M3_HOME=/usr/share/maven
    export PATH=$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$PATH
    EOF

##### [Install Fastboot](http://wiki.cyanogenmod.com/wiki/Fastboot)

    wget -P /tmp/ http://dl.dropbox.com/u/47820156/bin/fastboot-mac.zip
    unzip -o /tmp/fastboot-mac.zip -d /tmp/
    mv /tmp/fastboot-mac /usr/local/bin/fastboot

##### Install ApkSign

    wget -P /tmp/ http://dl.dropbox.com/u/47820156/bin/AndroidMod.zip
    unzip -o /tmp/AndroidMod.zip -d /tmp/
    mv /tmp/SignApk/signapk.jar /usr/local/bin/

    cat <<EOF > /usr/local/bin/signapk
    #!/bin/bash
    java -jar \`dirname \$0\`/signapk.jar \$@
    EOF
    
    chmod +x /usr/local/bin/signapk

##### Create Android virtual devices

```bash
ruby -e "
  target = %x* android list targets | awk '/android-15/ {print \$2}' *.chomp
  system %Q* echo | android create avd --name 15 --target #{target} * \
    unless %x* android list avd -c *.include? %Q*15\n*
"
```

##### ~~Maven build and deploy -- no longer in our uses~~

    git clone git://git.springsource.org/spring-mobile/samples.git /workspace/gits/spring-mobile-samples
    cd /workspace/gits/spring-mobile-samples/spring-android-showcase/client
    mvn clean install
    mvn android:emulator-start
    mvn android:deploy

##### Work with Ruby script or test project

1. File → Import... → General → Existing Projects into Workspace
2. Enter `/workspace/gits/henry4j/projs./rubist` for root directory
3. Refresh → Finish
4. Run | Debug As, and then select either Ruby Script, or Ruby Test

##### Install Eclipse plugins

1. Eclipse | Help | Install New Software... | Add...
2. Enter `ADT Plugin` and `https://dl-ssl.google.com/android/eclipse/` into Add Repository | OK
3. Select All | Next | Next | Accept EULA | Finish | Restart Now  
4. Repeat 1 - 3 for `m2e` and `http://download.eclipse.org/technology/m2e/releases/`
5. (Optional) Repeat it for Maven Integration for Android Development Tools.

#
* **See Also** Getting started with Maven Integration for Android Development Tools http://code.google.com/a/eclipselabs.org/p/m2eclipse-android-integration/wiki/GettingStarted
* **See Also** Blog posts about Maven Integration for Android Development Tools http://code.google.com/p/maven-android-plugin/wiki/LinksBlogsAndMore
* **[More Eclipse plugins](http://marketplace.eclipse.org/metrics/installs/last30days)**: MouseFeed at `http://update.mousefeed.com/`, JAutoDoc at `http://jautodoc.sourceforge.net/update/`

##### Set Android SDK Location & Custom Debug Keystore

1. Preferences... | Android | SDK Location: `/workspace/android-sdk`
2. Optionally to build platform applications & services
   * `wget -P /workspace/ http://dl.dropbox.com/u/47820156/platform.jks`
   * Android | Build | Custom debug keystore: `/workspace/platform.jks`

##### [Git Config](http://gitref.org/creating/)

    git config --global user.name  henry4j # your name instead
    git config --global user.email henry4js@gmail.com # your email address instead
    git config --global color.diff auto
    git config --global color.status auto
    git config --global color.branch auto

or `cp /workspace/gits/henry4j/bin/.gitconfig ~`

##### [Git Ignore](http://help.github.com/ignore-files/)

    git config --global core.excludesfile ~/.gitignore
    cat <<EOF > ~/.gitignore
    *.class
    *.md.html
    *.mkd.html
    .DS_Store
    .p4config
    .settings/
    bin/
    build/
    gen/
    target/
    project.properties
    EOF

##### [Git Reference](http://gitref.org/creating/), [Screencast](http://www.ralfebert.de/blog/tools/git_screencast/), and [Guide](http://marklodato.github.com/visual-git-guide/index-en.html)

^-^;;;;;

###### git init, add, commit, and push

    mkdir notes
    cd notes
    git init
    touch readme.mkd
    git add readme.mkd
    git commit -m 'git init'
    git remote add origin git@github.com:henry4j/notes.git
    git push -u origin master

##### Install EGit - no longer in our uses.

* Install Eclipse EGit under Collaboration at http://download.eclipse.org/releases/indigo/
* Usage 
  * Team → Share Project... → Git → Next → Use or create Repository in parent folder → Finish
  * Team → Pull
  * Team → Remote → Push...
  * Team → Show in History to inspect history
     * See also http://wiki.eclipse.org/EGit/User_Guide#Inspect_History

##### Work with Desktop Java project

1. `pushd /workspace/gits/henry4j/projs./places;`  
   `mvn eclipse:eclipse -DdownloadSources=true` # [spawns](http://www.spawn.com/comics/series.aspx?series_id=1) an eclipse project w/ sources
2. File → Import... → General → Existing Projects into Workspace
3. Enter `/workspace/gits/henry4j/projs./places` for root directory
4. Refresh → Finish
5. **Eclipse** → Preferences → Java → Build Path → Classpath Variables → New... → M2_REPO and `~/.m2/repository` in case of m2eclipse malfunctions.

##### Work with Android Java project

1. File → Import... → General → Existing Projects into Workspace
2. Enter `/workspace/gits/henry4j/projs./yamba` for root directory
3. Refresh → Finish

##### Install [Jad-Clipse](http://jadclipse.sourceforge.net/wiki/index.php/Main_Page) and [Jad](http://www.varaneckas.com/jad)

    bash < <(/usr/bin/curl -fsSL https://raw.github.com/henry4j/-/master/bin/install_jadclipse)

#
* Help | Install New Software ... | Add ... to open the Add Repository dialog
  * Type `http://jadclipse.sourceforge.net/update/` to location | OK
  * Check `Java Decompiler Eclipse Plug-in` | `Next` twice
  * Choose to accept the license and then `Finish`
* Preferences | General | Editors | File Associations
  * Make default editor `Decompiled Class File Viewer` for `*.class`.
* Preferences | Java | Decompilers | Jad
  * Debug → Check `Output original line numbers as comments`
  * Formatting → Check `Don't insert a newline before opening brace`
* /Applications/eclipse/Eclipse.app/Contents/MacOS/eclipse -clean & # if mal-functioning ...

#### DO USE Jad-Clipse, Fernflower, and JD-GUI together.

##### Inside Eclipse, we will use CMD + Shift + T, or R for decompiled projects.

* `CMD + Shift + T` to open a type that gets decompiled instantly by Jad-Clipse, e.g. FacebookService type.
* `CMD + Shift + R` to open a source file that is already decompiled by Fernflower, e.g. FacebookService.java.

##### Outside Eclipse, we will use [JD-GUI](http://java.decompiler.free.fr/?q=jdgui)

* File | Open ... | "*.jar" (any Java Archive that we want to decompile)
* File | Save All Sources to save the decompiled sources into a zip file

##### [JD-Eclipse](http://java.decompiler.free.fr/?q=jdeclipse) -- no longer in our uses.

* Help | Install New Software ... | Add ... to open the Add Repository dialog
* Type `http://java.decompiler.free.fr/jd-eclipse/update` to location | OK
* Check `Java Decompiler Eclipse Plug-in` | `Next` twice
* Choose to accept the license and then `Finish`

##### [JD-Eclipse w/ realignment](http://java.decompiler.free.fr/?q=node/464) at [sourceforge](http://sourceforge.net/projects/realignmentjd/files/) -- no longer in our use.

* `wget -P /Applications/eclipse/plugins http://iweb.dl.sourceforge.net/project/realignmentjd/realignment.jd.ide.eclipse_1.0.2.jar`
* Preferences → General → Editors → File Associations and select "*.class" file type and to choose "Realignment for JD Class File Editor" for Associated editors.

##### Install apk2eclipse

    bash < <(/usr/bin/curl -fsSL https://raw.github.com/henry4j/-/master/bin/install_apk2eclipse)

##### adb-*, and *-pkgset commands

    adb-pull-pkg
    list-pkgset ./system/framework/framework-res.apk
    sign-pkgset ./system/framework/framework-res.apk platform.jks
    push-pkgset ./system/framework/framework-res.apk

##### Decompile facebook apk

    adb shell sqlite3 /data/data/com.android.vending/databases/market_assets.db .dump | grep facebook
    adb shell 'pm path com.facebook.katana' # tells /data/app/com.facebook.katana-1.apk for facebook 1.7.2
    adb pull /data/app/com.facebook.katana-1.apk
    mv com.facebook.katana-1.apk com.facebook.katana-1.7.2.apk
    apk2eclipse com.facebook.katana-1.7.2.apk

##### Decompile google service framework

    adb shell 'pm list packages -f com.google.android.gsf'
    adb shell 'pm path com.google.android.gsf'
    adb pull /system/app/GoogleServicesFramework.apk
    mv GoogleServicesFramework.apk com.google.android.gsf-2.3.apk
    apk2eclipse com.google.android.gsf-2.3.apk

##### Install Busybox to Android

    wget -P /tmp/ http://busybox.net/downloads/binaries/latest/busybox-armv4tl
    adb shell mount -o remount,rw /system
    adb push /tmp/busybox-armv4tl /system/bin/busybox
    adb shell chmod 755 /system/bin/busybox

##### Install Superuser unless installed

    wget -P /tmp/ http://downloads.androidsu.com/superuser/Superuser-3.0.6-efgh-signed.zip
    unzip -o /tmp/Superuser-3.0.6-efgh-signed.zip -d /tmp/
    adb push /tmp/system/app/Superuser.apk /system/app
    adb push /tmp/system/bin/su /system/bin
    adb shell chmod 664 /system/app/Superuser.apk
    adb shell chmod 4755 /system/bin/su

##### Memory Analysis

* dumpsys meminfo `pid` # shows shared and private dirty.
* procrank -u # shows proportional and unique set sizes
  * pss and uss contain dalvik and native bytes, so it is hard to tell which is which.
* Eclipse | DDMS | Devices | Dump HROF file, and then open with MAT http://www.eclipse.org/mat/.
* reference: http://stackoverflow.com/questions/2298208/how-to-discover-memory-usage-of-my-application-in-android

##### [StrictMode](http://developer.android.com/reference/android/os/StrictMode.html) on Android 2.3 or higher

StrictMode is most commonly used to catch accidental disk or network access on the application's main thread, where UI operations are received and animations take place as follows:

02-08 17:09:20.570: DEBUG/StrictMode(1538): StrictMode policy violation; ~duration=1430 ms: android.os.StrictMode$StrictModeDiskReadViolation: policy=23 violation=2
02-08 17:09:20.570: DEBUG/StrictMode(1538):     at android.os.StrictMode$AndroidBlockGuardPolicy.onReadFromDisk(StrictMode.java:745)
02-08 17:09:20.570: DEBUG/StrictMode(1538):     at dalvik.system.BlockGuard$WrappedFileSystem.open(BlockGuard.java:228)
02-08 17:09:20.570: DEBUG/StrictMode(1538):     at java.io.FileInputStream.<init>(FileInputStream.java:80)
02-08 17:09:20.570: DEBUG/StrictMode(1538):     at com.mobipocket.jsr75.filesystem.AndroidFile.openInputStream(AndroidFile.java:123)

***

##### Common Java & ORM Recipes

    mkdir -p /workspace/gits/commons; cd /workspace/gits/commons
    git clone https://code.google.com/p/guava-libraries/ google-guava    
    svn checkout http://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.1.3 httpcomponents-client
    svn checkout http://svn.apache.org/repos/asf/httpcomponents/httpcore/trunk httpcomponents-core
    svn checkout http://svn.apache.org/repos/asf/httpcomponents/httpasyncclient/tags/4.0-beta1 httpcomponents-asyncclient
    svn checkout http://svn.apache.org/repos/asf/commons/proper/beanutils/trunk/ commons-beanutils
    svn checkout http://svn.apache.org/repos/asf/commons/proper/cli/trunk/ commons-cli
    svn checkout http://svn.apache.org/repos/asf/commons/proper/collections/trunk commons-collections
    svn checkout http://svn.apache.org/repos/asf/commons/proper/functor/trunk/ commons-functor
    svn checkout http://svn.apache.org/repos/asf/commons/proper/io/trunk commons-io
    svn checkout http://svn.apache.org/repos/asf/commons/proper/lang/trunk commons-lang3
    svn checkout http://svn.apache.org/repos/asf/commons/proper/primitives/trunk commons-primitives
    wget -P /tmp/ http://apache.mirrors.redwire.net//httpcomponents/httpclient/source/httpcomponents-client-4.1.2-src.tar.gz
    tar xvf /tmp/httpcomponents-client-4.1.2-src.tar
    svn checkout http://svn.apache.org/repos/asf/lucene/dev/trunk lucene-trunk
    export GREP_OPTIONS='--color=auto'
    find . | grep -v 'git\|svn\|Test' | xargs grep -n -A1 -B1 'public .*[eE]xists' # to see usages of exists

***

##### Wicked Cool Scripts

    diff -rq samsung-SGH-I727-Email samsung-SGH-I717-KJ5-Email # to see the diffs between Celox and Quincy

###### Nuke System and Email accounts

    cat <<EOF >> ~/bin/delete-edb 
    adb shell rm /data/system/accounts.db
    adb shell rm -r /data/data/com.htc.android.mail/databases/*
    adb shell rm -r /data/data/com.htc.android.mail/shared_prefs/*
    adb shell rm -r /data/data/com.android.email/databases/*
    adb shell rm -r /data/data/com.android.email/shared_prefs/*
    adb shell busybox killall com.htc.android.mail
    adb shell busybox killall com.android.email
    adb shell busybox killall com.google.android.gm
    adb shell busybox killall com.google.process.gapps
    # adb shell reboot
    EOF

###### List and Add to market-assets

    cat <<EOF > ~/bin/list-mdb
    adb shell sqlite3 /data/data/com.android.vending/databases/market_assets.db .dump
    EOF
    chmod +x ~/bin/list-mdb

    cat <<EOF > ~/bin/add-to-mdb
    adb shell sqlite3 /data/data/com.android.vending/databases/market_assets.db "INSERT OR REPLACE INTO asset_versions VALUES('v2:com.joulespersecond.seattlebusbot:1:21','content://downloads/my_downloads/6','DOWNLOAD_PENDING',1320161445169,1320161447243,1320161453103,0,447135,'com.joulespersecond.seattlebusbot','false','KP1Z29pn70_zfKRDHQCHWCpEFJU',0,21,'details?doc=com.joulespersecond.seattlebusbot',NULL,-1,'','',-1,4,-1,'','',-1,4)"
    adb shell sqlite3 /data/data/com.android.vending/databases/market_assets.db "INSERT OR REPLACE INTO assets VALUES('com.joulespersecond.seattlebusbot','DEFAULT','honesthenry@gmail.com')"
    adb shell busybox killall com.android.vending
    EOF
    chmod +x ~/bin/add-to-mdb

***

##### Install & Instantiate [Redis](http://redis.io/) → [tutorial](http://try.redis-db.com/)

    mkdir ~/opt/; pushd ~/opt/;
    wget http://redis.googlecode.com/files/redis-2.4.2.tar.gz;
    tar xvf redis-2.4.2.tar.gz;
    ln -s redis-2.4.2 redis; pushd redis;

    sed -e 's/port 6379/port 6389/g' redis.conf > redis-6389;
    src/redis-server redis-6389.conf; # in one terminal
    src/redis-server; # in another terminal

##### Install & Instantiate [Node](http://nodejs.org/) → [downloads](http://nodejs.org/#download)

    mkdir ~/opt/; pushd ~/opt/;
    wget http://nodejs.org/dist/node-v0.4.12.tar.gz
    tar xvf node-v0.4.12.tar.gz
    ln -s node-v0.4.12 node; pushd node

    # to build according to README.md
    ./configure
    make
    sudo make install

    # to test an example node
    cat <<EOF >> /tmp/example.js
    var http = require('http');
    http.createServer(function (req, res) {
      res.writeHead(200, {'Content-Type': 'text/plain'});
      res.end('Hello World\n');
    }).listen(1337, "127.0.0.1");
    console.log('Server running at http://127.0.0.1:1337/');
    EOF

    node /tmp/example.js & # plays a server.
    curl http://127.0.0.1:1337/ # plays a client.

***

##### Resources

* [[FAQ] ADB and FASTBOOT for Windows (Updated 1st June 2011)](http://androidforums.com/desire-all-things-root/337356-faq-adb-fastboot-windows-updated-1st-june-2011-a.html)
* [How to root Nexus S, or Nexus S 4G](http://nexusshacks.com/nexus-s-hacks/how-to-root-nexus-s-or-nexus-s-4g-new/)
* [Intro. to Maven](http://maven.apache.org/guides/introduction/introduction-to-the-pom.html)
* [Spring for Android](http://www.springsource.org/spring-android "Android extension for Spring Framework")
* [How to install](http://www.omappedia.org/wiki/Android_Installing_Busybox_Command_Line_Tools) [busybox](http://busybox.net/downloads/binaries/) 
* [Administrative Scripting by Ruby](http://faculty.cs.nku.edu/~waldenj/classes/2009/fall/cit383/)
* [Administraitve Scripting Labs](http://faculty.cs.nku.edu/~waldenj/classes/2009/fall/cit383/labs/)
* [What to expect from the Ruby expect library?](http://www.42klines.com/2010/08/14/what-to-expect-from-the-ruby-expect-library.html)
* [Java decompiler tests](http://www.program-transformation.org/Transform/JavaDecompilerTests)
* [Android Build System](http://elinux.org/Android_Build_System)
