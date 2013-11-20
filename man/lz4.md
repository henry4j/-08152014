echo | mvn archetype:generate \  
  -DarchetypeGroupId=org.apache.maven.archetypes \  
  -DarchetypeArtifactId=maven-archetype-quickstart \  
  -DarchetypeVersion=1.1 \  
  -DgroupId=com.henry4j \  
  -DartifactId=henry4j-common-compress \  
  -Dversion=1.0-SNAPSHOT \  
  -DpackageName=com.henry4j \  
  -DinteractiveMode=false

***

* [ant/cpptasks](http://search.maven.org/#artifactdetails%7Cant-contrib%7Ccpptasks%7C1.0b5%7Cjar)
  * wget -O cpptasks-1.0b5.jar http://search.maven.org/remotecontent?filepath=ant-contrib/cpptasks/1.0b5/cpptasks-1.0b5.jar
* [ant/ivy](http://ant.apache.org/ivy/download.cgi)
  * wget -O apache-ivy-2.3.0-rc2-bin.zip http://mirror.quintex.com/apache//ant/ivy/2.3.0-rc2/apache-ivy-2.3.0-rc2-bin.zip
* [lz4-java](https://github.com/jpountz/lz4-java)
  * `git clone` and `ant`

***

http://www.gamlor.info/wordpress/2010/07/lookup-logic-for-native-libraries-in-java/
http://libgdx.googlecode.com/svn-history/r1393/trunk/gdx/src/com/badlogic/gdx/utils/GdxNativesLoader.java
