# JRuby and Java Code Examples

Below are some code examples showing how to call JRuby from Java and how to call Java from JRuby.<br/><br/>

## JRuby calling Java

See also: [Calling Java from JRuby](https://github.com/jruby/jruby/wiki/CallingJavaFromJRuby)

### JRuby: call_java.rb

```ruby
require "java"

include_class "java.util.TreeSet"
include_class "com.example.CallMe"
include_class "com.example.ISpeaker"

puts "Hello from ruby"
set = TreeSet.new
set.add "foo"
set.add "Bar"
set.add "baz"
set.each { |v| puts "value: #{v}" }

cm = CallMe.new
cm.hello
$globalCM.hello

class CallJava
  include ISpeaker
  def initialize
    super
    @count = 0
  end

  def say(msg)
    puts "Ruby saying #{msg}"
  end
  
  def addOne(from)
#   m.synchronize {
      @count += 1
      puts "Now got #@count from #{from}"
#   }
  end
end
```
### Java: ISpeaker.java

```java
package com.example;

public interface ISpeaker {
    public void say(String msg);
    public void addOne(String from);
}
```

### Java: CallMe.java

```java
package com.example;

public class CallMe {

    String mName;

    public CallMe() {
        this("Default");
    }
    
    public CallMe(String name) {
        mName = name;
    }
    
    public void hello() {
        System.out.println("Hello from "+mName);
    }
    
    public static void main(String []args) {
        System.out.println("Called main");
    }
}
```

## Java calling JRuby
### Java: CallRuby.java

```java
package com.example;

import org.apache.bsf.BSFManager;
import org.apache.bsf.util.IOUtils;
import org.jruby.Ruby;
import org.jruby.javasupport.Java;
import org.jruby.javasupport.JavaEmbedUtils;
import org.jruby.javasupport.JavaUtil;
import org.jruby.runtime.Block;
import org.jruby.runtime.GlobalVariable;
import org.jruby.runtime.builtin.IRubyObject;

import java.io.FileReader;
import java.io.IOException;

/**
 * Example of how to:
 * 1. Use java objects in ruby
 * 2. Subclass/implement java objects in ruby
 * 3. Get ruby objects for use in java world
 * 4. Proxy ruby objects for normal use as java objects (interfaces/class)
 */
public class CallRuby {

    public static void main(String[] args) throws Exception {

        String dir = "/dclark/workspace/jrubytest/ruby/";

        double[] deltas = new double[3];
        for (int i = 0; i < 3; i++) {
            boolean useBSF = (i == 0);
            long start = System.currentTimeMillis();

            if (useBSF) {
                //--- Initialise ruby
                BSFManager.registerScriptingEngine("ruby", "org.jruby.javasupport.bsf.JRubyEngine", new String[]{"rb"});
                BSFManager manager = new BSFManager();

                //--- Define a global variable
                CallMe javaCallMe = new CallMe("globalCallMeInJava");
                manager.declareBean("globalCM", javaCallMe, javaCallMe.getClass());

                //--- Load a ruby file
                manager.exec("ruby", "call_java.rb", -1, -1, getFileContents(dir + "call_java.rb"));

                //--- Make a new ruby object
                String expr = "CallJava.new";
                ISpeaker ruby = (ISpeaker) manager.eval("ruby", "call_java.rb", -1, -1, expr);

                testMultiThreadsCallingRubyObject(ruby);

            } else {

                //--- Initialise ruby
                final Ruby runtime = Ruby.getDefaultInstance();

                // Need the blank object so can get a nice runtime for the Java.staticMethods calls
                runtime.eval(runtime.parse("require \"java\"\nclass BlankForJva\nend\n", "BlankForJva.rb", runtime.getCurrentContext().getCurrentScope(), 0));
                final IRubyObject blankRuby = runtime.evalScript("BlankForJva.new");

                //--- Define a global variable
                CallMe javaCallMe = new CallMe("globalCallMeInJava");
                IRubyObject globValue = JavaUtil.convertJavaToRuby(runtime, javaCallMe);

                // Wrap so that all methods are visible to ruby
                globValue = Java.java_to_ruby(blankRuby, globValue, Block.NULL_BLOCK);

                GlobalVariable globVar = new GlobalVariable(runtime, "$globalCM", globValue);
                runtime.defineVariable(globVar);

                //--- Load a ruby file
                runtime.eval(runtime.parse(getFileContents(dir + "call_java.rb"), "call_java.rb", runtime.getCurrentContext().getCurrentScope(), 0));

                //--- Make a new ruby object
                String expr = "CallJava.new";
                final IRubyObject rawRuby = runtime.evalScript(expr);
                ISpeaker ruby;
                if (i == 1) {
                    // Standard wrapper using Java Proxies
                    ruby = (ISpeaker) JavaEmbedUtils.rubyToJava(runtime, rawRuby, ISpeaker.class);
                } else {
                    // Or manually wrap ruby object so can be used as the interface (can optionally add synchronization as required on methods)
                    ruby = new ISpeaker() {
                        public void addOne(String from) {
                            //                            synchronized (rawRuby) {
                            rawRuby.callMethod(runtime.getCurrentContext(), "addOne", JavaUtil.convertJavaToRuby(runtime, from));
                            //                            }
                        }

                        public void say(String msg) {
                            rawRuby.callMethod(runtime.getCurrentContext(), "say", JavaUtil.convertJavaToRuby(runtime, msg));
                        }
                    };
                }
                testMultiThreadsCallingRubyObject(ruby);
            }
            long end = System.currentTimeMillis();
            deltas[i] = (end - start) / 1000.0;
        }

        for (int i = 0; i < deltas.length; i++) {
            System.out.println("Took " + deltas[i] + " on pass " + i);
        }
    }

    private static String getFileContents(String filename) throws IOException {
        FileReader in = new FileReader(filename);
        return IOUtils.getStringFromReader(in);
    }

    public static void testMultiThreadsCallingRubyObject(final ISpeaker ruby) throws InterruptedException {
        Thread t1 = new Thread(new Runnable() {
            public void run() {
                for (int i = 0; i < 1000; i++) {
                    ruby.addOne("t1");
                }
            }
        });
        Thread t2 = new Thread(new Runnable() {
            public void run() {
                for (int i = 0; i < 1000; i++) {
                    ruby.addOne("t2");
                }
            }
        });
        t1.start();
        t2.start();
        t1.join();
        t2.join();
        ruby.addOne("end");
    }
}
```

**Note:** If you have only simple interface requirements, you can use the second method of proxying the ruby object to include method level synchronization.

**Note:** BSF calling does not preserve ruby stack traces (add an error to a script and run both ways using the code above).
