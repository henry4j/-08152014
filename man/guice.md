#### [Comparing Spring vs. Google Guice: By Example](http://www.theserverside.com/feature/Comparing-Spring-vs-Google-Guice-By-Example?vgnextfmt=print)

Guice autowires all concret clasess w/ little or no configuration, while Spring identifies beans.
* itemizing individual components, or scanning within Java package names.
* the framework still uses string ids to track them as beans, despite claiming to wire classes by type.

```java
@Component
public class Car {
    private @Autowired Engine betterEngine; // builds a better engine
}

@Component
public class BetterEngine extends Engine {
	private @Autowired Piston piston;
}
```

Guice has an annotation very similar to Spring's @Qualifier called @Named, which also uses a string identifier to match a potential implementation.  But to avoid the hazards of string identifiers, Guice has a better, type-safe way using a concept called binding annotations.

```java
@BindingAnnotation
@Target({ ElementType.FIELD, ElementType.PARAMETER, ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
public @interface Better {}

public class GuiceMain {
    private @Inject Car car;

    public static void main(String[] args) {
        Injector injector = Guice.createInjector(new AbstractModule {
   			public void configure() {
        		bind(Engine.class).annotatedWith(Better.class).to(BetterEngine.class);
    		}
		});
        GuiceMain main = injector.getInstance(GuiceMain.class);
    }
}

public class Car {
    @Inject @Better Engine engine
}

public class BetterEngine extends Engine {
    @Inject Piston piston;
}
```

However, let's look at this a little more closely.  As with our duplicate Car example above, in order to substitute an alternate implementation, Spring requires that we sacrifice type safety.  The compiler has no way to verify that a bean with a qualifier value of "betterEngine" actually exists, or that it is compatible with the Engine type.  You must wait until runtime for the framework to initialize before you discover a possible error.

However, let's look at this a little more closely. The compiler has no way to verify that a bean with a qualifier value of "betterEngine" actually exists, or that it is compatible with the Engine type.  You must wait until runtime for the framework to initialize before you discover a possible error.

Or as an alternative, Spring also lets you create custom annotations to flag special implementations.

```java
@Component
public class Car {
    @Autowired @Better Engine engine;
}

@Better
@Component
public class BetterEngine extends Engine {
    @Autowired
    public BetterEngine(Piston piston) {
        super(piston);  
    }
}

@Target ({ ElementType.TYPE, ElementType.PARAMETER })
@Retention ( RetentionPolicy.RUNTIME )
@Qualifier
public @interface Better{}
```


***
