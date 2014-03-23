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

@BindingAnnotation
@Target({ ElementType.FIELD, ElementType.PARAMETER, ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
public @interface Better {}
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

@Qualifier
@Target ({ ElementType.TYPE, ElementType.PARAMETER })
@Retention ( RetentionPolicy.RUNTIME )
public @interface Better{}
```

Look familiar? This is very similar to the Guice approach with one major difference: you're annotating the class to be autowired, rather than the relationship between the class and its injection point.  As such, this implementation is only providing the illusion of type safety because the compiler has no way to know if a class annotated with @Better actually matches the parameter annotated with @Better.  What's to stop me from doing the following at compile-time instead of annotating Engine?

```
@Better
@Component
public class NotAnEngine {}
```

Guice avoids all this confusion in two ways: it uses actual types to wire dependencies together rather than bean names (or qualifier values), and it requires the module be in charge of resolving ambiguities using bindings.  In the Guice example above, the special binding annotation @Better not only flags the injection point of Engine, but the module MyModule then binds that annotation to the desired implementation (BetterEngine).  No need for string identifiers.  No need to understand semantic differences between @Qualifier and @Resource or to wonder whether you're wiring by qualifier value or by bean id. And everything is type-checked at compile time.

#### Key Distinction

Regardless of whether your framework is Spring or Guice, if you need to add a new dependency to 50 different classes, you must modify 50 different source files to declare that dependency.  However, Spring Java configuration also requires modifying 50 different @Bean methods to manually wire in that new dependency.  Guice, conversely, can handle it with a single line, or in the case of a new concrete dependency, no lines at all.  It’s difficult to understate the impact of this feature on code reusability.



***
