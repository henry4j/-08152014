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

***
