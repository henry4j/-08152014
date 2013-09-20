**Spring Framework [1.2 (Mar 2004)](http://static.springsource.org/spring/docs/1.2.x/reference/), J2SE 5.0 (Sept 2006), [2.5 (Nov 2006)](http://static.springsource.org/spring/docs/2.5.x/reference/), [3.0 (Dec 2009)](http://static.springsource.org/spring/docs/3.0.0.RELEASE/spring-framework-reference/htmlsingle/), and [3.1 (Dec 2011)](http://static.springsource.org/spring/docs/3.1.x/spring-framework-reference/htmlsingle/spring-framework-reference.html#beans-setter-injection)**.

### [Spring Dependency Injection Styles](http://www.youtube.com/watch?v=dJh84cjMY3E)

#### DI is just OO done right -- DIP and SRP OO principles done right.

* **DO inject depedencies** by constructors or setters instead of looking them up, which is an anti-pattern.
* TransferService below has a depedency on 'accounts'. Where the 'accounts' object come from? How to get a hold of it?

```java
public class TransferService {
    public TransferService(AccountRepo accounts) {
        this.accounts = accounts; // 'DI' allows the transfer method to remain simple, testable, etc.
    }

    public void transfer(double amount, String fromId, String toId) {
        // our service becomes inflexible, and can't be unit-tested through all forms of 'dependency lookup'.
        AccountRepo accounts = new JdbcAccountRepo(dependencies...); // instantiate directly?
            accounts = AccountRepoFactory.getInstance(); // lookup by a factory of prod. infrastructures?
            accounts = jndiContext.lookup("accounts"); // lookup by JNDI (Java naming & directory interface)?
        Account from = accounts.findById(fromId);
        Account to = accounts.findById(toId);
        from.debit(amount);
        to.credit(amount);
    }
}
```

##### Sidebar: Constructor-based or setter-based DI?

* Since you can mix both, **constructor-** and **setter-based DI**, it is a good rule of thumb to use constructor arguments for mandatory dependencies and setters for optional dependencies. The disadvantage is that the object becomes less amenable to reconfiguration and re-injection.
* The Spring team generally advocates setter injection, because large numbers of constructor arguments can get unwieldy, especially when properties are optional. Note that the use of a @Required annotation on a setter can be used to make setters required dependencies.

##### Sidebar: [SOLID design principles identified by Robert C. Martin](http://en.wikipedia.org/wiki/SOLID_\(object-oriented_design\))

* **SRP** (single responsibility p-), **OCP** (open-closed p-), **LSP** (liskov substitution p-), **ISP** (interface segregation p-) and **DIP** (dependency inversion p-)

##### DI style without the Spring framework leads to duplicate code, or ends up creating your own framework.

```java
public class TransferScript {
    public static void main(String... args)
            throws InsufficientFundsException, IOException {

        DataSource dataSource = new EmbeddedDatabaseBuilder()
                .setType(EmbeddedDatabaseType.HSQL)
                .addScript("classpath:com/bank/config/schema.sql")
                .addScript("classpath:com/bank/config/seed-data.sql").build();

        Properties props = new Properties();
        props.load(TransferScript.class.getClassLoader()
                .getResourceAsStream("com/bank/config/app.properties"));

        TransferService transferService = new DefaultTransferService(
                new JdbcAccountRepository(dataSource),
                new FlatFeePolicy(Double.valueOf(props.getProperty("flatfee.amount"))));

        transferService.setMinimumTransferAmount(
                Double.valueOf(props.getProperty("minimum.transfer.amount")));

        // generate a random amount between 10.00 and 90.00 dollars
        double amount = (new Random().nextInt(8) + 1) * 10;

        TransferReceipt reciept = transferService.transfer(amount, "A123", "C456");
        System.out.println(reciept);
    }
}
```

#### Spring IoC container -- "the only factory you will ever need."

* Java platform and its functionality lacks the means to organize the basic building blocks into a coherent whole, leaving that task to architects and developers.
* We can use design patterns (factory, abstract factory, builder, decorator, and service locator) that are formulized best practices we must implement ourselves.
* Spring IoC container addresses this concern by providing a formalized means of composing disparate components into a fully working application ready for use.
  * allows us to configure DI using XML, annotation, or in pure Java, and goes far beyond simple DI along with AOP and instrumentation.

<img src="http://static.springsource.org/spring/docs/3.1.x/spring-framework-reference/htmlsingle/images/container-magic.png" />

##### Why so many choices and ways? No silver bullet!

<table>
  <thead>
    <tr>
      <th rowspan="2"></th>
      <th colspan="4">Spring DI styles</th>
    </tr>
    <tr>
      <th colspan="2">XML-based</th>
      <th>annotation-driven</th>
      <th>Java-based</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Highlights</th>
      <td>
        <p>Since Spring 1.0, &lt;beans/&gt;&nbsp;xml</p>
        <ul>
          <li>general-purpose, simple, and flexible.</li>
          <li>allows access to powerful features like AOP, and instrumentation.</li>
        </ul>
      </td>
      <td>
        <p>Since Spring 2.0, &lt;namespace:*&gt; xml and more in 2.5, and 3.x</p>
        <ul>
          <li>reduces large amount of xml boilerplate.</li>
          <li>has become widely-used in most Spring applications.</li>
          <li>has quite comprehensive support in Spring 3.1.</li>
        </ul>
      </td>
      <td>
        <p>Since Spring 2.5</p>
        <ul>
          <li>&lt;context:component-scan/&gt; xml, @Component, @Autowired, and @Qualifier.</li>
          <li>Spring <b>TestContext</b> framework and testing annotations -- @RunWith & @ContextConfiguration.</li>
          <li><a href="http://www.tutorialspoint.com/spring/spring_jsr250_annotations.htm">JSR-250</a>: @PostConstruct, @PreDestroy, and @Resource.</li>
        </ul>
        <p>Since Spring 3.0</p>
        <ul>
          <li>JSR-330 (<a href="http://docs.oracle.com/javaee/6/api/javax/inject/package-summary.html">javax.inject package</a>): @Inject, @Named, and @Singleton.</li>
        </ul>
      </td>
      <td>
        <p>Since Spring 3.0</p>
        <ul>
          <li>@Configuration classes instead of &lt;beans/&gt; xml documents.</li>
          <li>@Bean methods instead of &lt;bean/&gt; xml elements.</li>
        </ul>
      </td>
    </tr>
    <tr>
      <th>Pros & Cons</th>
      <td colspan="2">
        <ul>
          <li>allows for a centralized app '<b>blueprint</b>'.</li>
          <li>can be changed w/o recompilation.</li>
          <li>extremely well-understood in the industry.</li>
          <li>can grow out of control; can be verbose.</li>
          <li>not type-safe; needs special tooling (STS, IDEA, NetBeans).</li>
        </ul>
      </td>
      <td>
        <ul>
          <li>really concise, convenient.</li>
          <li>has become widely-used, esp. for Spring MVC @Controller(s).</li>
          <li>no such app '<b>blueprint</b>' as it is decentralized.</li>
          <li>still requires xml config resource for bootstrap/component-scan and 3rd-party components.</li>
        </ul>
      </td>
      <td>
        <ul>
          <li>type-safe, while allowing for a centralized app '<b>blueprint</b>'.</li>
          <li>can configure any component w/ complete programmatic control.</li>
          <li>requires no special tooling.</li>
          <li>can mix-and-match w/ other styles seamlessly.</li>
        </ul>
      </td>
    </tr>
  </tbody>
</table>

#### xml-based DI without and with 2.0 namespaces

* app-config.xml since Spring 1.0

```xml
<beans
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="http://www.springframework.org/schema/beans"
  xsi:schemaLocation="
    http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd">

  <bean class="org.springframework.beans.factory.config.PropertyPlaceholderConfigurer">
    <property name="location" value="classpath:com/bank/config/app.properties" />
  </bean>

  <bean id="transferService" class="org.springframework.transaction.interceptor.TransactionProxyFactoryBean">
    <property name="transactionManager" ref="transactionManager" />
    <property name="transactionAttributes">
      <value>transfer=PROPAGATION_REQUIRED</value>
    </property>
    <property name="target">
      <bean class="com.bank.service.internal.DefaultTransferService">
        <constructor-arg ref="accountRepository" />
        <constructor-arg ref="feePolicy" />
        <property name="minimumTransferAmount" value="${minimum.transfer.amount}" />
      </bean>
    </property>
  </bean>

  <bean id="accountRepository" class="com.bank.repository.internal.JdbcAccountRepository">
    <constructor-arg ref="dataSource" />
  </bean>

  <bean id="dataSource" class="org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseFactoryBean">
    <property name="databasePopulator">
      <bean class="org.springframework.jdbc.datasource.init.ResourceDatabasePopulator">
        <property name="scripts">
          <list>
            <value>classpath:com/bank/config/schema.sql</value>
            <value>classpath:com/bank/config/seed-data.sql</value>
          </list>
        </property>
      </bean>
    </property>
  </bean>

  <bean id="transactionManager" class="org.springframework.jdbc.datasource.DataSourceTransactionManager">
    <property name="dataSource" ref="dataSource" />
  </bean>

  <bean id="feePolicy" class="com.bank.service.internal.FlatFeePolicy">
    <constructor-arg value="${flatfee.amount}" />
  </bean>
</beans>
```

* app.properties

```txt
flatfee.amount=2.00
minimum.transfer.amount=10.00
```

* app-config.xml since Spring 2.0

```xml
<beans
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="http://www.springframework.org/schema/beans"
  xmlns:context="http://www.springframework.org/schema/context"
  xmlns:jdbc="http://www.springframework.org/schema/jdbc"
  xmlns:tx="http://www.springframework.org/schema/tx"
  xsi:schemaLocation="
    http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
    http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context.xsd
    http://www.springframework.org/schema/jdbc http://www.springframework.org/schema/jdbc/spring-jdbc.xsd
    http://www.springframework.org/schema/tx http://www.springframework.org/schema/tx/spring-tx.xsd">

  <context:property-placeholder location="classpath:com/bank/config/app.properties" />

  <tx:annotation-driven transaction-manager="transactionManager" />

  <bean id="transferService" class="com.bank.service.internal.DefaultTransferService">
    <constructor-arg ref="accountRepository" />
    <constructor-arg ref="feePolicy" />
    <property name="minimumTransferAmount" value="${minimum.transfer.amount}" />
  </bean>

  <bean id="accountRepository" class="com.bank.repository.internal.JdbcAccountRepository">
    <constructor-arg ref="dataSource" />
  </bean>

  <jdbc:embedded-database id="dataSource">
    <jdbc:script location="classpath:com/bank/config/schema.sql" />
    <jdbc:script location="classpath:com/bank/config/seed-data.sql" />
  </jdbc:embedded-database>

  <bean id="transactionManager" class="org.springframework.jdbc.datasource.DataSourceTransactionManager">
    <property name="dataSource" ref="dataSource" />
  </bean>

  <bean id="feePolicy" class="com.bank.service.internal.FlatFeePolicy">
    <constructor-arg value="${flatfee.amount}" />
  </bean>
</beans>
```

* app-config.xml with p namespace since Spring **2.0.x**

```xml
<beans>
  <!-- 'traditional' declaration -->
  <bean name="john-classic" class="com.example.Person">
    <property name="name" value="John Doe" />
    <property name="spouse" ref="jane" />
  </bean>

  <bean name="jane" class="com.example.Person">
    <property name="name" value="Jane Doe" />
  </bean>

  <!-- 'p-namespace' declaration -->
  <bean name="john-modern"
    class="com.example.Person"
    p:name="John Doe"
    p:spouse-ref="jane" />
</beans>
```

* app-config.xml with c namespace since Spring **3.1**

```xml
<beans>
  <!-- 'traditional' declaration -->
  <bean id="foo" class="x.y.Foo">
    <constructor-arg ref="bar" />
    <constructor-arg ref="baz" />
    <constructor-arg value="foo@bar.com" />
  </bean>

  <!-- 'c-namespace' declaration -->
  <bean id="foo" class="x.y.Foo"
    c:bar-ref="bar" c:baz-ref="baz" c:email="foo@bar.com" />
</beans>
```

#### annotation-driven DI since Spring 2.5

* DefaultTransferService.java

```java
package com.bank.service.internal;

import static java.lang.String.*;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class DefaultTransferService implements TransferService {
    private final AccountRepository accountRepository;
    private final FeePolicy feePolicy;
    private double minimumTransferAmount = 1.00;

    @Autowired
    public DefaultTransferService(AccountRepository accountRepository, FeePolicy feePolicy) {
        this.accountRepository = accountRepository;
        this.feePolicy = feePolicy;
    }

    @Override
    public void setMinimumTransferAmount(double minimumTransferAmount) {
        this.minimumTransferAmount = minimumTransferAmount;
    }

    @Override
    @Transactional
    public TransferReceipt transfer(double amount, String srcAcctId, String dstAcctId)
            throws InsufficientFundsException {

        if (amount < minimumTransferAmount)
            throw new IllegalArgumentException(
                    format("transfer amount must be at least $%.2f", minimumTransferAmount));

        Account srcAcct = accountRepository.findById(srcAcctId);
        Account dstAcct = accountRepository.findById(dstAcctId);

        double fee = feePolicy.calculateFee(amount);
        if (fee > 0)
            srcAcct.debit(fee);

        TransferReceipt receipt = new TransferReceipt();
        receipt.setInitialSourceAccount(srcAcct);
        receipt.setInitialDestinationAccount(dstAcct);
        receipt.setTransferAmount(amount);
        receipt.setFeeAmount(fee);

        srcAcct.debit(amount);
        dstAcct.credit(amount);

        accountRepository.updateBalance(srcAcct);
        accountRepository.updateBalance(dstAcct);

        receipt.setFinalSourceAccount(srcAcct);
        receipt.setFinalDestinationAccount(dstAcct);

        return receipt;
    }
}
```

* JdbcAccountRepository.java

```java
package com.bank.repository.internal;

import java.sql.ResultSet;
import java.sql.SQLException;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

@Repository
public class JdbcAccountRepository implements AccountRepository {
    private final JdbcTemplate jdbcTemplate;

    @Autowired
    public JdbcAccountRepository(DataSource dataSource) {
        jdbcTemplate = new JdbcTemplate(dataSource);
    }

    @Override
    public Account findById(String srcAcctId) {
        return jdbcTemplate.queryForObject(
                "select id, balance from account where id = ?",
                new AccountRowMapper(), srcAcctId);
    }

    @Override
    public void updateBalance(Account dstAcct) {
        jdbcTemplate.update("update account set balance = ? where id = ?",
                dstAcct.getBalance(), dstAcct.getId());
    }

    private static class AccountRowMapper implements RowMapper<Account> {
        @Override
        public Account mapRow(ResultSet rs, int rowNum) throws SQLException {
            return new Account(rs.getString("id"), rs.getDouble("balance"));
        }
    }
}
```

* FlatFeePolicy.java

```java
package com.bank.service.internal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class FlatFeePolicy implements FeePolicy {
    private final double flatFee;

    public FlatFeePolicy(@Value("${flatfee.amount}") double flatFee) {
        this.flatFee = flatFee;
    }

    public double calculateFee(double transferAmount) {
        return flatFee;
    }
}
```

* app-config.xml without `transferService`, `accountRepository`, and `feePolicy` beans 

```xml
<beans
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="http://www.springframework.org/schema/beans"
  xmlns:context="http://www.springframework.org/schema/context"
  xmlns:jdbc="http://www.springframework.org/schema/jdbc"
  xmlns:tx="http://www.springframework.org/schema/tx"
  xsi:schemaLocation="
    http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
    http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context.xsd
    http://www.springframework.org/schema/jdbc http://www.springframework.org/schema/jdbc/spring-jdbc.xsd
    http://www.springframework.org/schema/tx http://www.springframework.org/schema/tx/spring-tx.xsd">

  <context:component-scan base-package="com.bank" />

  <context:property-placeholder location="classpath:com/bank/config/app.properties" />

  <tx:annotation-driven transaction-manager="transactionManager" />

  <jdbc:embedded-database id="dataSource">
    <jdbc:script location="classpath:com/bank/config/schema.sql" />
    <jdbc:script location="classpath:com/bank/config/seed-data.sql" />
  </jdbc:embedded-database>

  <bean id="transactionManager" class="org.springframework.jdbc.datasource.DataSourceTransactionManager">
    <property name="dataSource" ref="dataSource" />
  </bean>
</beans>
```

#### pure Java-based DI since Spring 3.0

* AppConfig.java

```java
package com.bank.config;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableTransactionManagement
@PropertySource("classpath:com/bank/config/app.properties")
public class AppConfig {
    @Autowired
    private Environment env;

    @Bean
    public PlatformTransactionManager transactionManager() {
        return new DataSourceTransactionManager(dataSource());
    }

    @Bean
    public DataSource dataSource() {
        return new EmbeddedDatabaseBuilder()
                .setType(EmbeddedDatabaseType.HSQL)
                .addScript("classpath:com/bank/config/schema.sql")
                .addScript("classpath:com/bank/config/seed-data.sql")
                .build();
    }

    @Bean
    public FeePolicy feePolicy() {
        return new FlatFeePolicy(env.getProperty("flatfee.amount", double.class));
    }

    @Bean
    public AccountRepository accountRepository() {
        return new JdbcAccountRepository(dataSource());
    }

    @Bean
    public TransferService transferService() {
        DefaultTransferService transferService =
                new DefaultTransferService(accountRepository(), feePolicy());
        transferService.setMinimumTransferAmount(
                env.getProperty("minimum.transfer.amount", double.class));
        return transferService;
    }
}
```

##### Java-based config example from Spring framework 3.0 reference documentation

* Spring 3.0 supports `@Configuration`, `@Bean`, `@DependsOn`, `@Primary`, `@Lazy`, `@Import`, `@ImportResource`, and `@Value`.
* Spring 3.1 supports `Environment`, `@PropertySource`, `@ComponentScan`, `@EnableTransactionManagement`, `@EnableCaching`, `@EnableWebMvc`, `@EnableScheduling`, `@EnableAsync`, `@EnableAspectJAutoProxy`, `@EnableLoadTimeWeaving`, and `@EnableSpringConfigured`

* [Example: Unit Test w/ bean profiles](http://blog.42.nl/articles/advanced-unit-testing-with-your-spring-configuration), [Full Spring 3.1 Config](http://danwatt.org/2012/07/full-spring-3-1-config/)

```java
package org.example.config;

@Configuration
public class AppConfig {
    private @Value("#{jdbcProperties.url}") String jdbcUrl;
    private @Value("#{jdbcProperties.username}") String username;
    private @Value("#{jdbcProperties.password}") String password;

    @Bean
    public FooService fooService() {
        return new FooServiceImpl(fooRepository());
    }

    @Bean
    public FooRepository fooRepository() {
        return new HibernateFooRepository(sessionFactory());
    }

    @Bean
    public SessionFactory sessionFactory() {
        // wire up a session factory
        AnnotationSessionFactoryBean asFactoryBean = 
            new AnnotationSessionFactoryBean();
        asFactoryBean.setDataSource(dataSource());
        // additional config
        return asFactoryBean.getObject();
    }

    @Bean
    public DataSource dataSource() { 
        return new DriverManagerDataSource(jdbcUrl, username, password);
    }
}
```

#### hybrid-style DI -- XML-centric use of @Configuration classes

* Refer to `app.properties` in xml-based DI.
* Refer to `DefaultTransferService.java`, `JdbcAccountRepository.java`, and `FlatFeePolicy.java` in annotation-driven DI.
* AppConfig.java for 3rd party components.

```java
package com.bank.config;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@ComponentScan("com.bank")
@EnableTransactionManagement
@ImportResource("classpath:com/bank/config/app-config.xml")
public class AppConfig {
    @Autowired
    DataSource dataSource;

    @Bean
    public PlatformTransactionManager transactionManager() {
        return new DataSourceTransactionManager(this.dataSource);
    }
}
```

* app-config.xml for uses of namespaces

```xml
<beans
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="http://www.springframework.org/schema/beans"
  xmlns:context="http://www.springframework.org/schema/context"
  xmlns:jdbc="http://www.springframework.org/schema/jdbc"
  xmlns:jee="http://www.springframework.org/schema/jee"
  xmlns:tx="http://www.springframework.org/schema/tx"
  xsi:schemaLocation="
    http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
    http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context.xsd
    http://www.springframework.org/schema/jdbc http://www.springframework.org/schema/jdbc/spring-jdbc.xsd
    http://www.springframework.org/schema/jee http://www.springframework.org/schema/jee/spring-jee-3.1.xsd
    http://www.springframework.org/schema/tx http://www.springframework.org/schema/tx/spring-tx-3.1.xsd">

  <context:property-placeholder location="classpath:com/bank/config/app.properties" />

  <beans profile="default">
    <jdbc:embedded-database id="dataSource">
      <jdbc:script location="classpath:com/bank/config/schema.sql" />
      <jdbc:script location="classpath:com/bank/config/seed-data.sql" />
    </jdbc:embedded-database>
  </beans>

  <beans profile="production">
    <jee:jndi-lookup id="dataSource" jndi-name="java:comp/env/jdbc/datasource" />
  </beans>
</beans>
```

### Spring annotations

#### Context configuration annotations

These annotations are used by Spring to guide creation and injection of beans.

<table>
  <thead>
    <tr>
      <th>Annotation</th>
      <th>Use</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th align="left">@Autowired</th>
      <td>Constructor, Field, Method</td>
      <td>Declares a constructor, field, setter method, or configuration method to be autowired by type. Items annotated with @Autowired do not have to be public.</td>
    </tr>
    <tr>
      <th align="left">@Configurable</th>
      <td>Type</td>
      <td>Used with <context:spring-configured> to declare types whose properties should be injected, even if they are not instantiated by Spring. Typically used to inject the properties of domain objects.</td>
    </tr>
    <tr>
      <th align="left">@Order</th>
      <td>Type, Method, Field</td>
      <td>Defines ordering, as an alternative to implementing the org.springframework.core.Ordered interface.</td>
    </tr>
    <tr>
      <th align="left">@Qualifier</th>
      <td>Field, Parameter, Type, Annotation Type</td>
      <td>Guides autowiring to be performed by means other than by type.</td>
    </tr>
    <tr>
      <th align="left">@Required</th>
      <td>Method (setters)</td>
      <td>Specifies that a particular property must be injected or else the configuration will fail.</td>
    </tr>
    <tr>
      <th align="left">@Scope</th>
      <td>Type</td>
      <td>Specifies the scope of a bean, either singleton, prototype, request, session, or some custom scope.</td>
    </tr>
  </tbody>
</table>

* Autowiring bean properties

```xml
<bean id="pirate" class="Pirate">
  <constructor-arg value="Long John Silver" />
  <property name="treasureMap" ref="treasureMap" />
</bean>
```

```java
@Autowired
public void directionsToTreasure(TreasureMap treasureMap) {
    this.treasureMap = treasureMap;
}

@Autowired
private TreasureMap treasureMap;

@Autowired
@Qualifier("mapToTortuga")
private TreasureMap treasureMap;

@Required // must be injected, or else Spring throws a BeanInitializationException and context creation fails.
public void setTreasureMap(TreasureMap treasureMap) {
    this.treasureMap = treasureMap;
}
```

##### [Sidebar](http://programmerspitfalls.blogspot.com/2012/06/injecting-map-with-autowired-gives.html): Strongly-typed maps (HashMap, TreeMap, etc) can be autowired, as long as the key type is assignable to `java.lang.String`.

```java
@Resource // @Resource can wire this weakly-typed, enum map, while @Autowired causes FatalBeanException.
private Map<Enum<?>, Long> miles; 
```

##### @Autowired vs. @Resource --- excerpted from Spring Reference Manual 3.1

* If you intend to express annotation-driven injection by name, do not primarily use @Autowired, even if is technically capable of referring to a bean name through @Qualifier values. Instead, use the JSR-250 @Resource annotation, which is semantically defined to identify a specific target component by its unique name, with the declared type being irrelevant for the matching process.
* As a specific consequence of this semantic difference, beans that are themselves defined as a collection or map type cannot be injected through @Autowired, because type matching is not properly applicable to them. Use @Resource for such beans, referring to the specific collection or map bean by unique name.
* @Autowired applies to fields, constructors, and multi-argument methods, allowing for narrowing through qualifier annotations at the parameter level. By contrast, @Resource is supported only for fields and bean property setter methods with a single argument. As a consequence, stick with qualifiers if your injection target is a constructor or a multi-argument method.

#### Streotyping annotations

These annotations are used to stereotype classes with regard to the application tier that they belong to. Classes that are annotated with one of these annotations will automatically be registered in the Spring application context if `<context:component-scan>` is in the Spring XML configuration. In addition, if a `PersistenceExceptionTranslationPostProcessor` is configured in Spring, any bean annotated with `@Repository` will have `SQLExceptions` thrown from its methods translated into one of Spring’s unchecked `DataAccessExceptions`.

<table>
  <thead>
    <tr>
      <th>Annotation</th>
      <th>Use</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th align="left">@Component</th>
      <td>Type</td>
      <td>Generic stereotype annotation for any Spring-managed component.</td>
    </tr>
    <tr>
      <th align="left">@Controller</th>
      <td>Type</td>
      <td>Stereotypes a component as a Spring MVC controller.</td>
    </tr>
    <tr>
      <th align="left">@Repository</th>
      <td>Type</td>
      <td>Stereotypes a component as a repository. Also indicates that SQLExceptions thrown from the component’s methods should be translated into Spring DataAccessExceptions.</td>
    </tr>
    <tr>
      <th align="left">@Service</th>
      <td>Type</td>
      <td>Stereotypes a component as a service.</td>
    </tr>
  </tbody>
</table>

```java
@Component
public class Pirate {
    private String name;
    private TreasureMap treasureMap;

    public Pirate(String name) { this.name = name; }

    @Autowired
    public void setTreasureMap(TreasureMap treasureMap) {
        this.treasureMap = treasureMap;
    }
}

@Component("jackSparrow")
public class Pirate { /* ... */ }
```
##### Sidebar: Specifying scope for auto-configured beans

By default, all beans in Spring, including auto-configured beans, are scoped as singleton. But you can specify the scope using the @Scope annotation. For example:

```java
@Component
@Scope("prototype") // specifies that the pirate bean be scoped as a prototype bean.
public class Pirate { /* ... */ }
```

<img width="426" height="300" src="http://static.springsource.org/spring/docs/3.1.x/spring-framework-reference/htmlsingle/images/singleton.png" />
<img width="426" height="300" src="http://static.springsource.org/spring/docs/3.1.x/spring-framework-reference/htmlsingle/images/prototype.png" />

#### JSR-250 annotations

In addition to Spring’s own set of annotations, Spring also supports a few of the annotations defined by JSR-250, which is the basis for the annotations used in EJB 3.

<table>
  <thead>
    <tr>
      <th>Annotation</th>
      <th>Use</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th align="left">@PostConstruct</th>
      <td>Method</td>
      <td>Indicates a method to be invoked after a bean has been created and dependency injection is complete. Used to perform any initialization work necessary.</td>
    </tr>
    <tr>
      <th align="left">@PreDestroy</th>
      <td>Method</td>
      <td>Indicates a method to be invoked just before a bean is removed from the Spring context. Used to perform any cleanup work necessary.</td>
    </tr>
    <tr>
      <th align="left">@Resource</th>
      <td>Method, Field</td>
      <td>Indicates that a method or field should be injected with a named resource (by default, another bean).</td>
    </tr>
  </tbody>
</table>

Using `@Resource`, you can wire a bean property by name:

```java
public class Pirate {
    @Resource
    private TreasureMap treasureMap;
}
```

In this case, Spring will attempt to wire the "treasureMap" property with a reference to a bean whose ID is "treasureMap". If you’d rather explicitly choose another bean to wire into the property, specify it to the name attribute:

```java
public class Pirate {
    @Resource(name="mapToSkullIsland")
    private TreasureMap treasureMap;
}
```

Using JSR-250’s `@PostConstruct` and `@PreDestroy` methods, you can declare methods that hook into a bean’s lifecycle. For example, consider the following methods added to the Pirate class:

```java
public class Pirate {
    @PostConstruct
    public void wakeUp() {
        System.out.println("Yo ho!");
    }

    @PreDestroy
    public void goAway() {
        System.out.println("Yar!");
    }
}
```

### SpEL (Spring Expression Language)

* SpEL expressions can be used to set property & constructor-arg values, while they can refer to predefined 'systemProperties' variable, and other bean properties.

```xml
<bean id="taxCalculator" class="org.spring.samples.TaxCalculator">
  <property name="defaultLocale" value="#{ systemProperties['user.region'] }"/>
</bean>

<bean id="numberGuess" class="org.spring.samples.NumberGuess">
  <property name="randomNumber" value="#{ T(Math).random() * 100.0 }"/>
</bean>

<bean id="shapeGuess" class="org.spring.samples.ShapeGuess">
  <property name="initialShapeSeed" value="#{ numberGuess.randomNumber }"/>
</bean>
```

```java
public static class FieldValueTestBean
    @Value("#{ systemProperties['user.region'] }")
    private String defaultLocale;
}

public static class PropertyValueTestBean
    private String defaultLocale;

    @Value("#{ systemProperties['user.region'] }")
    public void setDefaultLocale(String defaultLocale) {
        this.defaultLocale = defaultLocale;
    }
}

@Autowired
public MovieRecommender(CustomerPreferenceDao customerPreferenceDao,
                        @Value("#{systemProperties['user.country']}"} String defaultLocale) {
    this.customerPreferenceDao = customerPreferenceDao;
    this.defaultLocale = defaultLocale;
}

@Autowired
public void configure(MovieFinder movieFinder, 
                      @Value("#{ systemProperties['user.region'] }"} String defaultLocale) {
    this.movieFinder = movieFinder;
    this.defaultLocale = defaultLocale;
}
```

#### SpEL language features

<table>
  <thead>
    <tr>
      <th>Type</th>
      <th>Use</th>
      <th>Example</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th align="left">Literal expression</th>
      <td>The types of literal expressions supported are strings, dates, numeric values (int, real, and hex), boolean and null. Strings are delimited by single quotes.</td>
      <td>'Hello Spring'<br/>6.0221415E+23<br/>0x7FFFFFFF<br/>'2012/11/30'<br/>true<br/>null</td>
    </tr>
    <tr>
      <th align="left">Properties, Arrays, Lists, Maps, Indexers</th>
      <td>Navigating with property references is easy, just use a period to indicate a nested property value.<br/>Case insensitivity is allowed for the first letter of property names. The contents of arrays and lists are obtained using square bracket notation.<br/>The contents of maps are obtained by specifying the literal key value within the brackets.</td>
      <td>person.name<br/>person.Name<br/>person.getName()<br/>inventions[3]<br/>Members[0].Inventions[6]<br/>Officers['advisors'][0].PlaceOfBirth.Country</td>
    </tr>
    <tr>
      <th align="left">Inline lists</th>
      <td>Lists can be expressed directly in an expression using {} notation.</td>
      <td>{1,2,3,4}<br/>{{'a','b'},{'x','y'}}</td>
    </tr>
    <tr>
      <th align="left">Inline arrays</th>
      <td>Arrays are built using the familiar Java syntax.</td>
      <td>new int[3][4]<br/>new int[]{1,2,3}</td>
    </tr>
    <tr>
      <th align="left">Methods</th>
      <td>Methods are invoked using typical Java syntax. You may also invoke methods on literals. Varargs are also supported.</td>
      <td>'abc'.substring(2, 3)<br/>hasSpouse('Mihajlo')</td>
    </tr>
    <tr>
      <th align="left" rowspan="2">Relational operators</th>
      <td>The relational operators; equal, not equal, less than, less than or equal, greater than, and greater than or equal are supported using standard operator notation.</td>
      <td>2 == 2 or 2 eq 2<br/>'black' lt 'block' or 'black' &lt; 'block'</td>
    </tr>
    <tr>
      <td>In addition to standard relational operators SpEL supports the 'instanceof' and regular expression based 'matches' operator.</td>
      <td>'xyz' instanceof T(int)<br/>'5.00' matches '^-?\\d+(\\.\\d{2})?$'</td>
    </tr>
    <tr>
      <th align="left">Logical operators</th>
      <td>The logical operators that are supported are and, or, and not.</td>
      <td>hasSpouse('Nikola') and hasSpouse('Mihajlo')<br/>hasSpouse('Nikola') or hasSpouse('Albert')<br/>hasSpouse('Nikola') and !hasSpouse('Mihajlo')</td>
    </tr>
    <tr>
      <th align="left">Mathematical operators</th>
      <td>The addition operator can be used on numbers, strings and dates. Subtraction can be used on numbers and dates. Multiplication and division can be used only on numbers. Other mathematical operators supported are modulus (%) and exponential power (^). Standard operator precedence is enforced.</td>
      <td>1000.00 - -1e4 / 3<br/>1 + 2 - 3 * 4 % 5<br/>'test' + ' ' + 'string'</td>
    </tr>
    <tr>
      <th align="left">Types</th>
      <td>The special 'T' operator can be used to specify an instance of java.lang.Class (the 'type'). Static methods are invoked using this operator as well.</td>
      <td>T(String).format('Hello %s', 'world')<br/>T(Math).random()<br/>T(Math).PI</td>
    </tr>
    <tr>
      <th align="left">Constructors</th>
      <td>Constructors can be invoked using the new operator. The fully qualified class name should be used for all but the primitive type and String.</td>
      <td>new org.spring.samples.spel.Inventor('Albert Einstein', 'German')"</td>
    </tr>
    <tr>
      <th align="left">#this and #root variables</th>
      <td>Although #this may vary as components of an expression are evaluated, #root always refers to the root.</td>
      <td>#primes.?[#this&lt;10]</td>
    </tr>
    <tr>
      <th align="left">Bean references</th>
      <td>If the evaluation context has been configured with a bean resolver it is possible to lookup beans from an expression using the (@) symbol.</td>
      <td>@dataSource</td>
    </tr>
    <tr>
      <th align="left">Ternary operator</th>
      <td>You can use the ternary operator for performing if-then-else conditional logic inside the expression.</td>
      <td>T(Math).random() > .5 ? 'She loves me' : 'She hates me'</td>
    </tr>
    <tr>
      <th align="left">Elvis Operator</th>
      <td>The Elvis operator is a shortening of the ternary operator syntax and is used in the Groovy language.</td>
      <td>singer.Name?:'Elvis Presley'</td>
    </tr>
    <tr>
      <th align="left">Safe Navigation operator</th>
      <td>The Safe Navigation operator is used to avoid a NullPointerException and comes from the Groovy language.</td>
      <td>address.city?.name<br/>person.name?.length()</td>
    </tr>
    <tr>
      <th align="left">Collection selection</th>
      <td>This involves collecting all, first, or last elelemnts that meet the criteria expressed in bracket into a new collection.</td>
      <td>friends.?[age&gt;20]<br/>friends.^[age&lt;30]<br/>friends.$[getAge()&gt;25]</td>
    </tr>
    <tr>
      <th align="left">Collection projection</th>
      <td>This involves collecting a particular property from each of the elements of a collection into a new collection.</td>
      <td>friends.![name]<br/>friends.![name.length()]</td>
    </tr>
    <tr>
      <th align="left">Templated expression</th>
      <td>You can use the expression language to evaluate expressions inside of string expressions. The result is returned. In this case, the result is dynamically created by evaluating the ternary expression and including 'good' or 'bad' based on the result.</td>
      <td>She ${T(Math).random()> .5 ? 'loves' : 'hates'} me</td>
    </tr>
  </tbody>
</table>

##### Glue code and the evil singleton --- excerpted from Spring Reference Manual 3.1

It is best to write most application code in a dependency-injection (DI) style, where that code is served out of a Spring IoC container, has its own dependencies supplied by the container when it is created, and is completely unaware of the container. However, for the small glue layers of code that are sometimes needed to tie other code together, you sometimes need a singleton (or quasi-singleton) style access to a Spring IoC container. For example, third-party code may try to construct new objects directly (Class.forName() style), without the ability to get these objects out of a Spring IoC container. If the object constructed by the third-party code is a small stub or proxy, which then uses a singleton style access to a Spring IoC container to get a real object to delegate to, then inversion of control has still been achieved for the majority of the code (the object coming out of the container). Thus most code is still unaware of the container or how it is accessed, and remains decoupled from other code, with all ensuing benefits. EJBs may also use this stub/proxy approach to delegate to a plain Java implementation object, retrieved from a Spring IoC container. While the Spring IoC container itself ideally does not have to be a singleton, it may be unrealistic in terms of memory usage or initialization times (when using beans in the Spring IoC container such as a Hibernate SessionFactory) for each bean to use its own, non-singleton Spring IoC container.

Looking up the application context in a service locator style is sometimes the only option for accessing shared Spring-managed components, such as in an EJB 2.1 environment, or when you want to share a single ApplicationContext as a parent to WebApplicationContexts across WAR files. In this case you should look into using the utility class ContextSingletonBeanFactoryLocator locator that is described in this SpringSource team blog entry.

##### Best of the best examples: Our profile service components

* [application-config-v3.xml](http://tiny/1boaui1q5/p4dbamazsourp4dbbrazfile) from [application-config-v1.xml](http://tiny/10q7p5rz8/p4dbamazsourp4dbbrazfile)
* [dependencies to Spring 3.1 Core, Beans, Context, Expression, and Test](http://tiny/1ez5l4lap/p4dbamazsourp4dbbrazfile)
* [AppConfig.java](http://tiny/kllelb26/p4dbamazsourp4dbbrazfile)
* [ProfileDao.Java](http://tiny/ljqp8rsp/p4dbamazsourp4dbbrazfile)
* [ProfileTask.java @Profile({ "prod", "devo" })](http://tiny/jnk82ieb/p4dbamazsourp4dbbrazfile)
* [profiles.active="prod,devo"](http://tiny/3gxdodbi/p4dbamazsourp4dbbrazfile)

##### Spring Presentation Slides

* [Spring 3.1 review and 3.2 preview](http://cbeams.github.com/spring-3.1-review/), and [modern config](http://cbeams.github.com/modern-config/) by Chris Beams.
* [Spring 3.1 to 3.2 in a Nutshell](http://www.slideshare.net/sbrannen/spring-31-to-32-in-a-nutshell-sdc2012)
* [Introduction to Spring Framework - what's new in 3.0 & 3.1](http://www.slideshare.net/iceycake/introduction-to-spring-framework)?
* [Spring Framework - Expression Language](http://www.slideshare.net/analizator/spring-framework-expression-language)
* [Spring has got me under its SpEL](http://www.slideshare.net/eadno1/spring-has-got-me-under-its-spel)
* [That old Spring magic has me in its SpEL](http://www.slideshare.net/habuma/spel)