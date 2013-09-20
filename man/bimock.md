##### BimockTest.java

* [src/test/resources/test-record-and-replay-list.json](https://github.com/henry4j/-/blob/master/algorist/java/src/test/resources/test-record-and-replay-list.json)
* [src/test/resources/test-record-and-replay-map.json](https://github.com/henry4j/-/blob/master/algorist/java/src/test/resources/test-record-and-replay-map.json)

```java
package com.henry4j.core;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import lombok.val;

import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

import com.henry4j.core.Bimock;
import com.henry4j.core.Bimock.Mode;

public class BimockTest {
    private Mode mode = Mode.Replay;
    private List<Long> list;

    @Test
    public void testRecordAndReplay() {
        val map = Bimock.of(new HashMap<String, Integer>(), mode, 
                new File("src/test/resources/test-record-and-replay-map.json"));
        assertThat(map.put("abc", 3), equalTo(null));
        assertThat(map.size(), equalTo(1));
        assertThat(map.get("abc"), equalTo(3));

        val l = Bimock.of(new ArrayList<Long>(), mode, 
                new File("src/test/resources/test-record-and-replay-list.json"));
        ReflectionTestUtils.setField(this, "list", l); // this can also be injected by ReflectionUtils.
        try {
            list.remove(-1);
            fail();
        } catch (ArrayIndexOutOfBoundsException e) {
            assertThat(e.getMessage(), equalTo("-1"));
        }
        assertThat(list.add(100L), equalTo(true));
        assertThat(list.toArray(new Long[1]), equalTo(new Long[] { 100L }));
    }
}
```

#### Bimock.java

```java
package com.henry4j.core;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.SneakyThrows;
import lombok.experimental.Accessors;

import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.google.common.base.Charsets;
import com.google.common.io.Files;

public class Bimock {
    private static String LINE = System.getProperty("line.separator");
    private static PojoMapper POJO_MAPPER = new PojoMapper(new MixIns());

    @SneakyThrows({ IOException.class })
    public static <T> T of(T object, Mode mode, final File resource) {
        if (Mode.Record == mode) {
            if (resource.exists()) {
                resource.delete();
            }
            @SuppressWarnings("unchecked")
            T mock = mock((Class<T>)object.getClass(), withSettings()
                    .spiedInstance(object)
                    .defaultAnswer(new Answer<T>() {
                @Override
                public T answer(InvocationOnMock iom) throws Throwable {
                    Object[] args = POJO_MAPPER.copyOf(iom.getArguments());
                    try {
                        T success = (T)iom.callRealMethod();
                        Files.append(Invocation.toJson(success, null, iom.getMethod(), args) + LINE, resource, Charsets.UTF_8);
                        return success;
                    } catch (Throwable failure) {
                        Files.append(Invocation.toJson(null, failure, iom.getMethod(), args) + LINE, resource, Charsets.UTF_8);
                        throw failure;
                    }
                }
            }));
            return mock;
        } else {
            @SuppressWarnings("unchecked")
            T mock = mock((Class<T>)object.getClass(), withSettings()
                    .spiedInstance(object)
                    .defaultAnswer(new Answer<T>() {
                        @Override
                        public T answer(InvocationOnMock invocation) throws Throwable {
                            throw new RuntimeException("UNCHECKED: this bug should go unhandled, as we come across unstubbed invocation(s).");
                        }
                    }));
            for (String line : Files.readLines(resource, Charsets.UTF_8)) {
                Invocation invocation = POJO_MAPPER.fromJson(line, Invocation.class);
                if (null != invocation.failure()) {
                    invocation.replay(doThrow(invocation.failure()).when(mock));
                } else {
                    if (Void.TYPE.equals(invocation.method().getReturnType())) {
                        invocation.replay(doNothing().when(mock));
                    } else {
                        invocation.replay(doReturn(invocation.success()).when(mock));
                    }
                }
            }
            return mock;
        }
    }

    public static enum Mode {
        Record, Replay
    }

    @Data
    @Accessors(fluent = true)
    @NoArgsConstructor @AllArgsConstructor(staticName = "of")
    public static class Invocation {
        private Object success;
        private Throwable failure;
        private Method method;
        private Object[] arguments;

        public static String toJson(Object success, Throwable failure, Method method, Object[] arguments) {
            return POJO_MAPPER.toJson(Invocation.of(success, null, method, arguments));
        }

        @SneakyThrows({ InvocationTargetException.class, IllegalAccessException.class })
        public Object replay(Object object) {
            return ReflectionUtils.findMethod(method.getDeclaringClass(), method.getName(), method.getParameterTypes()).invoke(object, arguments);
        }
    }

    @Component // this module needs to be Spring-auto-wired into PojoMapper's constructor.
    public static class MixIns extends SimpleModule {
        private static final long serialVersionUID = -2479398644334238459L;

        public MixIns() {
            super("Bimock.MixIns", new Version(1, 0, 0, "", "", ""));
            setMixInAnnotation(Method.class, MethodMixIn.class);
            setMixInAnnotation(Throwable.class, ThrowableMixIn.class);
        }

        @JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "@class")
        static abstract class ThrowableMixIn {
        }

        @JsonIgnoreProperties({ "override", "clazz", "annotations", "parameterAnnotations", "root", "synthetic", "typeParameters", "declaredAnnotations", "genericReturnType", "genericParameterTypes", "genericExceptionTypes", "bridge", "varArgs", "accessible" })
        static abstract class MethodMixIn {
            @JsonCreator
            MethodMixIn(
                    @JsonProperty("declaringClass") Class<?> declaringClass,
                    @JsonProperty("name") String name,
                    @JsonProperty("parameterTypes") Class<?>[] parameterTypes,
                    @JsonProperty("returnType") Class<?> returnType,
                    @JsonProperty("checkedExceptions") Class<?>[] checkedExceptions,
                    @JsonProperty("modifiers") int modifiers,
                    @JsonProperty("slot") int slot,
                    @JsonProperty("signature") String signature,
                    @JsonProperty("annotations") byte[] annotations,
                    @JsonProperty("parameterAnnotations") byte[] parameterAnnotations,
                    @JsonProperty("annotationDefault") byte[] annotationDefault) {
             }
        }
    }
}
```

##### PojoMapper.java

```
package com.henry4j.core;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;

import lombok.Cleanup;
import lombok.SneakyThrows;

import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonTypeInfo.As;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.Module;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectMapper.DefaultTyping;

@Component
public class PojoMapper {
    private JsonFactory jsonFactory = new JsonFactory();
    private ObjectMapper objectMapper = new ObjectMapper()
            .configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true)
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .enableDefaultTyping(DefaultTyping.NON_CONCRETE_AND_ARRAYS, As.PROPERTY)
            .setSerializationInclusion(Include.NON_NULL) // excludes null-valued properties.
            .setVisibility(PropertyAccessor.FIELD, Visibility.ANY);

    @Autowired
    public PojoMapper(Module... modules) {
        for (Module m : modules) {
            objectMapper.registerModule(m);
        }
    }

    @SneakyThrows({ JsonParseException.class, IOException.class })
    public <T> T fromJson(byte[] bytes, Class<T> pojoClass) {
        return objectMapper.readValue(bytes, pojoClass);
    }

    @SneakyThrows({ JsonParseException.class, IOException.class })
    public <T> T fromJson(String string, Class<T> pojoClass) {
        return objectMapper.readValue(string, pojoClass);
    }

    @SneakyThrows({ JsonParseException.class, IOException.class })
    public <T> T fromJson(String input, TypeReference<T> typeRef) {
        return objectMapper.readValue(input, typeRef);
    }

    @SneakyThrows({ JsonParseException.class, IOException.class })
    public <T> T fromJson(InputStream input, Class<T> pojoClass) {
        return objectMapper.readValue(input, pojoClass);
    }

    @SneakyThrows({ JsonParseException.class, IOException.class })
    public <T> T fromJson(Reader input, Class<T> pojoClass) {
        return objectMapper.readValue(input, pojoClass);
    }

    public <T> String toJson(T pojo) {
        return toJson(pojo, false);
    }

    @SneakyThrows({ JsonGenerationException.class, IOException.class })
    public <T> String toJson(T pojo, boolean prettyPrint) {
        StringWriter writer = new StringWriter();
        @Cleanup JsonGenerator jg = jsonFactory.createJsonGenerator(writer);
        if (prettyPrint) {
            jg.useDefaultPrettyPrinter();
        }
        objectMapper.writeValue(jg, pojo);
        return writer.toString();
    }

    @SneakyThrows({ JsonGenerationException.class, IOException.class })
    public <T> OutputStream toJson(T pojo, OutputStream output, boolean prettyPrint) {
        @Cleanup JsonGenerator jg = jsonFactory.createJsonGenerator(output);
        if (prettyPrint) {
            jg.useDefaultPrettyPrinter();
        }
        objectMapper.writeValue(jg, pojo);
        return output;
    }

    @SneakyThrows({ JsonGenerationException.class, IOException.class })
    public <T> Writer toJson(T pojo, Writer output, boolean prettyPrint) {
        @Cleanup JsonGenerator jg = jsonFactory.createJsonGenerator(output);
        if (prettyPrint) {
            jg.useDefaultPrettyPrinter();
        }
        objectMapper.writeValue(jg, pojo);
        return output;
    }

    public <T> T copyOf(T pojo) {
        @SuppressWarnings("unchecked")
        Class<T> pojoClass = (Class<T>)pojo.getClass();
        return fromJson(toJson(pojo), pojoClass);
    }
}
```

##### TODO: `@Bimock(mode = RecordOrReplay, file = new File("test-record-and-replay-map.json"))`

* excerpted from InjectingAnnotationEngine.java

```
public void injectMocks(final Object testClassInstance) {
    Class<?> clazz = testClassInstance.getClass();
    Set<Field> mockDependentFields = new HashSet<Field>();
    Set<Object> mocks = newMockSafeHashSet();
    
    while (clazz != Object.class) {
        new InjectMocksScanner(clazz).addTo(mockDependentFields);
        new MockScanner(testClassInstance, clazz).addPreparedMocks(mocks);
        clazz = clazz.getSuperclass();
    }
    
    new DefaultInjectionEngine().injectMocksOnFields(mockDependentFields, mocks, testClassInstance);
}

public class DefaultInjectionEngine {
    public void injectMocksOnFields(Set<Field> needingInjection, Set<Object> mocks, Object testClassInstance) {
        MockInjection.onFields(needingInjection, testClassInstance)
                .withMocks(mocks)
                .tryConstructorInjection()
                .tryPropertyOrFieldInjection()
                .handleSpyAnnotation()
                .apply();
    }
}
```