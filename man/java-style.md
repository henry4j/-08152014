##### References

* Eclipse Plugins: CheckStyle, FindBugs, PMD, Jalopy, Sonar, and the like.
* Google Web Toolkit's [style guides](http://code.google.com/webtoolkit/makinggwtbetter.html#codestyle) and [style check rules for Eclipse plugin](http://google-web-toolkit.googlecode.com/svn/trunk/eclipse/settings/code-style/gwt-checkstyle.xml)
* [Eclipse Java styles for Google-originated open-source projects](http://code.google.com/p/google-styleguide/source/browse/trunk/eclipse-java-google-style.xml)
* [Java style rules for Android open-source projects](https://sites.google.com/a/android.com/opensource/submit-patches/code-style-guide)
  * Dislikes: prefixing private instance and static fields with `m` or `s`.

## Code Formatter

* Regarding code formatting, we use the default Eclipse style with a couple of minor tweaks. By default, it is recommended to follow [Oracle/Sun's Java code conventions](http://www.oracle.com/technetwork/java/codeconv-138413.html) and formats.
  * Regarding code indents, we use [1TBS (one true brace style)](http://en.wikipedia.org/wiki/Indent_style#Variant:_1TBS) that is a K&R variant.
  * Java | Code Style | Formatter | Import... and open [eclipse-bless.xml](https://raw.github.com/henry4j/-/master/eng/eclipse-blessed.xml) --- contains these settings:
     * `<setting id=".formatter.join_wrapped_lines" value="false"/>`
     * `<setting id=".formatter.lineSplit" value="130"/>`
     * `<setting id=".formatter.tabulation.char" value="space"/>`

* General | Editors | Text Editors
  * Show whitespace characters
  * Insert spaces for tabs --- this is text and xml sources

### Examples

--- excerpted from [GetAttachmentHandler.java](https://github.com/henry4j/-/blob/master/eng/GetAttachmentHandler.java)

#
    public void execute(final ActiveSyncContext syncContext, final AbstractHttpClient httpClient, final HttpContext httpContext,
            final String attachmentName, final Action<InputStream, String> reader)
            throws IOException {
        this.reader = reader;
        final URI requestUri = getRequestURI(syncContext, attachmentName);
        if (syncContext.isDebugEnabled()) {
            logRequestUri(logger, requestUri);
        }
        @SuppressWarnings("unused")
        String ignore = executeHttpPost(syncContext, httpClient, httpContext, new HttpPostFunction() {
            public HttpPost apply() throws IOException {
                HttpPost httpPost = new HttpPost(requestUri);
                httpPost.setHeaders(syncContext.getGetAttachmentRequestHeaders());
                return httpPost;
            }
        });
    }

    @Override
    protected ResponseHandler<String> responseHandlerOf(final ActiveSyncContext syncContext) {
        return new ResponseHandler<String>() {
            @Override
            public String handleResponse(HttpResponse response)
                    throws IOException {
                if (HttpStatus.SC_OK != response.getStatusLine().getStatusCode()) {
                    // We throw HttpResponseException just like the basic response handler does.
                    StatusLine statusLine = response.getStatusLine();
                    throw new HttpResponseException(statusLine.getStatusCode(), statusLine.getReasonPhrase());
                }
                Header[] headers = response.getHeaders("Content-Type");
                String contentType = headers.length == 0 ? null : headers[0].getName();
                BufferedInputStream bis = new BufferedInputStream(response.getEntity().getContent());
                if (syncContext.isDebugEnabled()) {
                    SyncHandlerBase.logResponseStream(logger, bis);
                }
                reader.apply(bis, contentType);
                return null;
            }
        };
    }

##### Fluent [MapMaker](http://google-collections.googlecode.com/svn/trunk/javadoc/com/google/common/collect/MapMaker.html) interface --- excerpted from [next generation Java programming style](http://codemonkeyism.com/generation-java-programming-style/)

    ConcurrentMap graphs = new MapMaker()
            .concurrencyLevel(32)
            .softKeys()
            .weakValues()
            .expiration(30, TimeUnit.MINUTES)
            .makeComputingMap(
                new Function() {
                    public Graph apply(Key key) {
                        return createExpensiveGraph(key);
                    }
                });

## Purpose of Code Formatting

* Code formatting is too important to ignore. It is about communication, and communication is the professional software design engineer's first order of business. Perhaps you thought "getting it working" was the first order of business. The function you create today has a chance of changing in the next cycles, but the readability of your code has a profound effect on all the changes that will ever be made. The coding style, discipline, and readability survies, long after the original code has been changed beyond recognition, and continues to affect maintainability and extensibility.
* We should choose a set of simple rules that govern the format of your code, and consistently apply those rules. As we are working on a team, the team should agree to a single set of formatting rules and all members should comply. Or, we will see a scrambled mass of code that looks like our code was written by a bevy of drunken sailors.

### Vertical Formatting

* We build significant systems out of files that are typically 200 lines long with an upper limit of 500 --- not a hard and fast rule, but very desirable for readability.

##### Vertical Ordering upon Newspaper Article Metaphor

* We expect called functions should be placed below calling functions to create a nice flow down the source code module from high level to low level.
* We expect the most important concepts to come first, and expressed with the least amount of polluting details, and the least important detail to come last.
* We order members within a class definition:
  * Constants (sorted by modifiers, though beware public)
  * Inner enum classes and interfaces
  * Instance fields (should all be private, or sorted by modifiers)
  * Static fields (again, should all be private)
  * Constructors (static constructor first, if it exists)
  * Abstract methods (ordered by modifiers)
  * Implementation methods (also called overridden methods)
  * Methods - public
  * Methods - getters/setters
  * Methods - IoC (inversion of control)
  * Methods - protected, package, and then private
  * Inner classes (internally, should mirror the above ordering)

##### Vertical Openness and Density -- generally conserve, unless readability ...

* Each line represents an expression, or a clause, and each group of lines represents a complete thought. Each blank line of a visual clue closely identifies a new and separate concept.

##### Vertical Distance

* Variables should be declared as close to their usage as possible; instance variables should be declared in one well-known place --- in C++, we put instance variables at the bottom (also called _scissor rule_), but in Java, we put them all at the top of the class
* While **dependent functions** should be vertically close, caller functions (dependent) should be defined above callee functions (dependee).
* Due to **conceptual affinity**, certain bits of code wants to be near other bits, e.g. when a group of functions performs conceptually similar operations.

### Horizontal Formatting 

* [Eclipse default line of 80 chars is out of date](http://www.vogella.de/blog/2011/09/06/eclipse-default-line-length-of-80-chars-outdate/). 130 characters should be optimum on a 24" screen, and we can even have 2 Java editors next to each other. Java language is not for 80 character long lines because the number of lines doubles with a few generics.

##### Horizontal Openness and Density

* White spaces to associate strongly related things and disassociate weekly related things --- space around assignment operators; no space between function name and opening parenthesis; no space around factors, but space around terms.

#
    public class Quadratic {
        public static double root1(double a, double b, double c) {
            double determinant = determinant(a, b, c);
            return (-b + Math.sqrt(determinant)) / (2*a);
        }

        public static double root2(int a, int b, int c) {
            double determinant = determinant(a, b, c);
            return (-b - Math.sqrt(determinant)) / (2*a);
        }

        private static double determinant(double a, double b, double c) {
            return b*b - 4*a*c;
        }
    }

##### Horizontal Alignment

* DO NOT ALIGN horizontally, as the problem is the length of declarations, not the lack of alignment. This long list of declarations suggests that a class, or a function should be split up.

#
    public class FitNesseExpediter implements ResponseSender {
        private   Socket          socket;
        private   InputStream     input;
        private   OutputStream    output;
        private   Request         request;
        private   Response        response;
        private   FitNesseContext context;
        protected long            requestParsingTimeLimit;
        private   long            requestProgress;
        private   long            requestParsingDeadline;
        private   boolean         hasError;
    
        public FitNesseExpediter(Socket          s,
                                 FitNesseContext context) throws Exception {
            this.context =            context;
            socket =                  s;
            input =                   s.getInputStream();
            output =                  s.getOutputStream();
            requestParsingTimeLimit = 10000;
        }

##### [1TBS Indentation Style (K&R variation)](http://en.wikipedia.org/wiki/Indent_style#Variant:_1TBS)

* While Java is often written in Allman or other styles, a significant body of Java code uses a minor variant of the K&R style in which the opening brace is on the same line as the class or method declaration, largely because Sun's original style guides ([pdf](https://docs.google.com/viewer?url=http%3A%2F%2Fwww.oracle.com%2Ftechnetwork%2Fjava%2Fcodeconventions-150003.pdf
), or [html](http://www.oracle.com/technetwork/java/javase/documentation/codeconvtoc-136057.html)) used this K&R variant, and as a result most of the standard source code for the Java API is written in this style. It is also a popular indent style for ActionScript and JavaScript, along with the Allman style.
* It is sometimes tempting to break the indentation rule for short `if` statement, `while` loops, or short functions. But, always go back and put them back in.

##### More examples

* wrapping: extends goes to the same line as the class declaration, but implements and throws go to the next line.
* import: do not use wildcard for imports.
* exceptional formatting for readability and comprehension.

* DO PROPERLY indent and surround by braces empty `for` or `while` statements.

#
    while (dis.read(buf, 0, readBufferSize) != -1) {
    }

* DO PLACE operators at the beginning of the next line to help readers see the continuation, and line up parenthetical expressions that span multiple lines with the expression it beings to.

#
    if ((a == b
         || b == c)
        && d != e) {
        ...

##### See also

http://programmers.stackexchange.com/questions/127128/what-is-the-best-style-for-functions-with-multiple-returns-and-if-else-statement
