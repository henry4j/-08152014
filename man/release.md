* [FAQ: Logical Maven Project Artifact Versioning](http://stackoverflow.com/questions/14346055/logical-maven-version-numbering)
* [HOW-TO: Performing Release in Batch Mode](http://maven.apache.org/maven-release/maven-release-plugin/examples/non-interactive-release.html)
* [JBoss Release Process](https://community.jboss.org/wiki/JBossCommonCoreReleaseProcess) & [JBoss Proj. Versioning](https://community.jboss.org/wiki/JBossProjectVersioning)
* [Spring Data JPA release and snapshot versions](http://www.springsource.org/spring-data/jpa)
* [OpenMRS Release Process](https://wiki.openmrs.org/display/docs/Release+Process)

Let's say that we just released 1.2.3.
Our next release version will be 1.2.4.
So what will be our development version?

1.2.4-SNAPSHOT is the "in-progress" default.
Usually for big projects the numbered releases would be

* 1.2.4-M1, 1.2.4-M2 (milestone builds)
* 1.2.4-Beta1, 1.2.4-Beta2 (beta builds)
* 1.2.4-RC (release candidate)
* 1.2.4 (or sometimes 1.2.4-FINAL)

and in between all those big releases, the *-SNAPSHOT keeps coming out all the time (sometimes several times a day) with getting their own versions. Logically, the latest snapshot overrides any previous ones (though some repositories store a few of the previous snapshots as well).
