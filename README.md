Joey
====

Rationale
---------

Testing is a major part of any enterprise software development life
cycle. Testing should be automated to the maximum extent possible; and
writing automated tests should be easy and fun, else developers won't
do it. Conventional testing frameworks for Java, JavaScript,
etc. offer fine-grained control but do not make the process of writing
tests particularly easy or convenient. An alternative might be to
extend a programming or scripting language with special constructs
that allow test cases to be written quickly, with many details such as
generating test data and making assertions on results implicit,
leaving the programmer to only specify what needs to be tested, what
the input should look like, and what the expected output is.

Summary
-------

Joey is an integration testing framework for RESTful services. It is
named after the character Joey Pardella from the movie _Hackers_,
because like its inspiration, it knows nothing about the internals of
the systems it tests. Instead it "throws commands" (in the form of
HTTP requests) at them, observes the results, and compares them
against expected results.

Joey is written in Kawa Scheme and runs on top of the JVM. The use of
Scheme allows for Joey to be controlled and programmed with special
language extensions built atop Scheme that are specific to
testing. New tests can be written quickly and easily.

Joey supports the following features:

* Extensional testing of a Web service by serving as an HTTP client

* GET, POST, PUT, DELETE methods

* Test cases are written in Scheme with an embedded testing DSL

* Generation of randomized test data

* Results are matched to templates
