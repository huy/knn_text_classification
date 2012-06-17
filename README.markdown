# Solution for populate synonym's table automatically

**Modeling the problem**

The task is similar to problem of text classification. Text classification is the problem of identifying
which category a new observed document (aka test document) belongs, on the basis of a training set of data 
containing documents whose category membership is known.

In our case, a training set is new code table in which each code represents one category, the code's 
description represents a document of known category - known document. New observed document - unknown document
is a code of a previouly enriched code table. 

The problem will be solved by using two popular algorithms 

* Multinominal Naive Bayes
* K-Nearest Neighbor

Both algorithms are implemented using corresponding pseudo code taken from Text Book "Introduction to 
Information Retrieval" by Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze.
 
**Multinominal Naive Bayes**

In this algorithm we will try to find a category (i.e a code of new table) with maximum P(c|d) - probability of 
category given a document. 

**K-Nearest Neighbor Weighted Proximity**

In K-Nearest Neighbor algorithm we look for a K - known documents (K is predefined parameter) that are "closest" 
to a test document then assign the test document to the same category of these documents with majority. 

Documents are represented using tf*idf vector space model. The distance between two document is consine similarity 
of their vectors. Documents are represented using tf*idf vector space model. 

**Term's processing**

In order to use these algorithms, code's description as well as description of it's synonyms has to be tokenized, 
stop worlds has to be removed and terms are to be stemmed 

* the list of stop words is borrowed from ENGLISH_STOP_WORDS of lucene libarary
* the implementation of stemming is borrowed from http://tartarus.org/~martin/PorterStemmer/java.txt 

**Implementation**

Except borrowed stemming code which is in Java, code is written in Scala. 

**Build**

To build project we need sbt tool, which can be install following [this instruction] (https://github.com/harrah/xsbt/wiki).

    $sbt
    >compile # compile
    >test  # run test
    >proguard # create self executed jar

**Run**

Using java

    java -jar ./target/scala-2.9.2/text_classification_2.9.2-1.0.min.jar
    --params:
    Map()
    java -jar text_classification_2.9.2-1.0.min.jar --algo=nb|1nn|2nn|... --new-table=filename --existing-table=filename --result-table=filename [--code-id=id,...] [--debug]

Using scala

    scala ./target/scala-2.9.2/text_classification_2.9.2-1.0.jar
    --params:
    Map()
    scala -jar text_classification_2.9.2-1.0.jar --algo=nb|1nn|2nn|... --new-table=filename --existing-table=filename --result-table=filename [--code-id=id,...] [--debug]
    
