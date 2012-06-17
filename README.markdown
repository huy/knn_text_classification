# Solution for populate synonym's table automatically

## Modeling the problem

The task is similar to problem of text classification. Text classification is the problem of identifying
which category a new observed document (aka test document) belongs, on the basis of a training set of data 
containing documents whose category membership is known.

In our case, a training set is new code table in which each code represents one category, the code's 
description represents a document of known category - known document. New observed document - unknown document
is a code of a previouly enriched code table. 

The problem will be solved by using two popular algorithms 

* Multinominal Naive Bayes
* K-Nearest Neighbor

Both algorithms are implemented using corresponding pseudo code taken from Text Book [Introduction to 
Information Retrieval](http://nlp.stanford.edu/IR-book/) by Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze.
 
**Multinominal Naive Bayes**

In this algorithm we will try to find a category (i.e a code of new table) with maximum P(c|d) - probability of 
category given a document. 

**K-Nearest Neighbor by Weighted Proximity**

In K-Nearest Neighbor algorithm we look for a K - known documents (K is predefined parameter) that are "closest" 
to a test document then assign the test document to the same category of these documents with majority. 

Documents are represented using tf*idf vector space model. The proximity between two document is consine similarity 
of their vectors. 

**Term's processing**

In order to use these algorithms, code's description as well as description of it's synonyms has to be tokenized, 
stop worlds has to be removed and terms are to be stemmed 

* the list of stop words is borrowed from ENGLISH_STOP_WORDS of http://lucene.apache.org/
* the implementation of stemming is borrowed from http://tartarus.org/~martin/PorterStemmer/java.txt 

## Implementation

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

Using scala

    scala ./target/scala-2.9.2/text_classification_2.9.2-1.0.jar
    
Using sbt

    sbt
    >run

**Examples**

Enrich new code table in file `new.txt` by Naive Bayes Algorithm using previously enriched table `existing.txt` and write result after enrichment to `out.txt`

    scala -jar text_classification_2.9.2-1.0.jar --algo=nb --new-table=new.txt --existing-table=existing.txt \
    --result-table=out.txt 

Enrich new code table in file `new.txt` by KNN Algorithm with K=3 using two codes from previously enriched table `existing.txt`, write debug info and the result to stdout

    scala -jar text_classification_2.9.2-1.0.jar --algo=3nn --new-table=new.txt --existing-table=existing.txt \
    --result-table=out.txt --code-id=311,142 --debug

**File format of a code table**

Code table is ascii file with the following format e.g.

    1       Administrator  # id  and desc of a code
    -       system administrator # synnonm
    -       network administrator
    -       Database Administrator
    434     Recruitment Consultant
    -       On-site Consultant
    -       Senior Recruitment Consultant


