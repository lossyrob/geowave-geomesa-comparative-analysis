# GeoMesa and GeoWave Comparative Analysis: Final Report

###  Abstract

> This document details the results of a comparative analysis between two open source geospatial big data frameworks: GeoWave and GeoMesa.
> A feature comparison and a set of performance tests with analysis are presented.
> We conclude that although there is a large set of overlapping features, most specifically around indexing
> and querying spatial and spatiotemporal data in Accumulo, the projects should not be considered
> the same. Through analyzing performance test data, we make three conclusions about the performance characteristics
> of the current versions of the systems (GeoMesa 1.2.6 and GeoWave 0.9.3) for use case of indexing spatial and spatiotemporal data in Accumulo:
> 1. GeoMesa performed better against queries with large result counts while GeoWave performed better
> on smaller result sets; 2. GeoWave performed better against queries with larger temporal bounds, while
> GeoMesa performed better when the temporal bounds were smaller (around a couple of weeks or less);
> and 3. that GeoWave outperforms GeoMesa in multitenancy use cases, where there are 16 to 32 queries being
> executed against the system in parallel. We also find that the two systems perform reasonably well
> in all cases, and that neither system was dominant in performance characteristics. We provide recommendations
> for was the two projects can collaborate moving forward in light of this analysis.

## Introduction

The GeoMesa and GeoWave open source projects have been in development for several years in support of various “Big Data Analytics” programs. Each project provides the ability to query vector features in Accumulo. New software projects that want to utilize this functionality are faced with the decision to choose one project over the other. It is the aim of this paper to help determine the use cases where one project might be superior to the other.

In Summer of 2016, Azavea, the developer of the GeoTrellis project, conducted a comparative analysis of the features, abilities, and limitations of each of these projects. Because the GeoTrellis team already had some familiarity with both projects and Accumulo, the GeoTrellis team was in a unique position to lead such an analysis. The team has worked to perform the analysis with an unbiased, independent perspective.

Each project has unique features that provide functionality that can not be compared meaningfully. However, a core functionality of both projects is their ability to query vector features from Accumulo. We will discuss these unique features, but the focus of this paper will be the apples-to-apples comparison of the performance tests, to the extent that this is possible, of vector features provided in the context of use on top of Accumulo.

## Project Backgrounds

GeoWave and GeoMesa are part of the LocationTech Working Group under the Eclipse Foundation. This collaboration improves accessibility and outreach to software and application developers. Both projects are open sourced under the Apache version 2.0 license.

## Architectural overview

GeoWave and GeoMesa are designed to enable large-scale geospatial processing on cloud and distributed computing systems. Space Filling Curve (SFC) is the underlying method that describes the transformation of multidimensional data to a 1 dimensional number line.

Each project is under heavy development and consists of many subfeatures. The structure of how these features are assembled affects performance and extensibility. GeoWave’s design is more modular which facilitates customization. GeoMesa’s design makes it simpler and easier to get started with ...

### GeoWave

One of Geowave’s defining distinctions is the ability to index in N dimensions and ability to work with any index and storage configuration. This is achieved through an elaborate set of abstractions that provides a lot of utility via its generalizability, it can make the API cumbersome. While a primary way to access GeoWave is through geotools, this generalizability enables access through other means, for instance, Mapnik.

GeoWave has been in the open source ecosystem for less time than GeoMesa, and is a less mature open source project. Examples are a smaller user base and documentation that is not modular. Although thorough in some regards, the components of the documentation build on precedings ones which make it difficult to start in the middle and discover new features.

### GeoMesa

One of GeoMesa’s defining features is…. Geomesa is focused on providing big data storage, query, and analytics for geotools. While they do aim to support a variety of backends, it is built around Accumulo and other backends are not as well developed. See Appendix for complete list of features

## Feature Comparision

![Venn Diagram of features](img/venn-diagram.png)

The Venn Diagram above indicates the significant overlap of the core features of GeoWave and GeoMesa and some of the distinguishing features.

__secondary indexing__
__subsetting__
__index__
__index configuration, periodicity, sharding__

## Performance Tests

### Methods

In this section, we briefly describe the technical means by which we were able to test the relative performance of GeoWave and GeoMesa.

#### Environment

##### Development

A minimal working environment for either GeoWave or GeoMesa (assuming, as we do, an Accumulo backend) includes a number of interdependent, distributed processes through which consistent and predictable behavior is attained. Each of these pieces - i.e. Apache Zookeeper, HDFS, Accumulo - is a complex bit of technology in and of itself. Their interoperation multiplies this complexity and introduces the race conditions one expects of distributed systems. This extreme complexity manifests itself in longer cycles of iteration and more time spent setting up experiments than actually conducting them.

The solution we adopted, after exploring the already existing approaches, was to develop a set of Docker containers which jointly provide the pieces necessary to bring up GeoWave and/or GeoMesa on top of Accumulo. An ideal solution would be wired up in such a way as to allow a fully functional locally running cluster which could be brought up with as few commands as possible. Recent advances in managing virtual networks with Docker made it possible to build this system in such a way as to spin up an arbitrarily large (system resources permitting) cluster with a single YAML file defining the cluster’s component processes and a single call to docker-compose.

##### Deployment & System Topology

For all deployments, the following versions were used: Hadoop 2.7.2, Spark 2.0.0, Zookeeper 3.4.8, Accumulo 1.7.2, GeoMesa 1.2.6, and GeoWave built from source at GeoWave commit sha 8760ce2cbc5c8a65c4415de62210303c3c1a9710.

Though equivalence of development and deployment environments is ideal, it is often difficult to achieve in practice. While a local cluster with GeoDocker can currently be brought online with a single command, we opted to use the YARN, Zookeeper, and HDFS which is distributed on Amazon EMR to support GeoDocker’s Accumulo processes. This was done primarily out of expedience: both projects currently document an EMR-based deployment and the administration of these EMR managed technologies is complex enough to warrant some slippage between development and deployment if the differences mitigate the complexity of running long-lived clusters.

Pictured below is a rough diagram of the deployment most consistently used throughout our efforts. The entire infrastructure for actually running queries and collecting timings runs on AWS and orchestrated through a combination of makefiles (a simple tool for organizing common tasks) and Hashicorp’s Terraform which provides the means for specifying, in an idempotent fashion, a set of AWS resources. The machines and the software running on top of them were not especially tuned for performance. Instead, we opted to use default settings to see how each system operates under plausible (though likely non-optimal) circumstances.

![Test environment architecture](img/test-environment-architecture.png)

To bring this system online, requests are sent over the AWS command line client to bring up EMR clusters of whatever size is required (typically 3 or 5 workers) for each of GeoMesa and GeoWave. Once the bare EMR cluster is online, Docker images for GeoServer, Accumulo and the GeoWave or GeoMesa iterators are pulled down and run. Upon loading both GeoWave and GeoMesa their generated cluster IDs are used to register them with a cluster of ECS query servers (each of which is identical and stateless to allow for simple scaling). These ECS-backed query servers all sit behind an AWS load balancer to ensure adequate throughput so that the likelihood of testing artifacts due to network problems is reduced.

Once the system is online with data ingested (see the next section for details on our handling of this task), there’s still the question of persisting the results of queries. To simplify and centralize the process, we opted to use an AWS DynamoDB table which the query servers write timings and other important data to at the end of their response cycles. By keeping all timings in Amazon’s cloud, results are further insulated from network-related artifacts.

#### Ingest

All data used for benchmarking these systems was loaded through custom Spark-based ingest programs [see some appendix on the repo]. Initial attempts to use the command line tools provided by each of the projects were met with a few notable difficulties which made writing our own ingest programs the simplest solution:

- Though both tools are relatively well documented, the large number of arguments necessary for even the simplest interaction can be intimidating for new users. The unwieldy nature of both tools is likely fallout from the high degree of complexity in the underlying systems rather than obvious inadequacy in the design of either project.
- Early experiments with GeoWave’s command line tooling revealed that its out of the box Map-Reduce ingest support was plagued by Hadoop classpath issues. But due to the size and scope of the data being used, local ingests were deemed insufficiently performant.
- Because the systems we’re comparing for usability and performance are so complex, equivalent (to the extent that this is possible) schemas (which are encoded GeoTools SimpleFeatureTypes for our purposes) are desirable. Building simple features and their types explicitly within the body of a program proved to be relatively simple to reason about.
- This report does not aim to compare the performance of the ingest tooling for these projects. Any disparities in terms of ingest performance are immaterial.

A survey of the ingest tooling produced is useful for a detailed comparison of the use of parallel features between the two projects. Ingests can be carried out by using one of the many

#### Querying

Queries are generated and submitted by the Query Servers in response to requests from clients. This arrangement was chosen because it allows for quick experimentation and prototyping of different parameters simply by tweaking requests while also ensuring that results are as reproducible as possible (the query server endpoints are written so as to ensure endpoint immutability). The upshot of this is that results generated for this report should be conveniently reproducible and that decisions about which results should be generated, in what order, and how many times are largely in the client’s hands.

For the "Serial Queries", the specific queries were run on at a time, so that the only load on the GeoWave or GeoMesa system was a single query. For the "Multitenancy Stress Tests", a framework was used to produce a number of concurrent connections, so that we could test the multitenancy use case by querying the systems in parallel.

### Datasets

We performed performance tests on three different data sets, which are described below.

#### GeoLife

This GPS trajectory dataset was collected in (Microsoft Research Asia) Geolife project by 182 users in a period of over five years (from April 2007 to August 2012). A GPS trajectory of this dataset is represented by a sequence of time-stamped points, each of which contains the information of latitude, longitude and altitude. This dataset contains 17,621 trajectories with a total distance of 1,292,951kilometers and a total duration of 50,176 hours. These trajectories were recorded by different GPS loggers and GPS- phones, and have a variety of sampling rates. 91.5 percent of the trajectories are logged in a dense representation, e.g. every 1~5 seconds or every 5~10 meters per point. Although this dataset is wildly distributed in over 30 cities of China and even in some cities located in the USA and Europe, the majority of the data was created in Beijing, China.

Text taken from the GeoLife user guide, found at https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/User20Guide-1.2.pdf

##### GDELT

GDELT—Global Data on Events, Location and Tone—is a new CAMEO-coded data
set containing more than 200-million geolocated events with global coverage for 1979 to
the present. The data are based on news reports from a variety of international news
sources coded using the Tabari system for events and additional software for location
and tone. The data is freely available and is updated daily. The GDELT data we have tested against
contains data up through August 2016.

Text taken from the ISA 2013 paper introducing GDELT, found at http://data.gdeltproject.org/documentation/ISA.2013.GDELT.pdf

##### Synthesized Tracks

We tested against a dataset supplied by a third party that that contain a total of 6.34 million synthesized tracks.
This set of tracks has a median length of 29.8 km, a mean length of 38.82 km and each track contains an average of 491.45 points.
There is approx. 35.88 GB of data compressed and stored as around 730 avro encoded files.
The tracks are generated through a statistical process using Global Open Street Map data and Global Landscan data as inputs.
The dataset is available at `s3://geotrellis-sample-datasets/generated-tracks/`

Here is a view of the data for a specific time slice of the data, as shown in GeoServer:

![Synthetic Tracks SIZE::60](img/tracks/synthetic-tracks.png)

##### Track Length Stats (in miles)

|     count         |  min | max | mean | std dev | median | skewness | kurtosis |
|:-----------------:|:-----------------:|:-----------------:|:-----------------:|:-----------------:|:-----------------:|:-----------------:|:-----------------:|
| 2054751           |0.064998          |2839.198486       |38.829134         |115.975988        |29.791367         |15.466978         |266.782216        |

## Performance Test Conclusions

After performing a wide variety of tests against

## Conclusions

- Both systems perform well
- GeoMesa has issues with multitenancy.
- The projects are umbrellas.

## Recommendations

- Create a third project that has a collaboration point
- Other end of the spectrum...gm kafka…
- Ingest tooling around. Gw interested in gm’s.
- Common simplefeature avro schema
- Sf curve.
- Projects are umbrellas…

# Appendix A: Details of GeoMesa and GeoWave feautres

## Comparision of Attribute/Secondary Indices

Often, spatial coordinates aren't the only important condition used in searching for and filtering through a dataset. Paramedics might want to find only accidents within their geographic region but they also might only want those accidents whose 'severity' attribute is 'fatal'. For certain applications it is a matter of practical necessity that such fields are indexed for quick lookup later and both GeoMesa and GeoWave provide some tools for these purposes. It is worth mentioning that the documentation provided by both projects suggests that secondary/attribute indices are an area that will receive future focus by their respective teams. In what follows, we briefly compare the features provided by each.

### GeoMesa Attribute Indices
In GeoMesa, any attribute can be indexed with a simple modification to the `UserData` which is associated with a `SimpleFeatureType`'s attribute. Each attribute index is stored in a single, associated `attr_idx` table. By fiat, let's imagine we have a `SimpleFeatureType` which describes car accidents as described above. The following code will add the appropriate information to our type so that, upon ingest, indexes are created to the values in our 'severity' field:
```scala
val sft: SimpleFeatureType = ??? // Our feature's schema
sft.getDescriptor("severity").getUserData().put("index", "join");
sft.getDescriptor("severity").getUserData().put("cardinality", "high");
```
As seen above, two properties on this attribute index are exposed through the `UserData`: 'index' (the type of index operation) and 'cardinality' (the number of distinct values).


#### Full/Join Indices
The type of index - 'full' or 'join' - determines just how much data is replicated in the lookup table of the attribute index. Full indices store the entire `SimpleFeature` of a record, allowing for quick replies to indexed-attribute queries without joining against the records table. This is preferable under circumstances in which the attribute in question is regularly queried against and especially if the expected queries don't necessarily rely upon other fields for filtration. The 'join' index stores only the data necessary for identifying the values in the records table which satisfy the provided predicate and is therefore useful for preserving storage resources.

#### Low/High Index Cardinality
The utility of this distinction is somewhat unclear. A high cardinality index has enough values that we can expect any filtering it does to significantly slim down the number of returned records (thus, a query against a high cardinality index is given priority) while a low cardinality index seems to be ignored. The user documentation under ['Data Management'](http://www.geomesa.org/documentation/user/data_management.html) notes (as of 10/01/2016) that "technically you may also specify attributes as low-cardinality - but in that case it is better to just not index the attribute at all."

### Client Code Difficulties
As of 1.2.6, it appears as though a library which is shaded in GeoMesa client code needs to be appropriately shaded in any ingest client code which intends to take advantage of attribute indices. The fix for this issue can be found in [a commit](https://github.com/locationtech/geomesa/commit/2335a8856cc9b2388532209b4a6e61f925f2dd20) which made its way into 1.2.6.


### GeoWave Secondary Indices
Unlike GeoMesa, each secondary index gets its own table. Again, unlike GeoMesa, setting these secondary indices up is *not* a simple, two-line affair. Figuring out how to actually use these secondary indexes was not obvious or straightforward from the documentation.

Here we modify the same `SimpleFeatureType` for extra indexing on ingest as above:
```scala
val sft: SimpleFeatureType = ???
val secondaryIndexingConfigs = mutable.ArrayBuffer[SimpleFeatureUserDataConfiguration]()
val textFieldsToIndex = Set("severity")

secondaryIndexingConfigs += new TextSecondaryIndexConfiguration(textFieldsToIndex.asJava)
val config = new SimpleFeatureUserDataConfigurationSet(sft, secondaryIndexingConfigs.asJava)
config.updateType(sft)
```

#### Index Cardinality
Unlike GeoMesa, cardinality of indices isn't a static feature configured by the user. GeoWave's query planning and optimization attempts to determine the usefulness of an index for a given query based on the statistics it gathers on ingest.

#### Specialized Index Types
Another Point of divergence between these projects in terms of extra index support is GeoWave's intent to support specialized indices which can take advantage of various assumptions which are domain specific. Exact-match (as opposed to fuzzy) indices for text are not the same as exact indices for numbers or dates or even fuzzy indexing (through n-grams) of that same text. The specialization here makes it possible for GeoWave to index in ways that are sensitive to the types of data in question and even to the expectations of use (i.e. fuzzy vs exact and range-based vs exact queries).

#### Future Development
Documentation for GeoWave mentions the possibility of adding n-gram based fuzzy indexing of text fields (so that searches based on a subset of the data in a field can be used). It appears as though this feature is already in the works, as an n-gram table is currently generated on ingest in the development branch of GeoWave.


# Appendix B: Machine Specs
Across all tests, though the machine counts differed (cluster-size is mentioned where appropriate), the types of machines used were constant and role dependent:

|                | Instance Type | vCPU | Mem (GB) | Storage (Drives x GB) |
|----------------|---------------|------|----------|-----------------------|
| Query Server   | m3.large      | 2    | 7.5      | 1x32                  |
| Cluster Master | m3.xlarge     | 4    | 15       | 2x40                  |
| Cluster Worker | m3.2xlarge    | 8    | 30       | 2x80                  |

# Appendix C: Ingest tooling

# Appendix D: Ingest notes

## Geolife

Based on ingests into a cluster with 5 m3.2xlarge workers.

_Note: The performance test were performed against a 3 node cluster with a similar setup._

#### GeoMesa

- Disk Used:      1.68G
- Total Entries: 71.59M

###### Tables

| Tables                                | Number of Entries |
| ------------------------------------- |:-----------------:|
| `geomesa.geolife`                     |        10         |
| `geomesa.geolife_gmtrajectory_z3`     |    24.60 M        |
| `geomesa.geolife_records`             |    24.35 M        |
| `geomesa.geolife_stats`               |     8.00 K        |
| `geomesa.geolife_z2`                  |    24.55 M        |

###### Entries per tablet server

`11.95M, 11.67M, 11.67M, 11.95M, 24.35M`

###### HDFS usage report

DFS Used: 34.85 GB (4.84%)

#### GeoWave

- Disk Used: 1.45G
- Total Entries: 47.24M

###### Tables

| Tables                                                         | Number of Entries |
| -------------------------------------                          |:-----------------:|
| `geowave.geolife_SPATIAL_TEMPORAL_IDX_BALANCED_YEAR_POINTONLY` |      23.44M       |
| `geowave.geolife_GEOWAVE_METADATA`                             |        30         |
| `geowave.geolife_SPATIAL_IDX`                                  |      23.82 M      |

###### Entries per tablet server

The entires per tablet server server show that all entires are on one of the 5 workers,
which will dramatically affect performance. In order to correct that,
we change the split size and compact the table.

To get more splits, we execute the following command:

```
config -t geowave.geolife_SPATIAL_IDX -s table.split.threshold=100M
compact -t geowave.geolife_SPATIAL_IDX
config -t geowave.geolife_SPATIAL_TEMPORAL_IDX_BALANCED_YEAR_POINTONLY -s table.split.threshold=100M
compact -t geowave.geolife_SPATIAL_TEMPORAL_IDX_BALANCED_YEAR_POINTONLY
```

This gave the following entries per table:

`14.57M,8.81M,8.70M,2.92M,11.67M`


###### HDFS usage report

- DFS Used: 12.5 GB (1.74%)


## GDELT

Based on ingests into a cluster with 5 m3.2xlarge workers.
These stats were taken after ingest completed and compaction was done to all tables containing many entries.

#### GeoMesa

- Disk Used:       98.75G
- Total Entries:    1.22B

###### Tables

| Tables                                | Number of Entries |
| ------------------------------------- |:-----------------:|
| `geomesa.gdelt`                       |        10         |
| `geomesa.gdelt_gdelt_2devent_z3 `     |    406.51M        |
| `geomesa.gdelt_records`               |    406.51M        |
| `geomesa.gdelt_stats`                 |      7.88K        |
| `geomesa.gdelt_z2`                    |    406.51M        |

###### Tablet servers

| Tablet Count  | Number of Entries |
| ------------- |:-----------------:|
|      47       |    242.86M        |
|      44       |    234.28M        |
|      48       |    237.68M        |
|      46       |    241.10M        |
|      46       |    263.62M        |


###### HDFS usage report

DFS Used: 202.61 GB (28.16%)

#### GeoWave

We had problems ingesting GDELT, where the `geowave.gdelt_GEOWAVE_METADATA` table had way too many entries, all stored to memory,
and never flushing to disk although there was one minor compaction running the whole time. Any query or compact command
to that table would hang and timeout. We got around this issue by not saving any statistics to the table, by using the
`AccumuloOptions.setPersistDataStatistics(false)` method for our datastore options. An attempt was made to use the
`recalcstats` command in the geowave geotools, however we were unable to get this to work.

- Disk Used: 73.81
- Total Entries: 813.19

###### Tables

| Tables                                                              | Number of Entries |
| -------------------------------------                               |:-----------------:|
| `geowave.gdelt_SPATIAL_TEMPORAL_IDX_BALANCED_WEEK_HASH_4_POINTONLY` |      406.60M      |
| `geowave.gdelt_GEOWAVE_METADATA`                                    |           4       |
| `geowave.gdelt_SPATIAL_IDX_HASH_4`                                  |      406.60M      |

###### Entries per tablet server

| Tablet Count  | Number of Entries |
| ------------- |:-----------------:|
|      28       |    166.40M        |
|      26       |    151.95M        |
|      27       |    158.78M        |
|      29       |    170.14M        |
|      29       |    165.92M        |

###### HDFS usage report

- 156.6 GB (21.76%)

## Tracks

Based on ingests into a cluster with 5 m3.2xlarge workers.
These stats were pulled from a cluster that had undergone extensive performance testing.

#### GeoMesa

- Disk Used:       58.12G
- Total Entries:   19.59M

###### Tables

| Tables                                | Number of Entries |
|-------------------------------------- |:-----------------:|
| `geomesa.tracks`                      |        10         |
| `geomesa.tracks_records`              |      6.41M        |
| `geomesa.tracks_stats`                |        68         |
| `geomesa.tracks_xz2`                  |      6.33M        |
| `geomesa.tracks_xz3 `                 |      6.57M        |

###### Tablet servers

| Tablet Count  | Number of Entries |
| ------------- |:-----------------:|
|      45       |      4.22M        |
|      43       |      3.79M        |
|      45       |      3.66M        |
|      47       |      3.80M        |
|      44       |      4.13M        |


###### HDFS usage report

DFS Used: 120.6 GB (16.76%)

#### GeoWave

There is more entries here, which can be explained by the fact that GeoWave can store up to 3 duplicates per entry based on their indexing scheme.

- Disk Used: 106.24G
- Total Entries: 35.83M

###### Tables

| Tables                                                              | Number of Entries |
| ------------------------------------------------------------------  |:-----------------:|
| `geowave.tracks_SPATIAL_TEMPORAL_IDX_BALANCED_YEAR`                 |      18.04M       |
| `geowave.tracks_GEOWAVE_METADATA`                                   |          38       |
| `geowave.gdelt_SPATIAL_IDX_HASH_4`                                  |      17.78M       |

###### Entries per tablet server

| Tablet Count  | Number of Entries |
| ------------- |:-----------------:|
|      37       |      7.37M        |
|      41       |      6.59M        |
|      39       |      5.75M        |
|      37       |      6.26M        |
|      37       |      9.85M        |

###### HDFS usage report

- 218.93 GB (30.43%)

# Appendix E: Details of Serial Queries and Results

The following queries and results were executed serially, so that only one query was ever executing at a time on either the GeoWave or GeoMesa system.

This is not a complete list of the queries; that can be found in the source code for the service endpoints.
We will consider and analyze only a subset that we found interesting.

> Disclaimer
>
> These are simple a few ways of looking at the data that we found useful,
> after looking at the data in many different ways. We don't claim that these
> results are definitive, or that they tell the whole story. One reason for
> putting a repeatable process for others to perform these tests and analyze
> results (besides transparency, and the general spirit of FLOSS, and the
> techniques being more generally useful) is so that _you_ could perform the
> tests and dissect the results however _you_ think is best. What follows is
> a window into how we have dissected the results, and the conclusions we have
> drawn from those.

###### Notes
- In the following results, if we specify that outliers were removed, they were removed via the interquartile range rule.
- All maps were generated with `geojson.io`, basemaps © OpenStreetMap contributors

Throughout all results, unless otherwise noted, we will be following this general color scheme:

![Legend SIZE::20](img/legend.png)

### GeoLife

Test for GeoLife were performed on EMR 5.0.0 clusters of one m3.2xlarge master and three m3.2xlarge workers. The GeoLife dataset was ingested with default 2D and 3D indexes for both systems. See the appendix for details about machine specs and ingest results.

###### Spatial queries of Beijing

We used the Beijing geojson from Mapzen's borders dataset, which can be found in the resources of the `core` subproject. This represents the multipolygon seen below

![Beijing polygon SIZE::60](img/beijing-poly.png)

We then queried the city of Beijing over the whole time of the dataset. We tracked results for both iterating over the resulting SimpleFeatures.
Here are the timing results for that test, with outliers removed:

![GEOLIFE-IN-BEIJING-ITERATE](img/geolife-beijing-iterate.png)

These queries take a long time; this makes sense, as they are iterating over __19,902,865 results__.

###### Spatial queries of central Beijing

To test on queries with smaller result set, we using `geojson.io` to draw a rough polygon around the center of Beijing, we then performed spatial-only queries using this polygon:

![Beijing Center SIZE::60](img/beijing-center.png)

This allowed us to track the iteration and count queries against a smaller spatial extent.
However, this query did not actually cut out too many results; the result set for this query included __16,624,351 results__.
In the following chart, outliers have been removed.

![GEOLIFE-IN-BEIJING-CENTER-ITERATE](img/geolife-beijing-center-iterate.png)

These two results show GeoMesa handling queries with large results faster than GeoWave, which is a result we've seen fairly consistently in our tests.

###### Spatial queries of bounding boxes across Beijing

This query cuts the bounding box of Beijing into `N` equal sized bounding boxes, represented by the tile coordinate `COL` and `ROW`.

For instance, running `N=32` would create bounding boxes that look like this:

![Bounding Boxes SIZE::60](img/beijing-bbox-32.png)

We tested with `N=32`  `{ 2, 4, 8, 16, 32}`. This produced 1,024 unique queries.
417 were queries with 0 results, and were not considered.
103 of these queries did not produce the same result between GeoMesa and GeoWave;
a query for the entire bounding box of Beijing produces the same results, so it is unclear
why this mismatch occurs, and which system is incorrect.
Because these tests are focused on performance and not accuracy, these mismatched results are included
in the graphs below.

This graph plots the result count of each query against the duration of that query per system:

![GEOLIFE-BEIJING-BBOXES-ITERATE scatter](img/geolife-bbox-scatter.png)

This shows GeoMesa having more variance over duration; however it does not give a good indication of trends.
If we plot a linear regression on the two set of points, we can see that although GeoMesa appears to have
more variance in query duration, the queries typically return faster from GeoMesa than from GeoWave, and this
trend becomes more pronounced as the number of results increases.

![GEOLIFE-BEIJING-BBOXES-ITERATE scatter with regression](img/geolife-bbox-scatter-with-regression.png)

GeoMesa has a feature called "loose bbox" that allows you to trade performance for result accuracy;
it only uses the space filling curve to filter data and does no secondary filtering, so false positives
could be returned. The graph below includes a scatterplot and regression for the loose bounding box queries in yellow.

![GEOLIFE-BEIJING-BBOXES-ITERATE scatter with regression](img/geolife-bbox-regression-with-loose.png)

The following graph shows the regressions against queries returning less than 10,000 results.
It shows that even for queries with lower result counts, GeoMesa tends to slightly outperform GeoWave
for these spatial-only point data queries, for both loose and exact queries.

![GEOLIFE-BEIJING-BBOXES-ITERATE scatter with regression](img/geolife-bbox-regression-with-loose-less-than-10000.png)

###### Spatiotemporal query results

In both systems, spatiotemporal queries (those with bounds in both space and time) hit a different table and indexing mechanism than the spatial-only queries described above.
To include a temporal aspect to our queries, we ran a query over the center of Beijing for the month of August in 2011. This returned __84,496 results__.
In the following chart, outliers have been removed.

![GEOLIFE-IN-CENTER-BEIJING-JAN-2011-ITERATE](img/geolife-beijing-center-aug-2011.png)

We see that GeoMesa performs better in this query. If we plot the histogram of GeoMesa durations with outliers removed, for both
exact and loose (red and yellow, respectively), and compare it to the histogram of durations for GeoWave queries with outliers removed,
we see that there is a wider spread of timing results coming from GeoWave for this query.

![GEOLIFE-IN-CENTER-BEIJING-BBOX-FEB-2011-ITERATE (GM with loose)](img/geolife-beijing-center-feb-2011-bbox-gm.png)
![GEOLIFE-IN-CENTER-BEIJING-BBOX-FEB-2011-ITERATE (GW)](img/geolife-beijing-center-feb-2011-bbox-gw.png)

### GDELT

Test for GDELT were performed on EMR 5.0.0 clusters of one m3.2xlarge master and five m3.2xlarge workers.

##### Spatiotemporal queries of areas around cities: City Buffers

For these queries, which we call the "city buffer" tests, queries are taken from center points corresponding to the following cities:
Paris, Philadelphia, Istanbul, Baghdad, Tehran, Beijing, Tokyo, Oslo, Khartoum, and Johannesburg. For instance, the Paris city buffers
look like this:

![City Buffers SIZE::60](img/gdelt/paris-city-buffers.png)

The queries were taken over a set of these durations: six months, two months, two weeks, and six days.

Below is a scatter plot of duration by query result count.

![gdelt-result-over-duration](img/gdelt/gdelt-result-over-duration.png)

We can see that GeoMesa has much less consistent results than GeoWave. If we plot a linear regression on
this point sets, we'll get the following:

![gdelt-result-over-duration-regression](img/gdelt/gdelt-result-over-duration-regression.png)

GeoMesa tends to be slower at returning these queries than GeoWave, until the queries return
a large number of results. According to the regression, after around 1 million results returned,
GeoMesa becomes faster than GeoWave. This is an imprecise result, but one that we have found
consistent over point datasets: GeoMesa generally does better with queries that produce
large result sets.

This next graph shows the mean duration of queries over all cities and all buffer sizes, for 14 day queries,
based on the result count of the queries. The x axis in this case represents a bin of result counts;
points were averaged according to a quantile-based discretization function of result count, which is represented here on the X axis as
the lowest result count of that grouping.

![Durations over result counts, default indexes, all queries of 14 days](img/gdelt/014-days-default.png)

We see here that again GeoMesa appears to be slower than GeoWave, until a certain result count is reached,
after which it performs better.

If we take a look at the next graph, another pattern emerges. This graph moves the temporal bounds of
the query to 168 days.

![Durations over result counts, default indexes, all queries of 168 days](img/gdelt/168-days-default.png)

We see GeoMesa performing worse than the 14 day case. In this case it
actually never crosses the threshold where it starts outperforming GeoWave based on result count (according
to this technique of averaging result count).

###### Index configurations

We hypothesized that some of the timing differences we were seeing here was because of differences in the configuration
of their indexing mechanisms. As described in the section comparing the index configurations, the default periodicity
for GeoMesa is one week, while in GeoWave it is one year. Also, GeoWave does not shard it's data by default.
To find out how this configuration might be affecting the timing results, we tested with both systems set
to having the following configuration:

- Both systems configured to have a periodicity of one month, with default sharding
- Both systems configured to have a periodicity of one year, with default sharding
- Both systems configured to have a periodicity of one year, with 4 shards being generated by a hashing algorithm

The last configuration produced improvements in timing results for the City Buffer queries, which we will explore below,
and be referred to as the "matched" index configuration.

This graph shows the durations of 14 day queries averages broken into the same result count quartiles.
We can see a marked improvement in GeoMesas results.

![Durations over result counts, matching indexes, all queries of 14 days](img/gdelt/014-days-matching.png)

In the case of the 168 day queries, we see that although there is still a degradation of performance for GeoMesa,
it is not nearly as prominent as it was with the default indexing.

![Durations over result counts, matching indexes, all queries of 168 days](img/gdelt/168-days-matching.png)

When we look at the overall timings based on result count, we can that GeoMesa seems to be slightly
outperformed by GeoWave until a certain result set size is reached.

![Durations over result counts, matching, all queries](img/gdelt/overall-duration-vs-result-matching.png)

If we focus in on query durations with relatively lower result counts, and plot GeoMesa and GeoWave results
with both the default and the matched index configuration, we see that both systems improve with the matched
index configuration, and that GeoWave outperforms GeoMesa in this case for both index configurations.

<!-- ![Durations over result counts, matching, all queries, clamp 20](img/gdelt/overall-duration-vs-result-matching-cap-20.png) -->

![Durations over result counts, matching, all queries, clamp 20](img/gdelt/overall-duration-vs-result-both-cap-20.png)

If we only look at queries with time bounds that are less than 30 days, we see the GeoMesa matched index configuration
performing the best out of that group.

![Durations over result counts, both, less than 30 days, clamp 20](img/gdelt/lt-30days-duration-vs-result-both-cap-20.png)

When we only consider queries over 30 days, we see a more marked advantage of GeoWave performance.

![Durations over result counts, both, greater than 30 days, clamp 20](img/gdelt/gt-30days-duration-vs-result-both-cap-20.png)

<!-- ![Durations over result counts, both, less than 30 days](img/gdelt/lt-30days-duration-vs-result-both.png) -->
<!-- ![Durations over result counts, both, greater than 30 days](img/gdelt/gt-30days-duration-vs-result-both.png) -->

Here is the duration of queries plotted over the temporal bounds of the query. We see that GeoMesa performs better for
queries with a small time window, and both GeoMesa and GeoWave show better performance with the matched index configuration.

![Durations over days, both](img/gdelt/duration-over-days-default-and-matching.png)

Looking at the data in another way, we see that the size of the spatial component of the query
(shown in the x axis here in kilometers) does not have the same threshold-crossing effect as the temporal
component does, and that on average GeoWave outperforms GeoMesa across spatial queries.

![Durations over size, both](img/gdelt/duration-over-size-default-and-matching.png)

Finally for the City Buffer tests, we look at how the scatterplot and regression
of durations over result counts changes between the default and the matched index configuration.

![Durations over result, scatter and regression, both](img/gdelt/duration-over-result-default-and-matching.png)

###### South America

We queried events within South america countries for three weeks of every month of every year from 2000 to 2016.

![South America countries SIZE::60](img/gdelt/south-america-countries.png)

The results we found for this area of the world were unsurprising, with the matching index configuration
performing better in both systems than the default index configuration, and the effect of result count
having a greater effect on GeoWave performance than GeoMesa. The following chart is of the duration of
query execution over result size, with outliers removed.

![Durations over result counts, both, all queries](img/gdelt/sa-overall-duration-vs-result-both.png)

### Synthesized Tracks

Test for Tracks data were performed on EMR 5.0.0 clusters of one m3.2xlarge master and three m3.2xlarge workers.

We performed queries against a range of bounding boxes over the continental United States of America.
We project a powers of 2 pyramid over this area and query from pyramid level 4 to 7, with temporal bounds being one of
5 days, 18 days, 27 days, or one month. The beginning of the temporal bounds was selected from the range of time for which
data existed.

We refer to the spatial aspect of the bounding box queries according to "levels", where each level refers to a powers of 2 pyramid
over the bounding box of the US. Below is a depiction of those bounding boxes, to give a sense of scale.

##### Level 4
![Tracks grids level 4](img/tracks-usa-grid-4.png)
##### Level 5
![Tracks grids level 5](img/tracks-usa-grid-5.png)
##### Level 6
![Tracks grids level 6](img/tracks-usa-grid-6.png)
##### Level 7
![Tracks grids level 7](img/tracks-usa-grid-7.png)

There is an pattern of behavior exhibited by the query results for the generated tracks dataset that bears some mention.  Consider the following graphs:

![GM_duration_v_result_matched_1month.png](img/tracks/GM_duration_v_result_matched_1month.png)
![GW_duration_v_result_matched_1month.png](img/tracks/GW_duration_v_result_matched_1month.png)

There is an indication of a gentle upward trend in both the case of GeoWave and GeoMesa; however, the GeoWave results exhibit an additional tendency for the returns to stratify.  This behavior is possibly an artifact of GeoWave's tiering strategy and would only appear in datasets with elements that exhibit a range of geometric extents, which is the case for this dataset.

Below is a chart that represents the distribution of durations for each system, as well as marking the mean duration.
It shows GeoMesa performing better on this dataset, which is consistent with our analysis.

![GM_GW_tracks_duration_matched_1month.png](img/tracks/GM_GW_tracks_duration_matched_1month.png)

If we look at the mean durations over result count groupings according to a quantile-based discretization function of result count,
we see GeoMesa consistently outperforming GeoWave.

![Durations by result, by level](img/tracks/duration-by-result-count.png)

To understand if there is a relationship between the temporal bounds of the query and performance, we
can look at the above chart broken down by the 5 day, 18 day and 27 day queries. For GeoMesa, we
do not see a clear behavior.

![Durations by result, by level](img/tracks/geomesa-duration-by-result-count-and-days.png)

But for GeoWave, the performance seems to be correlated with the size of the temporal query bounds.

![Durations by result, by level](img/tracks/geowave-duration-by-result-count-and-days.png)

# Appendix F: Details of Multitenancy Stress Tests

In multitenancy tests we are interested in examining the behavior of the systems under heavy and dynamic query load.
This simulates a common deployment in which the Accumulo cluster is a shared resource backing a group of application servers.
The benchmark service acts as our application server, receiving the full query result-set and producing a digested view for the client.
We’ve formulate this test to make the Accumulo cluster and its SimpleFeature index the only contested resource.

## Summary

### GDELT

Under heavy dynamic query load against a GDELT store, GeoWave is better able to cope with high concurrent load producing stable and predictable performance.
In our specific scenario it is able to process a 3.5x higher request volume than GeoMesa.
We also found that GeoWave's return times increase roughly 1.5 times faster than GeoMesa as the size of the result set increases.
On the other hand, GeoMesa's return times increase roughly 8.5 times faster than GeoWave as the number of concurrent queries rises.
Additionally during this testing we have witnessed two instances where GeoMesa clusters entered degraded performance mode, where query duration seems to
increase permanently after a round of load testing.
We have not attempted to diagnose this issue but its effects are illustrated in our results.

#### Failure Analysis

The performance gap is further compounded because GeoWave is able to consistently complete queries covering large temporal spans (12 to 14 months) before the 60s client timeout.
When this does not happen the client connection times out and a new query is issued, which then must compete for resources with still executing scans.
Higher client timeouts cause the GeoMesa cluster to work under higher concurrent random scan load than GeoWave.
Highly concurrent random access scans cause tablet contention which is known to dramatically impact Accumulo performance.
This timeout cascade can be mitigated in deployment by tuning the allowed query load, cluster size, or canceling the scans when the client timeout happens.
Our benchmark services do not implement cancel-able queries so we are not able to comment on the difficulty of implementing such a feature.

### Generated Tracks

Generated tracks load tests allows us to test the performance of GeoMesa XZ3 vs GeoWave tired Hilbert index.
Under heavy load with variance both over spatial and temporal selectivity both GeoMesa and GeoWave produced stable and reliable performance.
GeoWave delivers 60% higher request completion volume vs GeoMesa with 95th percentile response time being 7.5s.

## Test Setup

All queries are triggered by [Gatling load-testing tool](http://gatling.io) issuing HTTP requests against a load-balanced group of application servers.
Gatling allows us to keep the number of active concurrent query connections constant.
A connection is considered active until the benchmark services iterates over the query results set or a time-out of 60 seconds.

As with other tests the benchmark service saves the query timings, which happens even in the event of a time-out on the client/benchmark-server connection.
However, unlike the earlier test-ups all the queries per scenario are done against either GeoMesa or GeoWave - not both.
This produces a reliable and uninterrupted load on the target system.

## GDELT Test

The queries are executed against GDELT ingested into a cluster with 5 workers as in previous tests.
We reuse the `in-city-buffers-*` and `in-south-america-countries-three-weeks` query groups, randomly selecting parameters for spatial and temporal bounds.
Up to eight concurrent connections for each query group will be open with no pause between subsequent requests.
This is a very dynamic load test as `in-city-buffers-*` queries span up fourteen months and the `in-south-america-countries` queries will issue twelve `DataStore` queries for every HTTP request.

Because the Accumulo-backed `DataStore` can receive records from multiple tablet servers the application server network interface can easily become saturated.
These simulations increase the number of application servers relative to other tests to remove this bottle-neck and increase the query throughput until the cluster under test becomes the limiting resource.
Because small client queries trigger large query workloads we do not consider the connection between application server and Gatling client a limited resource.

### Test Results

We are interested in how concurrency impacts the duration of query execution.
The number of concurrent queries at the time of submission is calculated based on query start and end times as recorded by the application service.
Before each round of testing we verified that all clusters were idle, not undergoing any compactions or scans.


### MT1: 1 Application Server
![mt1_duration_vs_concurrency] (img/multitenancy/graph_uncut_mt1.png)

This configuration is expected to degenerate; a single application server does not have the network bandwidth to pull the requested records and we see query duration spike well past usual levels.
From the client perspective this scenario produced cumulative timeout rates of about 50% for both systems.
After this round of testing GeoMesa cluster experienced an unexpected severity of degradation in query performance. This prompted us to bring up two replacements for future tests.


### MT2: 4 Application Servers

![mt2_duration_vs_concurrency] (img/multitenancy/graph_100k_mt2.png)

With four application servers we expect the network to no longer constrain the query execution. Notably we see that GeoMesa is working against higher concurrency and producing higher delays.
These effects are likely mutually-reinforcing.
From the client we saw the query timeouts drop to ~5% for GeoWave and remain at ~20% for GeoMesa.
These results contain two GeoMesa clusters and we do not see the effects of performance degradation from the `MT1` test.

### MT3: 6 Application Servers

![mt3_duration_vs_concurrency] (img/multitenancy/graph_100k_mt3.png)

We increase the application server count to six to verify that we have removed any performance impact from this parameter, as we expect the duration and concurrency distributions remain consistent, indicating that in both `MT2` and `MT3` results the cluster resources are the only remaining constraint.
However, at this point one of the two GeoMesa clusters started experiencing similar performance degradation as after `MT1` round of tests. This can be seen as two distinct distributions of GeoMesa results.

## Generated Tracks Test Specifics

This dataset is densest around continental United States and covers a single year, with track length biased to be short.
We project a powers of 2 pyramid over this area and query from pyramid level 4 to 8 with temporal selectivity ranging from 5 days to 1 month.
The generated query requests are biased towards lower levels of the pyramid, proportional to the number of grid cells at each level.
We test initially with 16, 32 concurrent connections.
Because we have seen from previous tests that six application servers is sufficient to handle query load from our cluster we only test against this configuration.

### Test Results

Increasing from 16 to 32 concurrent users produced nearly identical result counts per unit of time, 30 minutes.
However, we see that it has increased latency for each request.

Most interesting are the response time distributions, which explain the difference in overall throughput.
GeoWave index trades minimum response time for more consistent and on average faster results.

### GeoWave 16 Users
![gw-16-response](img/multitenancy/gw-16-responses.png)

### GeoWave 32 Users
![gw-32-response](img/multitenancy/gw-32-responses.png)

### GeoMesa 16 Users
![gm-16-response](img/multitenancy/gm-16-responses.png)

### GeoWave 32 Users
![gm-32-response](img/multitenancy/gm-32-responses.png)


# Appendix G: Details of Performance Test Conclusions

In the course of our performance comparisons, we were able to characterize some scenarios in which the two systems displayed difinably different behavior.
In this appendix, we define those scenarios, and we discuss some possible causes for those differences.
However, before doing either of those things, it is necessary to explain the set of experiments which provide the explanitory framework for this section.

## Query Planning

In this section, we discuss differences in performance that we noticed in experiments on the GDELT data set, which is composed of points.
At an abstract level, the point-storage mechanisms found in the respective systems are esentially equally capable.
(GeoMesa uses a Z-index and GeoWave uses a more sophisticated Hilbert-index, but that difference should not generate much of performance discrepency in-and-of itself.)
Since we did observe some systematic differences in performance, we must look beyond the modest high-level differences to try to find an answer.

Query-planning, the process of converting a query from the user into a collection of row-ranges to be submitted to Accumulo, is the highest-level significant algorithmic difference between the two systems that we have been able to identify (at least for point queries).
GeoWave uses the very sophisticated [uzaygezen](https://github.com/aioaneid/uzaygezen) library to compute its query plans, and GeoMesa uses faster (but less thorough) [sfcurve](https://github.com/locationtech/sfcurve) library.
The net effect is that GeoWave tends spend more time on query planning, but with greater selectivity (fewer false-positives which in the ranges which must later be filtered out).

## Query Planning Experiments

We performed a series of query planning experiments which mirror our GDELT experiments.
In an earlier section, we described at set of experiments in which we performed buffered queries around city centers using the GDELT dataset, here we used the excact same set CQL queries that were generated in the experimenet, but we gather different data.
That was done by isolating the parts of the respective systems responsible for query-planning and putting them into [stand-alone programs](https://github.com/azavea/geowave-geomesa-comparative-analysis/tree/master/query-planning).

We used those stand-alone programs to measure the amount of time spent on query-planning, as well as the results of the planning (the number of ranges generated and the lengths of those ranges).

### By Diameter

Allowing the diameter of the buffer around each city center to scale from 10 kilometers to 650 kilometers, we obtained the following results:

![size_time](img/query-planning/size-time.png)

The first graphs shows the average amount of time (in milliseconds) that each system spent in the query-planning phase for all queries of the given diameter.
Here we observe that GeoWave takes much longer to complete this phase (although it should be noted that these experiments were performed on a differently-configured computer than GDELT experiment was, so the timings are not directly comparable).

![size_ranges](img/query-planning/size-ranges.png)

The next graph shows the number of ranges generated by the two systems.
We see that GeoWave generates many more ranges over the range of diameters.

![size_length](img/query-planning/size-length.png)

The third graph shows the total length of the ranges (difference between the respective starting and ending indices of the ranges) generated by the two systems.
Here we see that GeoMesa's total is much greater, as much as a factor of ten for larger query windows.
This particular number becomes relatively more important in areas of greater data density.
Taken together with the number of ranges (displayed in the second figure), this quantity gives us an idea of the selectivity of the query planner.

### By Time Window

The same data can also be broken-down according to the temporal width of the queries.

![window_times](img/query-planning/window-times.png)

The first graph shows the query-planning time required by the two systems as a function of the temporal width of the queries.
Although the GeoMesa query-planning time is generally greater than that of GeoMesa, in this particular case we see that the two actually coincide at the "6 month" mark.
That fact will be important to us later.

![window_ranges](img/query-planning/window-ranges.png)

The second graph shows the number of ranges generated as a function of temporal widths.
Although GeoMesa normally has a larger number, in this case we see GeoMesa actually produce fewer ranges than GeoMesa at "6 month".
This is mildly surprising (based on a number of other experiments that we have performed) and will once again be important later.

![window_length](img/query-planning/window-length.png)

Finally, the last graph shows the aggregate query-plan length as a function of temporal window width.
As before, GeoWave remains consistently lower in this area.

## Performance Observations vis-a-vis Query Planning

In those experiments, GeoMesa tended to do better than GeoWave as the size of the result sets increased.
Examination of the respective query-planning strategies of the systems shows that GeoMesa submits fewer ranges of keys to Accumulo, but those ranges are wider in length.
These fewer-but-larger ranges could provide an advantage in this context.

Although GeoMesa tends to do better than GeoWave on the queries just described, GeoMesa's relative performance advantage lessens as the temporal widths of the queries increases.
GeoWave tends to produce sets of ranges whose counts goes down as the temporal window widens, whereas GeoMesa produces sets of increasing size.
Simultaneously, the sum of the widths of GeoWave's intervals are smaller than those of GeoWave.

Another pattern that we noticed is that GeoWave tends to do better on heavy load than GeoMesa on the GDELT dataset.
Once again looking to query-planning for an explination, the more-but-shorter ranges produced by GeoWave could provide an answer.
This creater selectivity could provide an advantage in which disk or network bandwidth is the limiting factor.