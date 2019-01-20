[![Build Status](https://travis-ci.com/mcherri/erp-akka-cqrs-es.svg?branch=master)](https://travis-ci.com/mcherri/erp-akka-cqrs-es)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/b6bc4dc137224abdaf7b2d0ba94daed1)](https://www.codacy.com/app/mcherri/erp-akka-cqrs-es?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=mcherri/erp-akka-cqrs-es&amp;utm_campaign=Badge_Grade)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/b6bc4dc137224abdaf7b2d0ba94daed1)](https://www.codacy.com/app/mcherri/erp-akka-cqrs-es?utm_source=github.com&utm_medium=referral&utm_content=mcherri/erp-akka-cqrs-es&utm_campaign=Badge_Coverage)

erp-akka-cqrs-es
================
erp-akka-cqrs-es is a basic ERP implemented using CQRS/ES and Akka. It is inspired by:

-   [akka-ddd](https://github.com/pawelkaczor/akka-ddd)
-   [Fun.CQRS](https://github.com/fun-cqrs/fun-cqrs)
-   [Bank API CQRS Event Sourcing System on Akka-Cluster](https://github.com/j5ik2o/akka-ddd-cqrs-es-example)
-   [Event Horizon](https://github.com/looplab/eventhorizon)
-   [Go.CQRS](https://github.com/jetbasrawi/go.cqrs)

Features/Todos
--------------
-   ~~Akka Persistance~~
-   ~~Everything is serialized in Protocol Buffers~~
-   ~~FSM~~
-   ~~Test Actor Failover~~
-   ~~Travis CI Builds~~
-   ~~Codacy Code Review~~
-   ~~Codacy Code Coverage~~
-   Cassandra
-   Kafka
-   Saga
-   Akka Cluster and Cluster Sharding
-   Local Environment using Vagrant
-   SBT Sub-projects
-   Docker Images for each Sub-project
-   Kubernetes

Some Useful Commands
--------------------
-   To test:
    ```bash
    sbt clean test
    ```

-   To test with coverage:
    ```bash
    sbt clean coverage test
    ```

-   To generate coverage report:
    ```bash
    sbt coverageReport
    ```

-   To view dependency graph:
    ```bash
    sbt dependencyBrowseGraph
    ```

-   To view dependency graph for testing:
    ```bash
    sbt test:dependencyBrowseGraph
    ```

-   To check is the dependencies are latest:
    ```bash
    sbt dependencyUpdates
    ```

-   To check is the dependencies are latest for testing:
    ```bash
    sbt test:dependencyUpdates
    ```
