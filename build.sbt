/*
 * Copyright © 2019 Mustapha Cherri
 *
 * This file is part of erp-akka-cqrs-es.
 *
 * erp-akka-cqrs-es is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * erp-akka-cqrs-es is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with erp-akka-cqrs-es.  If not, see <https://www.gnu.org/licenses/>.
 */

organization := "mcherri"
name := "erp-akka-cqrs-es"

version := "0.1"

scalaVersion := "2.12.8"

//resolvers += "Sonatype OSS Release Repository" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += "dnvriend" at "http://dl.bintray.com/dnvriend/maven"

libraryDependencies ++= Seq(
  "com.typesafe.akka"          %% "akka-persistence"          % "2.5.19",
  "com.typesafe.akka"          %% "akka-remote"               % "2.5.19",
  "org.sisioh"                 %% "baseunits-scala"           % "0.1.21"
    exclude("org.scalactic", "scalactic_2.12"),
  "org.fusesource.leveldbjni"   % "leveldbjni-all"            % "1.8",
  "org.scalactic"              %% "scalactic"                 % "3.0.5",

  "org.scalatest"              %% "scalatest"                 % "3.0.5"    % Test,
  "org.pegdown"                 % "pegdown"                   % "1.6.0"    % Test,
  "com.typesafe.akka"          %% "akka-testkit"              % "2.5.19"   % Test,
  "org.iq80.leveldb"            % "leveldb"                   % "0.10"     % Test,
  "com.github.dnvriend"        %% "akka-persistence-inmemory" % "2.5.15.1" % Test
    exclude("com.typesafe.akka", "akka-actor_2.12")
    exclude("com.typesafe.akka", "akka-stream_2.12")
    exclude("com.typesafe.akka", "akka-persistence_2.12")
    exclude("com.typesafe.akka", "akka-persistence-query_2.12"),
  "com.typesafe.akka"          %% "akka-stream"               % "2.5.19"   % Test,
  "com.typesafe.akka"          %% "akka-persistence-query"    % "2.5.19"   % Test
)

// ScalaTest settings
logBuffered in Test := false

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD", "-h", "target/test-reports")

// ScalaPB settings
PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

libraryDependencies += "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"

// sbt-scoverage settings
coverageExcludedPackages := "mcherri\\.erp\\.akka\\.cqrs\\.es\\.model\\.protobuf\\..*"
coverageMinimum := 80
coverageFailOnMinimum := true