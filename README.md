# ID2201_DistributedSystems
Deliverables for the course ID2201  taken as a part of the masters program in Software Engineering of Distributed Systems at KTH. Programming done in Erlang.

The deliverables are as follows:-

## Rudy

This assignment mainly deals with the creation of a simplistic web server named Rudy using the Erlang programming language.The main purpose of this exercise is to gain knowledge of the following concepts:
1.Understanding of the client-server architecture which forms the foundation of distributed systems.
2.Process for implementing a Socket API.
3.Passing message through TCP protocol.
4.Parsing of a http message and in turn learning about the components of the http header.

## Routy

In this assignment we implement a link state routing protocol using erlang. We go in detail into the Dijkstra/Open shortest path first(OSPF) algorithm, the most used routing protocol for routers. The Dijkstra algorithm helps us to find the shortest route it takes for a message to travel among the graph of nodes. We also learn about:
1.The structure of a link-state routing protocol.
2.Maintaining of a consistent view.
3.Observe network failure related issues.
4.Greater handling of lists and tuples, how to record, search and modify information in erlang.

## Loggy

In this assignment we learn how to work with logical time in distributed systems with the help of a practical example.We implement a logging procedure that is used to send processes from one place to another,which is not as straightforward as it seems when needed in an ordered fashion as each system will have its own time and this will lead to varied timestamps.The processes are tagged with the Lamport time stamp of the workers and these events need to be ordered before being written to the stdout to manage flow of information.

## Groupy

In this assignment we implement a group membership service that is used to demonstrate reliable multicasting. The prime purpose is to have multiple application layer processes to be in sync whilst passing messages i.e they should all perform the same sequence of state changes. The problem to solve is ensuring the synchronization of nodes despite the addition of new and removal of existing nodes from the network. GUI is given to us to show the states in terms of colours being displayed at the same time.

## Chordy

In this assignment we implement a distributed hash table by using the chord algorithm.Fundamentally this involves constructing a ring network with nodes having a list of storage that can store the key/value pairs.We implement this in erlang with the core of the chord protocol but in a simplified manner.This assignment will show how nodes are defined and structured in the ring and also introduce a storage module to demonstrate how keys are shared between the nodes.
