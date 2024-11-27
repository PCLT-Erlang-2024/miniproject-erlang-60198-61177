# Group

Our group is composed of:

- Iago Paulo (60198)
- James Furtado (61177)

# Organization & Important info

Each task was implemented on the file named `task<task-nr>.erl` where `<task-nr>` is the respective task number (i.e. 1, 2 and 3). To run each version of the task there is a respective function named `start(...)` which takes different number of parameters in different task (except the last two which are identical). In order to see what parameters are necessary without having to see in the code, for the respective task just run the `start()` function which will show the respective information.

# Architecture

Basically we have a factory that sends packages to differents conveyor belts in a round-robin fashion. Each respective belt receives packages and sends it to the current truck it is talking too. Then when the truck is full or the factory tells it there is no more package it just ends.

As for the **task3** due to time constraints, to define the max time (in milliseconds) a truck will wait we used a function that returns 100.
