-module(task1).
-export([start/0, start/3, conveyor_belt/2, truck/4]).


start() -> io:format("Please use the following: '~p:start(ConveyorNr, TotalPackageNr, MaxTruckCapacity)'~n", [?MODULE]).
start(ConveyorNr, TotalPackageNr, MaxTruckCapacity) when ConveyorNr < 1 orelse TotalPackageNr < 1 orelse MaxTruckCapacity < 1 
    -> {error, "ConveyorNr, TotalPackageNr and MaxTruckCapacity should all be positive integers!"};

start(ConveyorsNr, TotalPackageNr, MaxTruckCapacity) -> 
    register(factory, self()),
    io:format("[ Factory ] Starting with ~p conveyor belts and ~p packages~n", [ConveyorsNr, TotalPackageNr]),
    Conveyors = [
        {ConveyorId, spawn(?MODULE, conveyor_belt, [ConveyorId, MaxTruckCapacity])} || ConveyorId <- lists:seq(1, ConveyorsNr)
    ],
    feed_conveyors(Conveyors, TotalPackageNr),
    io:format("[ Factory ] Waiting for conveyors acknowledges~n"),
    wait_for_acks(ConveyorsNr).


% send packages to conveyor belts
feed_conveyors(Conveyors, TotalPackageNr) -> feed_conveyors(Conveyors, lists:seq(1, TotalPackageNr), Conveyors).

feed_conveyors(InitConveyors, [], _) -> [Pid ! {self(), finish} || {_, Pid} <- InitConveyors]; % send finish signal to conveyors
feed_conveyors(InitConveyors, Packages, []) -> 
    feed_conveyors(InitConveyors, Packages, InitConveyors);
feed_conveyors(InitConveyors, [Pkg | Packages], [{ConveyorId, Pid} | Conveyors]) -> 
    io:format("[ Factory ] Sending package ~p to conveyor ~p~n", [Pkg, ConveyorId]),
    Pid ! {self(), Pkg}, 
    feed_conveyors(InitConveyors, Packages, Conveyors).

% wait for conveyors acknowledges
wait_for_acks(0) -> unregister(factory), ok;
wait_for_acks(Rem) -> 
    receive
        ack -> 
            io:format("[ Factory ] ~p~n", [Rem]),
            wait_for_acks(Rem - 1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%
% Conveyor Belt things %
%%%%%%%%%%%%%%%%%%%%%%%%

conveyor_belt(ConveyorId, MaxTruckCapacity) -> 
    io:format("[ Conveyor Belt ~p ] Starting~n", [ConveyorId]),
    case whereis(factory) of
        undefinied -> error("'factory' service not defined");
        Pid when is_pid(Pid) -> 
            TruckCapacity = rand:uniform(MaxTruckCapacity),
            TruckCount = 1,
            feed_trucks(
                ConveyorId, Pid, MaxTruckCapacity, TruckCount, 
                spawn(?MODULE, truck, [self(), ConveyorId, TruckCount, TruckCapacity])
            )
    end.
    
feed_trucks(ConveyorId, FactoryPid, MaxTruckCapacity, TruckCount, CurrentTruckPid) -> 
    receive
        {FactoryPid, finish}  -> 
            io:format("[ Conveyor Belt ~p ] Received 'No more package signal' from Factory~n", [ConveyorId]),
            CurrentTruckPid ! {self(), finish},
            receive
                {CurrentTruckPid, done} -> FactoryPid ! ack
            end;

        {FactoryPid, Package} -> 
            io:format("[ Conveyor Belt ~p ] Received package ~p from factory~n", [ConveyorId, Package]),
            io:format(
              "[ Conveyor Belt ~p ] Sending package ~p to Truck ~p-~p~n", 
               [ConveyorId, Package, ConveyorId, TruckCount]
            ),
            CurrentTruckPid ! {self(), Package}
    end,

    receive 
        {CurrentTruckPid, truck_is_full} -> 
            io:format(
              "[ Conveyor Belt ~p ] Received 'Truck is full' signal from Truck ~p-~p~n", 
               [ConveyorId, ConveyorId, TruckCount]
             ),
            % TODO: Generate truck capacity randomly
            TruckCapacity = rand:uniform(MaxTruckCapacity),
            feed_trucks(
              ConveyorId, FactoryPid, MaxTruckCapacity,
              TruckCount + 1, spawn(?MODULE, truck, [self(), ConveyorId, TruckCount + 1, TruckCapacity])
            );
        {CurrentTruckPid, next_package} -> 
            feed_trucks(
                ConveyorId, FactoryPid, MaxTruckCapacity, TruckCount, CurrentTruckPid
            )
    end.


%%%%%%%%%%%%%%%
% Truckthings %
%%%%%%%%%%%%%%%
truck(ConveyorPid, ConveyorId, TruckId, TruckCapacity) -> 
    io:format("[ Truck ~p-~p ] Starting with capacity for ~p packages~n", [ConveyorId, TruckId, TruckCapacity]),
    consume_packages(ConveyorPid, ConveyorId, TruckId, TruckCapacity),
    io:format("[ Truck ~p-~p ] Departing...~n", [ConveyorId, TruckId]).

consume_packages(ConveyorPid, ConveyorId, TruckId, TruckCapacity) -> 
    receive 
        {ConveyorPid, finish} -> 
            io:format(
                "[ Truck ~p-~p ] Received 'No more package signal' from Conveyor Belt ~p~n",
                [ConveyorId, TruckId, ConveyorId]
            ),
            ConveyorPid ! {self(), done}
        ;
        {ConveyorPid, Package} -> 
            io:format(
                "[ Truck ~p-~p ] Received package ~p from Conveyor Belt ~p~n",
                 [ConveyorId, TruckId, Package, ConveyorId]
            ),
            case TruckCapacity - 1 of
                0 -> ConveyorPid ! {self(), truck_is_full},
                     io:format("[ Truck ~p-~p ] Truck is full!!!~n", [ConveyorId, TruckId]);
                NewCapacity -> 
                    ConveyorPid ! {self(), next_package},
                    consume_packages(ConveyorPid, ConveyorId, TruckId, NewCapacity)
            end
    end.
