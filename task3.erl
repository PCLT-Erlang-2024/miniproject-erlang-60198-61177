-module(task3).
-export([start/0, start/4, conveyor_belt/3, truck/4]).


start() -> io:format("Please use the following: '~p:start(ConveyorNr, TotalPackageNr, MaxTruckWeightCapacity, MaxPackageWeight)'~n", [?MODULE]).
start(ConveyorNr, TotalPackageNr, MaxTruckWeightCapacity, MaxPackageWeight) 
  when ConveyorNr < 1 orelse TotalPackageNr < 1 orelse MaxTruckWeightCapacity < 1 orelse MaxPackageWeight < 0
    -> {error, "ConveyorNr, TotalPackageNr, MaxTruckWeightCapacity and MaxPackageWeight should all be positive integers!"};

start(_, _, MaxTruckWeightCapacity, MaxPackageWeight) 
  when MaxPackageWeight > MaxTruckWeightCapacity
    -> {error, "MaxPackageWeight should be less or equals to MaxTruckWeightCapacity."};

start(ConveyorsNr, TotalPackageNr, MaxTruckWeightCapacity, MaxPackageWeight) -> 
    register(factory, self()),
    io:format("[ Factory ] Starting with ~p conveyor belts and ~p packages~n", [ConveyorsNr, TotalPackageNr]),
    MinTruckWeightCapacity = MaxPackageWeight,
    Conveyors = [
        {
            ConveyorId, 
            spawn(?MODULE, conveyor_belt, [ConveyorId, MinTruckWeightCapacity, MaxTruckWeightCapacity])
        } || ConveyorId <- lists:seq(1, ConveyorsNr)
    ],
    feed_conveyors(Conveyors, TotalPackageNr, MaxPackageWeight),
    io:format("[ Factory ] Waiting for conveyors acknowledges~n"),
    wait_for_acks(ConveyorsNr).


% send packages to conveyor belts
feed_conveyors(Conveyors, TotalPackageNr, MaxPackageWeight) -> send_packages_to_conveyors(
    Conveyors, [ 
        {PkgId, rand:uniform(MaxPackageWeight)} || PkgId <- lists:seq(1, TotalPackageNr) 
    ], Conveyors
).

send_packages_to_conveyors(InitConveyors, [], _) -> [Pid ! {self(), finish} || {_, Pid} <- InitConveyors]; % send finish signal to conveyors
send_packages_to_conveyors(InitConveyors, Packages, []) -> 
    send_packages_to_conveyors(InitConveyors, Packages, InitConveyors);
send_packages_to_conveyors(InitConveyors, [{PkgId, PkgWeight} | Packages], [{ConveyorId, Pid} | Conveyors]) -> 
    io:format("[ Factory ] Sending package { PkgId: ~p, Weight: ~p } to conveyor ~p~n", [PkgId, PkgWeight, ConveyorId]),
    Pid ! {self(), {PkgId, PkgWeight}}, 
    send_packages_to_conveyors(InitConveyors, Packages, Conveyors).

% wait for conveyors acknowledges
wait_for_acks(0) -> unregister(factory), ok;
wait_for_acks(Rem) -> 
    receive
        ack -> 
            wait_for_acks(Rem - 1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%
% Conveyor Belt things %
%%%%%%%%%%%%%%%%%%%%%%%%

conveyor_belt(ConveyorId, MinTruckWeightCapacity, MaxTruckWeightCapacity) -> 
    io:format("[ Conveyor Belt ~p ] Starting~n", [ConveyorId]),
    case whereis(factory) of
        undefinied -> error("'factory' service not defined");
        Pid when is_pid(Pid) -> 
            TruckWeightCapacity = rand_range(MinTruckWeightCapacity, MaxTruckWeightCapacity),
            TruckCount = 1,
            feed_trucks(
                ConveyorId, Pid, {MinTruckWeightCapacity, MaxTruckWeightCapacity}, TruckCount, 
                spawn(?MODULE, truck, [self(), ConveyorId, TruckCount, TruckWeightCapacity])
            )
    end.
    
feed_trucks(ConveyorId, FactoryPid, TruckConfig, TruckCount, CurrentTruckPid) -> 
    receive
        {FactoryPid, finish}  -> 
            io:format("[ Conveyor Belt ~p ] Received 'No more package signal' from Factory~n", [ConveyorId]),
            CurrentTruckPid ! {self(), finish},
            receive
                {CurrentTruckPid, done} -> FactoryPid ! ack
            end;

        {FactoryPid, Package} -> 
            {PkgId, Weight} = Package,
            io:format("[ Conveyor Belt ~p ] Received package { PkgId: ~p, Weight: ~p } from factory~n", [ConveyorId, PkgId, Weight]),
            send_to_truck(ConveyorId, TruckCount, CurrentTruckPid, Package),

            receive 
                {CurrentTruckPid, next_package} -> 
                    feed_trucks(
                        ConveyorId, FactoryPid, TruckConfig, TruckCount, CurrentTruckPid
                    );

                {CurrentTruckPid, truck_could_not_hold, OverFlowPkg} -> 
                    io:format(
                    "[ Conveyor Belt ~p ] Received 'Not enough space for new package' signal from Truck ~p-~p~n", 
                    [ConveyorId, ConveyorId, TruckCount]
                    ),
                    {MinTruckWeightCapacity, MaxTruckWeightCapacity} = TruckConfig,
                    TruckWeightCapacity = rand_range(MinTruckWeightCapacity, MaxTruckWeightCapacity),
                    feed_trucks(
                        ConveyorId, FactoryPid, TruckConfig,
                        TruckCount + 1, spawn(?MODULE, truck, [self(), ConveyorId, TruckCount + 1, TruckWeightCapacity])
                    ),
                    send_to_truck(ConveyorId, TruckCount, CurrentTruckPid, OverFlowPkg)
            end
    end.


send_to_truck(ConveyorId, TruckId, TruckPid, {PkgId, Weight}) -> 
    io:format(
        "[ Conveyor Belt ~p ] Sending package { PkgId: ~p, Weight: ~p } to Truck ~p-~p~n", 
        [ConveyorId, PkgId, Weight, ConveyorId, TruckId]
    ),
    TruckPid ! {self(), {PkgId, Weight}}.

rand_range(Min, Max) when Min == Max -> Min;
rand_range(Min, Max) -> Min + rand:uniform(Max - Min).

%%%%%%%%%%%%%%%
% Truckthings %
%%%%%%%%%%%%%%%
max_departure_wait_time_ms() -> 100.

truck(ConveyorPid, ConveyorId, TruckId, TruckWeightCapacity) -> 
    io:format("[ Truck ~p-~p ] Starting with capacity for ~p unit of weight~n", [ConveyorId, TruckId, TruckWeightCapacity]),
    CarriedWeight = consume_packages(ConveyorPid, ConveyorId, TruckId, 0, TruckWeightCapacity),
    WaitTime = rand:uniform(max_departure_wait_time_ms()),
    io:format("[ Truck ~p-~p ] Waiting ~p milliseconds before departing~n", [ConveyorId, TruckId, WaitTime]),
    timer:sleep(WaitTime),
    io:format("[ Truck ~p-~p ] Departing carrying ~p/~p of weight...~n", [ConveyorId, TruckId, CarriedWeight, TruckWeightCapacity]).

consume_packages(ConveyorPid, ConveyorId, TruckId, CarriedWeight, TruckWeightCapacity) -> 
    receive 
        {ConveyorPid, finish} -> 
            io:format(
                "[ Truck ~p-~p ] Received 'No more package signal' from Conveyor Belt ~p~n",
                [ConveyorId, TruckId, ConveyorId]
            ),
            ConveyorPid ! {self(), done},
            CarriedWeight
        ;
        {ConveyorPid, Package} -> 
            {PkgId, Weight} = Package,
            io:format(
                "[ Truck ~p-~p ] Received package { PkgId: ~p, Weight ~p } from Conveyor Belt ~p~n",
                 [ConveyorId, TruckId, PkgId, Weight, ConveyorId]
            ),
            NewCarriedWeight = CarriedWeight + Weight,
            if 
                NewCarriedWeight > TruckWeightCapacity -> 
                     ConveyorPid ! {self(), truck_could_not_hold, Package},
                     io:format("[ Truck ~p-~p ] Truck cannot hold new capacity...~n", [ConveyorId, TruckId]),
                     CarriedWeight
                     ;
                true -> 
                    ConveyorPid ! {self(), next_package},
                    consume_packages(ConveyorPid, ConveyorId, TruckId, NewCarriedWeight, TruckWeightCapacity)
            end
    end.
