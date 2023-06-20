% To run https://swish.swi-prolog.org/
% Ubuntu command <swipl>
:- discontiguous process_input/1.

% Dynamic room descriptions
room(bedroom, Description) :-
    position(you, bedroom),
    item(_, bedroom, Spot),
    generate_bedroom_description(Spot, Description).
room(bedroom) :-
    position(you, bedroom),
    (item(Item, bedroom, Spot)
        -> write('You find yourself in a mystical bedroom. The air is filled with an otherworldly aura. You can sense a path leading to the Hall if you go <straight>. You see a '), write(Item), write(' on the '), writeln(Spot)
        ;  writeln('You find yourself in a mystical bedroom. The air is filled with an otherworldly aura. You can sense a path leading to the Hall if you go <straight>.')
    ).

room(hall) :-
    position(you, hall),
    (item(Item, hall, Spot)
        -> write('As you step into the Hall, the mystical energy intensifies. Mysterious doorways await your exploration. Kitchen:<straight> Bedroom:<back> Sanctuary:<right>. You see a '), write(Item), write(' on the '), writeln(Spot)
        ;  writeln('As you step into the Hall, the mystical energy intensifies. Mysterious doorways await your exploration. Kitchen:<straight> Bedroom:<back> Sanctuary:<right>.')
    ).

room(kitchen) :-
    position(you, kitchen),
    (item(Item, kitchen, Spot)
        -> write('You enter a mystical kitchen where enchanted ingredients and ancient recipes fill the air. The Hall beckons you to go <back>, while a passage leads to a hidden Balcony:<straight>. You see a '), write(Item), write(' on the '), writeln(Spot)
        ;  writeln('You enter a mystical kitchen where enchanted ingredients and ancient recipes fill the air. The Hall beckons you to go <back>, while a passage leads to a hidden Balcony:<straight>.')
    ).

room(balcon) :-
    position(you, balcon),
    (item(Item, balcon, Spot)
        -> write('You stand on the mystical balcony, surrounded by ethereal beauty. The window reveals a breathtaking view. To return to the Kitchen, go <back>. You see a '), write(Item), write(' on the '), writeln(Spot)
        ;  writeln('You stand on the mystical balcony, surrounded by ethereal beauty. The window reveals a breathtaking view. To return to the Kitchen, go <back>.')
    ).

room(sanctuary) :-
    position(you, sanctuary),
    (item(Item, sanctuary, Spot)
        -> write('You have discovered a sacred sanctuary, pulsating with ancient power. The secrets of the mystical realm lie within. To return to the Hall, venture <back>. You see a '), write(Item), write(' on the '), writeln(Spot)
        ;  writeln('You have discovered a sacred sanctuary, pulsating with ancient power. The secrets of the mystical realm lie within. To return to the Hall, venture <back>.')
    ).
  

% item facts
item(book, bedroom, floor).
item(apple, kitchen, table).
item(sword, sanctuary, pedestal).

% item properties
is_interactable(book).
is_interactable(apple).
is_interactable(sword).

% Interactions
interact(book) :-
    position(you, CurrentPos),
    item(book, CurrentPos, _),  % Here's the change, we consider the Spot argument.
    writeln('You examine the book. It is filled with ancient wisdom.').

interact(apple) :-
    position(you, CurrentPos),
    item(apple, CurrentPos, _),  % Here's the change, we consider the Spot argument.
    writeln('You take a bite of the juicy apple. It tastes delicious.').

interact(sword) :-
    position(you, CurrentPos),
    item(sword, CurrentPos, _),  % Here's the change, we consider the Spot argument.
    writeln('You pick up the sword. Its blade gleams with a mystical aura.').



% item spots in rooms
spot_in_room(bedroom, [table, bed]).
spot_in_room(kitchen, [counter, shelf, sink]).
spot_in_room(sanctuary, [altar, pedestal]).

% Updated paths
path(bedroom, hall, straight).
path(hall, bedroom, back).
path(hall, kitchen, straight).
path(hall, sanctuary, right).
path(kitchen, hall, back).
path(kitchen, balcon, straight).
path(sanctuary, hall, back).

process_input(interact(Item)) :-   % Here's the change, Item should be a variable, not an atom.
    is_interactable(Item),
    position(you, CurrentPos),
    item(Item, CurrentPos, _),  % Here's the change, we consider the Spot argument.
    interact(Item).



process_input(move(Item, NewSpot)) :-
    is_interactable(Item),
    position(you, CurrentPos),
    item(Item, CurrentPos, _),
    retract(item(Item, CurrentPos, _)),
    assert(item(Item, CurrentPos, NewSpot)),
    writeln(Item), writeln('has been moved to'), writeln(NewSpot).



showControlls :- % shows list of available controls
    nl,
    writeln('               Hello, traveler!'),
    writeln(' Mode          Command                 Description'),
    writeln(''),
    writeln('[walk]         <straight>              room next to you'),
    writeln('[walk]         <back>                  room behind you'),
    writeln('[walk]         <left>                  room left to you'),
    writeln('[walk]         <right>                 room right to you'),
    writeln('[interact]     interact(<item>)        interact with an item'),
    writeln('[move]         move(<item>, <spot>)    move an item to a different spot').

        

 % Generating dynamic bedroom descriptions
generate_bedroom_description(table, 'You find yourself in a mystical bedroom. The air is filled with an otherworldly aura. You can sense a path leading to the Hall if you go <straight>. A book is placed on the table.').
generate_bedroom_description(bed, 'You find yourself in a mystical bedroom. The air is filled with an otherworldly aura. You can sense a path leading to the Hall if you go <straight>. The bed looks inviting.').
generate_bedroom_description(_,'You find yourself in a mystical bedroom. The air is filled with an otherworldly aura. You can sense a path leading to the Hall if you go <straight>.').
           


:- dynamic position/2.
:- dynamic item/3.
:- discontiguous process_input/1.
:- discontiguous process_interaction/3.
:- dynamic spot_in_room/2.



read_input(Words) :-
    read_line_to_string(user_input, Input),
    split_string(Input, " ", " ", Words).





process_input(help) :-
	showControlls.


process_input(Direction) :-
    position(you, CurrentPos),
    path(CurrentPos, NextPos, Direction),
    retract(position(you, CurrentPos)),
    assert(position(you, NextPos)),
    showPosition,
    !.   

process_input(_) :-
    writeln('Nothing in this direction'),
    showPosition.


inputHandler :-
    nl,
    writeln('What will you do?'),
    read(Input),
    call(process_input(Input)),
    inputHandler.


showPosition :-
    nl,
    position(you, CurrentPos),
    call(room(CurrentPos)).

setGame :-
    retractall(position(_,_)),
    assert(position(you, bedroom)).

%Starting game
startGame :-
    setGame,
    showControlls,
    showPosition,
    inputHandler.
