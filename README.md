Expectiminimax for Splendor
=================================

This is a program for my Introduction to AI final project at UNH. It plays the game Splendor.

Since the youngest player is supposed to move first, the AI is set up by default to move first. If you were born after April of 20188, invoke it as `./Main second` to make the AI move second.

Change the initial setup in the "Splendor.hs" file when dealing a new game. `make` will build `./Main`. `make prof` will run performance tests (note: this requires nix).

Direct all questions and complements to setupminimal@gmail.com. Direct all complaints about the code and UI to abuse@localhost.

How do I actually play against this program?
------------------------------------------------------

You will need a marked deck, marked the same as the data entered in "Data.hs", because that's how the AI refers to cards on the table.

Invoke the AI as `./Main` after running make. It will think for a while (~30 seconds on my laptop), before saying something like "Do: Take [Onyx, Ruby, Emerald]". This indicates that it wants to take an Onyx, Ruby, and Emerald token from the bank. It will then wait for your response. Here are the moves you can make:

- Take [Gem, Gem, Gem] - Take these gems from the bank.
- BuyCard 73 - Buy the card from the table (or your reserved pile) marked with that number
- NewCard 11 - Indicate that this card was put on the table from the deck.
- NoNewCard - Indicate that we are out of cards, and no card was drawn to replace a bought card.
- Reserve True 43 - Reserve the card marked with that number
- Win - You've won! Good job!
- Quit - Give up
- Many [Move, Move, ...] - Used in a game with more than 2 people to indicate that a series of actions happened.
- Magic [Gem, ...] 22 [96, ...] - Tell the AI that it has gotten something horribly wrong, and some magic needs to happen to put the board back in order.

This isn't compiling
------------------------

Its clearly your fault, and not mine. You might try ensuring that you have the following libraries:

- parallel
- parallel-io
- random

and that you have an updated version of ghc. (I'm developing with 8.2.2)
