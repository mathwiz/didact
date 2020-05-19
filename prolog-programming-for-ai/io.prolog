% load terms from file
% [eight_queens].

% open a file for output
tell(queens).

% write to the output stream
write( template( S ), solution( S ) ).

% set the output stream back to terminal
tell(user).
