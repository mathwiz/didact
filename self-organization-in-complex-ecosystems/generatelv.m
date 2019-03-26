% Generator for Lotka-Voltera
% example: generatelv(0.1, 0.1, 0.2, 0.01)
function VAL = generatelv(x, y, r, b)
    VAL = x * exp(r * (1 - x - b*y));
endfunction
