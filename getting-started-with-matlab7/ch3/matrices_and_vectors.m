C(3, 1:3) = [1 2 3];

A = [1 2 3; 4 5 6; 7 8 8];

A(3,3) = 9;

B = A(2:3, :);

B(:, 2) = [];  # delete column 2

V1 = A(:);  # creates column vector by stacking columns of A

V2 = reshape(A,1,9);

At = A';  # or transpose(A)

Z = zeros(3,4); 

N = []; # a null matrix

D = [A ones(3,1)];

E = [zeros(1,3); A];

I = eye(3,3);

F1 = rand(4,3);

F2 = randn(4,3);

G = diag([1 2 3 4]);

rot90(A)

fliplr(A)

flipud(A)

tril(A)

triu(A)

diag(A)

V3 = 0: 2: 20;

V4 = 0: pi/10: 2*pi;

V5 = linspace(0, 20, 10);

V6 = logspace(0, 20, 10);
