% viterbi
%
% Calculates the decoded bit stream given an encoded bit stream and
% two encoding polynomials g1 and g2 in boolean vector form.
% All vectors are in row form.
% The coded bit stream should be properly terminated and the starting state
% should be zero. This will make the starting and end state equal to zero.
%
% The codeword is the closest lattice point to coded and has elements in Z.
% (0 or 1) + 2*n, n in Z
%
% 18 April 2018, Paulo Alexandre Crisóstomo Lopes
% Lisbon University, Instituto Superior Técnico


function [decoded, codeword] = viterbi(coded, g1, g2)
    global nt;
    m = length(g1)-1;       % memory
    ns = 2^m;               % number of states
    n = length(coded)/2;    % number of outputs
    assert(round(n)==n); assert(length(g1)==length(g2));
    
    decisions = zeros(n,ns);     % the decision at each time and state
    len = zeros(ns,1);           % the current path length for each state
    
    % Calculates tables for the next state
    S=repmat((0:ns-1)', 1, 2);  % line
    X=repmat([0 1], ns, 1);     % column
    ns_table = floor(S/2) + 2^(m-1)*X;

    % Calculates tables for the output
    outputs = zeros(ns, 2, 2);
    for s_i=0:ns-1
        for x_i=0:1
            h = [x_i 1*(dec2bin(s_i,m)=='1')];
            outputs(s_i+1,x_i+1,1) = g1*h';     % this doesn’t need to be mod(_,2) because the subtraction is
            outputs(s_i+1,x_i+1,2) = g2*h';     % this doesn’t need to be mod(_,2) because the subtraction is
        end
    end

    s=1:ns;
    % backward pass
    for i=n:-1:1
        coded_i=repmat(coded(2*i-1:2*i),ns,1);
        zero_len(s) = len(next_state(0,s,ns_table)) + ...
            distance(out(0,s,outputs), coded_i);
        one_len(s) = len(next_state(1,s,ns_table)) + ...
            distance(out(1,s,outputs), coded_i);

        dec = zero_len > one_len & i <= n-nt/3;   % nt = 9
       
        decisions(i,:)=dec;
        len = (one_len .* dec + zero_len .* ~dec)';
    end
    
    % forward pass
    s=1;
    decoded = zeros(1,n);
    codeword = zeros(1,2*n);
    for i=1:n
        d = decisions(i,s);
        ot1 = out(d, s, outputs);
        ot2 = coded(2*i-1:2*i);
        rdn = 2*round((ot2-ot1)/2);
        % this gives the correct element of coset
        codeword(2*i-1:2*i)=ot1+rdn;
                
        decoded(i)=d;
        s = next_state(d,s,ns_table);
    end
end

function y=mod1(a,b)
    y=mod(a,b);
    y=y-b.*(y>b/2);
end

% the euclidian distance mod 2
function y = distance(a,b)
    y=sum(mod1(a-b,2).^2,2);
end

% Function out
% x : input
% s : current state
function y = out(x, s, outputs)
    y=reshape(outputs(s,x+1,:),length(s),2);
end

% Function next_state
% x : input
% s : current state
function ns = next_state(x,s,ns_table)
    ns = ns_table(s,x+1)+1;
end

