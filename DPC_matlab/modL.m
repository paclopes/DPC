function x=modL(v, q, gl1, gl2)
    [~,c]=viterbi(v/q, gl1, gl2);
    x = v - q*c;
    % x has elements in [-q, q]
end

