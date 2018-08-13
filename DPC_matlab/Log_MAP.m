% Log_MAP(y, sigma, LLR, gl1, gl2,q)
% Log_MAP function for use in the 'dpc.m' file.
%
% 18 April 2018, Paulo Alexandre Crisóstomo Lopes
% Lisbon University, Instituto Superior Técnico

function LLR = Log_MAP(y, var, LLR, gl1, gl2, q)
    global nt;
    M = length(y);                        % y is the channel output
    N = M/2*3;    assert(N==length(LLR)); % N is the number of input bits
    mem = 2;                              % the memory of the code
    tau = N/3;                            % the length of the terminated input sequence
    nStateBits = mem + 1;                 % lattice code memory + accumulator
    numStates = 2^nStateBits;             % number of state bits
    nInputBits = 3;                       % number of input bits
    assert(mod(nt, nInputBits)==0);
    numInputValues = 2^nInputBits;
    
    % Correspondence between matlab and BCJR paper variables
    %
    % k                 -> t: 1..N*R
    % s                 -> m
    % ss                -> m'
    % y(k)              -> Y_t
    % gamma(k,ss,s)     -> gamma(t,m',m)=P(S_t=m;Y_t|S_(t-1)=m')
    % alpha(k,s)        -> alpha(t-1,m)=P(St=m; Y_1^tau)
    % beta(k,s)         -> beta(t,m)=P(Y_(t+1)^tau|S_t=m)
    %
    % Other definitions
    %
    % LLR : LLR of the accumulator input bits at times (1 to N*R)
    %       log(P(0)/P(1)
    % s = bin2dec([c_t SVQ]);
    %
    % 4-PAM :        '00' -> -1.5; '01' -> -0.5; '10' -> +0.5; '11' -> +1.5; 

    % ********* Computing gamma for all states at each time *********

    log_gamma_ext = -inf*ones(tau, numStates, numInputValues, 2);
    log_gamma = -inf*ones(tau, numStates, 2,2);
    % log_gamma(k,ss,u,v)
    %
    % Note that the equation for the state is: x(n)=F(x(n-1),u(n))
    % and not x(n+1)= ...
    % This means that gamma(k,ss,s) can be replaced by gamma(k,ss,u,v)
    % because s is a function of ss, u and v. This leads to substantial
    % savings in computational complexity.
    % There are 8 possible values for u (3 bits)
    
    for k=1:tau
        for ss=1:numStates  % previous state
            for v=1:2
                for u=1:numInputValues
                    ss_b = my_dec2bin(ss-1, nStateBits);
                    u_b = my_dec2bin(u-1, nInputBits);
                    % these correspond to inputs 3*k-2:3*k
                    
                    % gamma(k,ss,s) = sum_x P(Sk=s|Sk-1=ss) P(Xt=x|Sk=s,Sk-1=ss) P(Yk|x)
                    % Accumulator + 4-PAM:  Sk = Uk xor Sk-1; Xk = 2*Sk-1 + Sk - 1.5; S0=0
                    
                    % termination
                    if k>=tau-nt/nInputBits+1 % nt=9
                        % the code words are formed by
                        % c_vq(n) = g0 u_vq(n) + g1 u_vq(n-1) + g2 u_vq(n-2)
                        % so the state of the vq is S_vq(n)=[u_vq(n-1), u_vq(n-2)].
                        % So in oder to have S_vq(tau)=0 -> the 3 last u_vq
                        % should be zero, and correspondingly the 9 last accumulator inputs.
                        
                        log_P = 0;  % transition probability
                        if (u-1~=ss_b(1) || v~=1)
                            continue; % log_gammma is -inf
                        end
                    else
                        % transition probability
                        log_P = sum(...
                            -max1(0, (2*u_b-1).*...
                            LLR(k*nInputBits-nInputBits+1:k*nInputBits))...
                            );
                        % one: log_P = -max1(0,LLR(k)); % zero: log_P = -max1(0,-LLR(k));
                    end
                    % Note that the probability of transition of the
                    % virtual bit is not required since it is a constant
                    % that can only depend on k.
                    
                    c_acc1 = xor(ss_b(1), u_b(1));
                    c_acc2 = xor(c_acc1, u_b(2));
                    c_acc3 = xor(c_acc2, u_b(3));
                    
                    cv1 = mod(dot(gl1, [v-1, ss_b(2:3)]),2);
                    yk1 = y(2*k-1);
                    x1 = 2*xor(c_acc1, cv1) + c_acc2 - 1.5;
                    log_gamma1 = -(mod2q(yk1-x1,q).^2)/2/var;
                        
                    cv2 = mod(dot(gl2, [v-1, ss_b(2:3)]),2);
                    yk2 = y(2*k);
                    x2 = 2*xor(c_acc1,cv2) + c_acc3 - 1.5;
                    log_gamma2 = -(mod2q(yk2-x2,q).^2)/2/var;
                    % log_gamma1 = 0; log_gamma2 = 0;
                    
                    log_gamma_ext(k,ss,u,v) = log_P + log_gamma1 + log_gamma2;
                    
                    u1 = xor(xor(u_b(1), u_b(2)), u_b(3)) + 1;
                    log_gamma(k,ss,u1,v) = max1(log_gamma(k,ss,u1,v), ...
                        log_P + log_gamma1 + log_gamma2);
                end
            end
        end
    end
    
    % ************** alpha recursions ********************
    log_alpha=-inf*ones(tau+1,numStates);
    log_alpha(1,1)=0;
    for k=2:tau+1
        for u=1:2
            for v=1:2
                for ss=1:numStates
                    s=state(ss,u,v,nStateBits);
                    log_alpha(k,s)= max1(log_alpha(k,s), log_gamma(k-1,ss,u,v)+log_alpha(k-1,ss));
                end
            end
        end
        if ~isinf(log_alpha(k,1))
            log_alpha(k,:)=log_alpha(k,:)-log_alpha(k,1); % Normalization;
        end
    end
       
    % ************** beta recursions ********************
    log_beta=-inf*ones(tau, numStates);
    log_beta(tau,1)=0; % this comes from de formula for beta(tau-1,:)
    for k=tau-1:-1:1
        for ss=1:numStates
            for u=1:2
                for v=1:2
                    s=state(ss,u,v,nStateBits);
                    log_beta(k,ss)=max1(log_beta(k,ss), log_gamma(k+1,ss,u,v)+log_beta(k+1,s));
                end
            end
        end
        if ~isinf(log_beta(k,1))
            log_beta(k,:)=log_beta(k,:)-log_beta(k,1) ; % Normalization
        end
    end
    
    %%%%%%%%%%%%%%%%%%% Computing the LLRs %%%%%%%%%%%%%%%
    for k=1:tau
        log_u = zeros(1,numInputValues);
        for u=1:numInputValues
            log=-inf;
            for ss=1:numStates
                for v=1:2
                    s=statex(ss,u,v,nStateBits);
                    log = max1(log, ...
                        log_alpha(k,ss)+log_gamma_ext(k,ss,u,v)+log_beta(k,s));
                end
            end
            log_u(u) = log;
        end
        % LLR=log_up-log_down; 0-> 1.0 (up); 1 -> -1.0 (down)
        LLR(3*k-2) = max1_v(log_u(1:4))-max1_v(log_u(5:8));
        LLR(3*k-1) = max1_v(log_u([0 1 4 5]+1))-max1_v(log_u([2 3 6 7]+1));
        LLR(3*k)   = max1_v(log_u(1:2:8))-max1_v(log_u((2:2:8)));
    end
end

function s=statex(ss,u,v,nSateBits)
    ub = my_dec2bin(u-1, 3);
    ux = xor(xor(ub(1), ub(2)), ub(3));
    s=state(ss,ux+1,v,nSateBits);
end

function s=state(ss,u,v,nSateBits)
    v_msb=2^(nSateBits-1);
    if ss-1 >= v_msb
        ssb1=1;
        ssb2=ss-1-v_msb;
    else
        ssb1=0;
        ssb2=ss-1;
    end
        
    s= xor(ssb1,u-1)*v_msb + floor(((v-1)*v_msb+ssb2)/2) + 1;
    % change the first bit and v enters to the shift register
end

% log(e^a+e^b)=max(a,b)+log(1+e^-|a-b|)
function c=max1(a,b)
    c = max(a,b)+log(1+exp(-abs(a-b)));
    c(a==inf & b==inf)=inf;
    c(a==-inf & b==-inf)=-inf;
end

function c=max1_v(a)
    c = -inf;
    for i=1:length(a)
        c = max1(c, a(i));
    end
end

% mod_2q: returns a number in ]-q,q]
function y=mod2q(x,q)
    y=mod(x,2*q);
    if (y>q)
        y=y-2*q;
    end
end

function b=my_dec2bin(d,n)
    b=rem(floor(d*pow2(1-n:0)),2);
end
