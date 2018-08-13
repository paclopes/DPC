% DCP
%
% Implementation of the a Dirty Paper Coding MODEM as described in:
% Erez, Uri, and Stephan Ten Brink. "A close-to-capacity dirty paper coding scheme." 
% IEEE Transactions on information theory 51.10 (2005): 3417-3432.
%
% 18 April 2018, Paulo Alexandre Crisóstomo Lopes
% Lisbon University, Instituto Superior Técnico

% w -> encoder -> x -> channel -> y -> decoder -> w_hat

rng(157572);
global nt;

N=4;                                    % N-PAM
q=N/2;  
q2=N;
log2N = log2(N);
nt = 9;                                 % number of termination symbols
% Because the virtual bits only enter the shift register once every 3
% time units the number of termination bits is high...

% --- Repeat Accumulator Code parameters ---
% --- Rch = 1/6 or 0.5 bits/s/Hz VQ mem 2

k = 6000;                               % the number of information bits in a word
Rch = 1/6;                              % the RA (channel code) rate
n_ch = k/Rch+nt;                        % the number of bits in a channel code word
CND1_p = 0.8; CND3_p = 0.2;             % the CND percentages
VND_dg = [3 10 76];                     % the VND degrees
VND_p = [0.6436 0.3124 0.04402];        % the VND percentages

CND = k /Rch;                      
CND3 = round(k/Rch*CND3_p);
CND1 = CND - CND3;
interleaver_size = CND1 + CND3 * 3;             % Interleaver size / VAR nodes outputs / CND inputs

VND = round(k*VND_p);
VND(1) = VND(1) + k - sum(VND);                 % compensates for rounding errors
assert(sum(VND)==k);

interleaver_size_vnd = sum(VND.*VND_dg);        % size of the interleaver using the VND

fprintf(1,'Interleaver size, using the VNDs and actual: %d; %d\n', interleaver_size_vnd, interleaver_size);

VNDc = interleaver_size - interleaver_size_vnd; % the number of repetition to add/sub to the last VND node
VND_dg = [VND_dg VND_dg(end)+VNDc];             % add another type of VND
VND(end)=VND(end)-1;
VND=[VND 1];
assert(sum(VND)==k); assert(sum(VND.*VND_dg)==interleaver_size);

interlever_perm = randperm(interleaver_size);         % interleaver
[~, interlever_inv_perm] = sort(interlever_perm);

% CND intercalated interleaver
% cnd_interlever_perm = zeros(1,CND);
% cnd_periode = floor(CND1/CND3)+1;
% CND1i=1; CND3i=1;
% for i=1:CND
%     if and(mod(i-1,cnd_periode)==0, CND3i<=CND3)
%         % choses one order 3 check node
%         cnd_interlever_perm(i)=CND1+CND3i;
%         CND3i=CND3i+1;
%     else
%         % choses one order 1 check node
%         cnd_interlever_perm(i)=CND1i;
%         CND1i=CND1i+1;
%     end
% end

% to place all the CND3 at the start:
cnd_interlever_perm = zeros(1,CND);
cnd_interlever_perm(1:CND3)=(1:CND3)+CND1;
cnd_interlever_perm(CND3+1:CND)=1:CND1;

% to place all the CND1 at the start:
% cnd_interlever_perm = 1:CND;

[~, cnd_interlever_inv_perm] = sort(cnd_interlever_perm);

% --- Lattice Code parameters ---

% Lattice polynomials
% memory 2 (3 bits) : 05 and 07 in octal (shaping gain of 0.98 dB)
% memory 8 (9 bits) : 0561 and 0753 in octal (shaping gain of 1.28 dB)
% theoretical max shaping gain is 1.53 dB
% The poly has the form g0 + g1 z^-1 + g2 z^-2 + ...

% m = 6; % the memory of the code
% gl1=1*(dec2bin(base2dec('0133', 8),m+1)=='1');
% gl2=1*(dec2bin(base2dec('0172', 8),m+1)=='1');

m = 2;                          % the memory of the code lattice code
gl1=1*(dec2bin(base2dec('05', 8),m+1)=='1');
gl2=1*(dec2bin(base2dec('07', 8),m+1)=='1');

% the input word length of the shaping code, 
% k, may set to greater or equal to the polynomial length

ns = n_ch*2/(2*log2N-1);        % Number of symbols per block (N-PAM)
nl = ns;                        % the lattice embedding dimension
kl = nl/2;                      % the dimension of the lattice code input

% Ce = 1/2*log2(1+Sx/Nx) ---- for PAM
% 4*Ce = 1 (for each input bit there are 4 analog samples)
% Results in Nx = Sx /(sqrt(2)-1) = 2.41*Sx
% Note that Sx is not (N^2-1)/12 because od the mod-L operation.

% gsdb : 0.98 mem 2; 1.13 mem 4; 1.215 mem 4; 1.28 mem 8
gsdb = 0.98;
G = 10^(-gsdb/10)/12;
% The volume of the lattice is equal to the volume of the imbedding lattice
% divided by the number of regions or lattice points in the
% larger region. Note that L=C+2*Z^ns
% V = 2^nl/2^(nl/2) = 2^(nl/2);
% V^(2/nl) = 2
% P = V^(2/nl) * G
P = 2*G;
Sx = q^2*P;
fprintf(1, 'The transmitted power is: %f\n',Sx);
Nx = Sx*1.5;    % Converges in 39 iterations
% We are 2.059258 dB from capacity. (Erez was at 1.9 dB)
% Eb/N0: 1.249387 dB. (Erez used 1.1 dB)

fprintf(1, 'The noise power is: %f\n',Nx);
sigma = sqrt(Nx);
fprintf(1, 'We are %f dB from capacity. (Erez was at 1.9 dB)\n', 10*log10(2.41*Sx/Nx));

% Eb R = S; N0 B = N -> S/N = Eb/N0 R/B
% For a complex channel (normal ones in tele) min Eb/N0 = -1.59 dBs as in
% the paper (For a real channel min Eb/N0 = 1.42 dB).
% In our case R/B = 1/4 * 2 (two real channels)

fprintf(1, 'Eb/N0: %f dB. (Erez used 1.1 dB)\n', 10*log10(2*Sx/Nx));

% alpha = 0.4;                              
alpha = Sx/(Sx+Nx);                         % lattice inflation factor
fprintf(1,'alpha is %d\n', alpha);
q_VQ = (1-alpha)^2*Sx+alpha^2*sigma^2;      % the noise value used in the BCJR

% table for the max1 function in Log_MAP
global max1_table;
d=0:15;
max1_table(d+1) = log(1+exp(-d));

% --- Simulation ---

Nblocks = 2;                          % Number of samples or symbols
NDecodingIter = 50;                   % Number of iteration of the decoder

for j=1:Nblocks
    w = randi([0,1], 1, k);
    fprintf('block %d\n', j);
    
    %%%%%%%%%%%%%%%%%%%% Coder
    
    % RA code
    % VND
    n_VND_dg = length(VND_dg);
    vnd_i=1; c_vnd=zeros(1, interleaver_size); vnd_j=1;
    for i=1:n_VND_dg
        degree = VND_dg(i);
        n_nodes = VND(i);
        n_outputs = n_nodes*degree;
        rp = repmat(w(vnd_i:vnd_i+n_nodes-1), degree, 1);
        c_vnd(vnd_j:vnd_j+n_outputs-1)=rp(:);
        vnd_i = vnd_i + n_nodes;
        vnd_j = vnd_j + n_outputs;
    end
    assert(vnd_i == k+1);
    assert(vnd_j == interleaver_size+1);
    
    % interleaver
    c_interleaver = c_vnd(interlever_perm);
    
    % CND
    assert(CND1+3*CND3==length(c_interleaver));
    CND_mat = reshape(c_interleaver(CND1+1:end), 3, []);
    c_CND0 = [c_interleaver(1:CND1) mod(sum(CND_mat),2)];
    assert(length(c_CND0)==k/Rch);
    c_CND = c_CND0(cnd_interlever_perm);
    
    % accumulator
    c_acc = [mod(cumsum(c_CND), 2) zeros(1,nt)];
    % the zeros terminates the code
    
    % upsampler (repetition code)
    % groups the bits into sets of log2N bits with the sign bit repeated
    % two by two.
    c_up0 = reshape(c_acc, 2*log2N-1, []);
    c_up1 = [c_up0(1:log2N,:); c_up0(1,:); c_up0(log2N+1:2*log2N-1,:)];
    % Example: N=8 (8-PAM); c_up=c_acc(1,2,3,1,4,5)
    c_up = c_up1(:);
    
    % N-PAM
    x_PAM0 = reshape(c_up, log2N, []);
    x_PAM = sum(x_PAM0.*repmat(2.^(log2N-1:-1:0)', 1, size(x_PAM0,2)),1)-(N-1)/2;
    % example: v5 = 2*v4(1,:)+v4(2,:)-1.5 (4-PAM)
    % example: v5 = 4*v4(1,:)+2*v4(2,:)+v4(3,:)-3.5 (8-PAM)
    
    % dither and interference
       
    u = 4*rand(1,nl)-2;
    s = 10*randn(1,nl);
    v = x_PAM-u-alpha*s;
    
    % mod-L
    assert(length(v)==nl);
    x = modL(v, q, gl1, gl2);
   
    %%%%%%%%%%%%%%%%%%%% Channel
   
    y = x + s + sigma*randn(1, ns);
    
    %%%%%%%%%%%%%%%%%%%% Decoder
    
    y1 = y * alpha + u;     % the mod operation is not be needed

    LaAcc = zeros(1, n_ch);
    LaCND = zeros(1, interleaver_size);
    LCND  = zeros(1, interleaver_size);
    
    LeAcc = zeros(1, n_ch);

    for iter = 1:NDecodingIter
        tic;
        fprintf(1,'Iter %d -------------------------------------------\n', iter);
        LAcc = Log_MAP(y1, q_VQ, LaAcc, gl1, gl2, q);
        
        cond = (LAcc==Inf & LaAcc==Inf) | (LAcc==-Inf & LaAcc==-Inf);
        LeAcc1 = LAcc - LaAcc;
        LeAcc(~cond) = LeAcc1(~cond);
        LeAcc(cond) = LAcc(cond);
        % We can argue in favor of this as follows:
        % In the belief propagation algorithm,
        % if there is a node that states for sure that the value of a
        % given bit is zero or one, then the other nodes can only agree,
        % they can trust this node without any further calculations.
                
        LeAcc=LeAcc(1:end-nt);   % because of the zero state termination
        
        fprintf(1,'MAP, Iter %d: %d errors in %d bits\n', iter, sum((LeAcc<0)~=c_CND), length(c_CND));

        % CNDs
        LeAcc=LeAcc(cnd_interlever_inv_perm);
        LCND(1:CND1) = LeAcc(1:CND1);
        LCND(CND1+1:3:end) = BoxPlus3(...
            LeAcc(CND1+1:end), ...
            LaCND(CND1+2:3:end), LaCND(CND1+3:3:end)...
            );
        LCND(CND1+2:3:end) = BoxPlus3(...
            LeAcc(CND1+1:end), LaCND(CND1+1:3:end),...
            LaCND(CND1+3:3:end)...
            );
        LCND(CND1+3:3:end) = BoxPlus3(...
            LeAcc(CND1+1:end), LaCND(CND1+1:3:end),...
            LaCND(CND1+2:3:end)...
            );
        LeCND = LCND;
        
        fprintf(1,'CND, Iter %d: %d errors in %d bits\n', iter, sum((LCND<0)~=c_interleaver),length(c_interleaver));
        fprintf(1,'Normalized information in LeCND: %f\n', NInfomation(LeCND));
        
        % Interleaver
        LaVND = LeCND(interlever_inv_perm);

        % VNDs
        LVND = zeros(1,interleaver_size);
        i0 = 1;
        for i=1:n_VND_dg
            degree = VND_dg(i);
            n_nodes = VND(i);
            
            L1 = reshape(LaVND(i0:i0+degree*n_nodes-1), degree, []); % groups the VNDs
            L2 = repmat(L1, degree,1);  % each VND has degree outputs
            L2(1:degree+1:end,:)=0;     % this one does not count since it is the message receiving edge
            L3 = sum(reshape(L2,degree, []));
            LVND(i0:i0+degree*n_nodes-1) = L3;
                        
            i0 = i0 + degree*n_nodes;
        end
        LeVND = LVND;
        
        fprintf(1,'VND, Iter %d: %d errors in %d bits\n', iter, sum((LVND<0)~=c_vnd), length(c_vnd));
        fprintf(1,'Normalized information in LeVND: %f\n', NInfomation(LeVND));

        % Interleaver
        LaCND = LeVND(interlever_perm);
        
        % CNDs
        LaAcc(1:CND1) = LaCND(1:CND1);
        LaAcc(CND1+1:end-nt) = BoxPlus3(...
            LaCND(CND1+1:3:end),...
            LaCND(CND1+2:3:end), LaCND(CND1+3:3:end)...
            );
        LaAcc(1:CND)=LaAcc(cnd_interlever_perm);
        toc;
    end
    
    LOUT = zeros(1,k);
    i0 = 1; i1=1;
    for i=1:n_VND_dg
        degree = VND_dg(i);
        n_nodes = VND(i);
        L1 = sum(reshape(LaVND(i1:i1+degree*n_nodes-1),degree, []));
        % sums the La values for each repetition node
        LOUT(i0:i0+n_nodes-1) = L1;
        i0 = i0+n_nodes;
        i1 = i1+degree*n_nodes;
    end
    
    fprintf('%d errors in %d bits\n', sum((LOUT<0)~=w),k);
end

% Normalized information
function I=NInfomation(LLR)
    P1 = 1./(1+exp(LLR));
    P0 = 1-P1;
    H=-P0.*log2(P0+1e-99)-P1.*log2(P1+1e-99);
    I=mean(1-H);
end
