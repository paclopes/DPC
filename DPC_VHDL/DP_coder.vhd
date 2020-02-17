-- DC_coder
--
-- Computes the output signal x to be sent to the channel given the uncoded bit sequence w and the pre-processed interference signal s'=alpha*s mod 4,
-- where s is the interference signal.
-- The component should be used as follows:
-- 1 - Write the internal w and s memories using the interface signals.
-- 2 - Activate the start signal (at '1') for on clock period.
-- 3 - Wait for the activation of the ended signal (at '1' for one clock period).
-- 4 - Read the internal x memory using the interface signals.
--
-- Paulo Lopes, INESC-ID

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utils.all;

entity DP_coder is
    port (
        -- access to input data memory w
        w_addr_access: in l_type;
        w_data_in_access: in std_logic;
        w_data_out_access: out std_logic;
        w_write_access: in std_logic;
        w_read_access: in std_logic;
        
        -- access to interference memory s
        -- : (alpha*s) mod 4
        s_addr_access: in k_type;
        s_data_in_access: in x_pair_type;
        s_write_access: in std_logic;
        s_read_access: in std_logic;

        -- access to memory x
        x_addr_access: in k_type;
        x_data_out_access: out x_pair_type;
        x_read_access: in std_logic; 
    
        start: in std_logic;
        ended: out std_logic;
        clock: in std_logic
    );
end DP_coder;

architecture Behavioral of DP_coder is
    -- The signals are named as the output of the components when they directly connect two components

    -- input data memory w
    signal w_addr: l_type;
    signal w_data_out: std_logic;
    signal w_read : std_logic;

    -- memory x
    signal x_data_in: x_pair_type;
    signal x_data_out: x_pair_type;
    signal x_addr: k_type; 
    signal x_write: std_logic_vector(0 to 1);
    signal x_read: std_logic;

    -- interleaver
    signal interleaver_IM_addr0: j_type;
    signal interleaver_IM_data_in0 : std_logic;
    signal interleaver_IM_write0 : std_logic := '0';
    signal interleaver_IM_read0 : std_logic := '0';
    signal interleaver_IM_addr1: j_type;
    signal interleaver_IM_data_in1 : std_logic;
    signal interleaver_IM_write1 : std_logic := '0';
    signal interleaver_IM_read1 : std_logic := '0';
    signal interleaver_ended: std_logic;
    signal interleaver_start: std_logic;

    -- interleaver memory (IM)
    signal IM_addr0: j_type;
    signal IM_data_in0 : std_logic;
    signal IM_write0 : std_logic := '0';
    signal IM_read0 : std_logic := '0';
    signal IM_data_out0 : std_logic;
    signal IM_data_out1 : std_logic;
    
    -- s_x_alpha memory
    signal s_addr: k_type;
    signal s_data_out: x_pair_type;
    signal s_read: std_logic;
    
    -- auxs
    signal running_vnds, running_vnds_2 : std_logic := '0';
    signal running_cnds, running_cnds_2, running_cnds_3, running_cnds_4, running_cnds_5: std_logic := '0';
    signal ended_cnds_1, ended_cnds_2, ended_cnds_3, ended_cnds_4, ended_cnds_5: std_logic := '0';
    signal running_zeros: std_logic := '0';
    signal running_viterbi: std_logic := '0';
    signal ended_i: std_logic := '0';
    
    -- addresses
    signal l: l_type;                   -- input data memory w address
    signal j: j_type;                   -- interleaver memory address
    signal k, k4, k5: k_type;           -- output memory y address (and s memory address)

    -- vnds
    signal i: unsigned(bl_vnd_dg-1 downto 0); -- the position inside a vnd
    constant bl_i_vnd : integer := length(VND(0));
    signal i_vnd: unsigned(bl_i_vnd-1 downto 0); -- the index number of the vnd of a given degree
    constant  bl_i_vnd_type : integer := length(n_VND_dg); 
    signal i_vnd_type: unsigned(bl_i_vnd_type-1 downto 0); -- the index number of the type of vnd (degree)
    signal vnds_w_read : std_logic := '0';

    -- cnds
    signal c: unsigned(1 downto 0);     -- counter 
    signal a, a3: std_logic;            -- accumulator
    signal is_cnd3: std_logic;
    signal first: std_logic;

    -- accumulator
    signal acc: std_logic := '0';       -- accumulator

    -- 4-PAM : 2*acc(n-1) + acc(n) 
    signal d, d4, d5: unsigned(1 downto 0);     -- counter 
    signal acc_d: std_logic;
    signal bank, bank4, bank5: unsigned(0 downto 0);
    
    -- dither and interference cancelation
    signal v, s_x_alpha, u: x_type;
    signal psrn: unsigned(bl_psrn-1 downto 0);  -- pesudo random number
    
    -- viterbi
    signal viterbi_x_data_in: x_pair_type;
    signal viterbi_x_addr: k_type;
    signal viterbi_x_read: std_logic;
    signal viterbi_x_write: std_logic;
	signal viterbi_start: std_logic;

begin
   
    ended <= ended_i;
        
    process(clock)
    begin
        if rising_edge(clock) then
        
            ----------- vnds --
            
            if start = '1' then
                running_vnds <= '1';
                j <= to_unsigned(0, bl_j);
                l <= to_unsigned(0, bl_l);
                i <= to_unsigned(0, bl_vnd_dg);
                i_vnd <= to_unsigned(0, bl_i_vnd);
                i_vnd_type <= to_unsigned(0, bl_i_vnd_type);
                psrn <= to_unsigned(random_number_seed, psrn'length);
                vnds_w_read <= '1';
            else
                if vnds_w_read = '1' then
                    vnds_w_read <= '0';
                end if;
                
                -- pipeline:
                -- stage 1 : read and incremente l
                
                if running_vnds = '1' then
                    if i < VND_dg(to_integer(i_vnd_type)) - 1 then
                        i <= i + 1;
                    else
                        l <= l + 1;
                        vnds_w_read <= '1';
        
                        if i_vnd < VND(to_integer(i_vnd_type)) - 1 then 
                            i_vnd <= i_vnd + 1;
                        else
                            if i_vnd_type < n_VND_dg - 1 then
                                i_vnd_type <= i_vnd_type + 1;
                            else
                                running_vnds <= '0';
                                interleaver_start <= '1';
                            end if; 
                            i_vnd <= to_unsigned(0, bl_i_vnd);
                        end if;
    
                        i <= to_unsigned(0, bl_vnd_dg);
                    end if;
                end if;
                
                if interleaver_start = '1' then
                    interleaver_start <= '0';
                end if;
                
                running_vnds_2 <= running_vnds;
                
                -- stage 2 : increment j and write at j
                
                if running_vnds_2 = '1' then
                    j <= j + 1;
                end if;
            end if;
            
            ----------- cnds --           

            if interleaver_ended = '1' then
                running_cnds <= '1';
                a <= '0';
                c <= to_unsigned(0, 2);
                j <= to_unsigned(0, bl_j);
                k <= to_unsigned(0, bl_k);
                d <= to_unsigned(0, 2);
                bank <= to_unsigned(0, 1);
            else
                -- pipeline
                running_cnds_2 <= running_cnds;
                
                -- stage 1: read IM and calculate j
                if running_cnds = '1' then
                   if j < 3*cnd3 then 
                        is_cnd3 <= '1'; -- is_cnd3 is used in pipeline stage 2
                    else 
                        is_cnd3 <= '0'; -- is_cnd3 is used in pipeline stage 2
                    end if;
                
                    if j < interleaver_length - 1 then
                        j <= j + 1;
                    else
                        running_cnds <= '0';
                        ended_cnds_1 <= '1';
                    end if;
                    
                end if;
                
                if ended_cnds_1 = '1' then
                    ended_cnds_1 <= '0';
                end if;

                ended_cnds_2 <= ended_cnds_1;
                
                -- stage 2: (load a) / (accumulate to a) and calculate c
                if running_cnds_2 = '1' then
                    if c = 0 then
                        a <= IM_data_out0;
                    else
                        a <= a xor IM_data_out0;
                    end if;
                    
                    if is_cnd3 = '1' then
                        if c < 2 then
                            c <= c + 1;
                            running_cnds_3 <= '0';
                        else
                            c <= to_unsigned(0, 2);
                            running_cnds_3 <= '1';
                        end if;
                    end if;
                    assert c = 0 or is_cnd3 = '1';
                else
                    running_cnds_3 <= '0';                   
                end if;
                
                ended_cnds_3 <= ended_cnds_2;

                -- stage 3: accumulate to acc, calculate d, k, bank and read s_x_alpha memory
                if running_cnds_3 = '1' then
                    acc <= acc xor a;
                    if d < 2 then
                        d <= d + 1;
                    else
                        d <= to_unsigned(0, 2);
                    end if;
                    
                    if d /= 0 then
                        bank <= bank + 1;
                        if bank = 1 then
                            k <= k + 1;
                        end if;
                    end if;
                end if;

                d4 <= d;
                ended_cnds_4 <= ended_cnds_3;
                running_cnds_4 <= running_cnds_3;
                bank4 <= bank; 
                k4 <= k;
    
                -- stage 4: add dither and intereference cancelation
                if running_cnds_4 = '1' then
                    
                    if d4 = 0 then
                        acc_d <= acc;
                    else
                        psrn <= psrn(14 downto 0) & (psrn(15) xor psrn(14) xor psrn(12) xor psrn(3));
                        v <= not acc_d & acc & '1' & to_signed(0, bl_x-3) -- subtract 1.5 from acc_d & acc & "00..."
                            - s_x_alpha - u;  -- cancel intereference and dither
                    end if;
                end if;
                
                d5 <= d4;
                ended_cnds_5 <= ended_cnds_4;
                running_cnds_5 <= running_cnds_4;
                bank5 <= bank4; 
                k5 <= k4;
    
                -- stage 5: store
                -- if running_cnds_5 = '1' then
                -- end if;
                
            end if;
            
            if ended_cnds_5 = '1' then
                running_zeros <= '1';
                acc <= '0';
                acc_d <= '0';
            else
                if running_zeros = '1' then 
                    if k < tau - 1 then
                        k <= k + 1;
                    else
                        viterbi_start <= '1';
                        running_zeros <= '0';
                    end if;
                end if;
            end if;
            
            if viterbi_start = '1' then
                viterbi_start <= '0';
                running_viterbi <= '1';
            end if;
            
            if ended_i = '1' then
                running_viterbi <= '0';
            end if;
            
        end if;
    end process;
        
    i0: entity work.interleaver
    port map (
        IM_addr0 => interleaver_IM_addr0,
        IM_data_in0 => interleaver_IM_data_in0,
        IM_data_out0 => IM_data_out0,
        IM_write0 => interleaver_IM_write0,
        IM_read0 => interleaver_IM_read0,
        IM_addr1 => interleaver_IM_addr1,
        IM_data_in1 => interleaver_IM_data_in1,
        IM_data_out1 => IM_data_out1,
        IM_write1 => interleaver_IM_write1,
        IM_read1 => interleaver_IM_read1,
        start => interleaver_start,
        ended => interleaver_ended,
        clock => clock
    );

    w_addr <= l when running_vnds = '1' else w_addr_access;
    w_read <= w_read_access or vnds_w_read;
    w_data_out_access <= w_data_out;
    w0: entity work.BRAM2
    generic map (size => n_bits)
    port map(
        addr => w_addr,
        data_in => w_data_in_access,
        data_out => w_data_out,
        write => w_write_access,
        read => w_read,
        clock => clock
    );

    IM_addr0 <= j when running_vnds_2 = '1' or running_cnds = '1' else interleaver_IM_addr0;
    IM_data_in0 <= w_data_out when running_vnds_2 = '1' else interleaver_IM_data_in0;
    IM_write0 <= running_vnds_2 or interleaver_IM_write0;
    IM_read0 <= running_cnds or interleaver_IM_read0;

    im0: entity work.interleaver_memory
--    generic map(data_type => std_logic)
    port map(
        addr0 => IM_addr0, 
        data_in0 => IM_data_in0,
        data_out0 => IM_data_out0,
        write0 => IM_write0,
        read0 => IM_read0,
        addr1 => interleaver_IM_addr1,
        data_in1 => interleaver_IM_data_in1,
        data_out1 => IM_data_out1,
        write1 => interleaver_IM_write1,
        read1 => interleaver_IM_read1,
        clock => clock
    );

    x_addr <= k5 when running_cnds_5 = '1' else
              k when running_zeros = '1' else  
              viterbi_x_addr when running_viterbi = '1' else
              x_addr_access;
              
    x_data_out_access <= x_data_out;
    x_read <= x_read_access or viterbi_x_read;
    
    gen1: for bk in 0 to n_group_samples-1 generate
        x_data_in(bk) <= viterbi_x_data_in(bk) when running_viterbi = '1' 
            else v;
        x_write(bk) <= '1' when 
            (bank5 = bk and d5 /= 0 and running_cnds_5 = '1') or
            (viterbi_x_write = '1' and running_viterbi = '1') or
            running_zeros = '1'
            else '0';
        x0: entity work.BRAM3
        generic map (size => tau, word_length => bl_y)
        port map(
            addr => x_addr,
            data_in => std_logic_vector(x_data_in(bk)),
            x_type(data_out) => x_data_out(bk),
            write => x_write(bk),
            read => x_read,
            clock => clock
        );
    end generate gen1;
    
    s_addr <= k when running_cnds_3 = '1' else s_addr_access;
    s_x_alpha <= s_data_out(to_integer(bank4));
    s_read <= '1' when (d=1 and running_cnds_3 = '1') or s_read_access = '1' else '0';
    u <= x_type(psrn(u'length-1 downto 0));
    
    gen2: for bk in 0 to n_group_samples-1 generate
        s0: entity work.BRAM3
        generic map (size => tau, word_length => bl_x)
        port map(
            addr => s_addr,
            data_in => std_logic_vector(s_data_in_access(bk)),
            x_type(data_out) => s_data_out(bk),
            write => s_write_access,
            read => s_read,
            clock => clock
        );
    end generate gen2;
    
    v0: entity work.viterbi generic map(
        data_size => 2*tau,
		generator1 => g1,
		generator2 => g2)
    port map (
        x_data_in => viterbi_x_data_in,
        x_data_out => x_data_out,
        x_addr => viterbi_x_addr,
        x_read => viterbi_x_read,
        x_write => viterbi_x_write,
		start => viterbi_start,
		ended => ended_i,
		clock => clock
		);          
    
end Behavioral;