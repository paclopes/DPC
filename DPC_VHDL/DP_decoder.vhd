-- DC_decoder.vhd 19-7-2019
-- Computes the decoded bit sequence given the pre-processes received signal y = (alpha*y0 + u) mod 4, where y0 is the receives signal and u is the dither signal.
-- The decoded bit sequence is the sign (most significant bit) of the bit log-likelihood output LV.
--
-- The component should be used as follows:
-- 1 - Write the internal y memory using the interface signals.
-- 2 - Activate the start signal (at '1') for on clock period.
-- 3 - Wait for the activation of the ended signal (at '1' for one clock period).
-- 4 - Read the internal LV memory (LMV) using the interface signals.
--
-- There is also an interface to allow reading and writting the internal interleaver memory (IM) for debugging.  
--
-- Paulo Lopes, INESC-ID

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utils.all;

entity DP_decoder is
    port (
        -- to write y
        y_address: in k_type;
        y_data: in y_pair_type;
        y_write: in std_logic; 

        -- to read and write IM
        IM_addr : in j_type;
        IM_data_in : in log_p_type;
        IM_data_out : out log_p_type;
        IM_write : in std_logic;
        IM_read : in std_logic;
        
        -- to read lvm
        LVM_addr_access: in l_type;
        LVM_data_out_access: out lv_type;
        LVM_read_access: in std_logic;
        
        qinv: in signed(bl_qinv-1 downto 0);
        start: in std_logic;
        ended: out std_logic;
        clock: in std_logic
    );
end DP_decoder;

architecture Behavioral of DP_decoder is
    constant total_iterations : integer := 20;   -- number of iterations of the BCJR algorithm

    -- The signals are named as the output of the components when they directly connect two components

    -- the actual interface with the Interleaver Memory (IM)
    signal IM_addr0 : j_type; -- input but several connections 
    signal IM_write0 : std_logic; -- input but several connections
    signal IM_read0 : std_logic; -- input but several connections
    signal IM_data_in0 : log_p_type;
    signal IM_data_out0 : log_p_type;
    signal IM_addr1 : j_type; -- input but several connections 
    signal IM_write1 : std_logic; -- input but several connections
    signal IM_read1 : std_logic; -- input but several connections
    signal IM_data_in1 : log_p_type;
    signal IM_data_out1 : log_p_type;
    
    -- beta memory interface
    signal beta_data_out: beta_data_type;
    
    -- the actual y memory interface
    signal y_read_addr: k_type; -- input but several connections 
    signal y_read0: std_logic; -- input but several connections
    signal y_output_data0: y_pair_type;
    
    -- the lvm memory interface
    signal LVM_addr: l_type;
    signal LVM_read: std_logic;
    signal LVM_data_out: lv_type;
    
    -- boxplus1_beta
    signal boxplus1_beta_start: std_logic;
    signal boxplus1_beta_start_output: std_logic;
    signal boxplus1_beta_llr_output: llr_data_type;
    signal boxplus1_beta_IM_address: j_type;
    signal boxplus1_beta_IM_read: std_logic; -- aux

    -- boxplus1
    signal boxplus1_start_output: std_logic;
    signal boxplus1_llr_output: llr_data_type;
    signal boxplus1_IM_address: j_type;
    signal boxplus1_IM_read: std_logic; -- aux
        
    -- boxplus2
    signal boxplus2_IM_data_in: log_p_type;
    signal boxplus2_IM_address: j_type;
    signal boxplus2_IM_write: std_logic;
    signal boxplus2_IM_read: std_logic;
    signal boxplus2_ended: std_logic;
    
    -- log_map_beta
    signal log_map_beta_dot_address: k_type;
    signal log_map_beta_dot_read: std_logic;
    signal log_map_beta_beta_banks_address: beta_address_type;
    signal log_map_beta_beta_banks_data: beta_data_type;
    signal log_map_beta_beta_banks_write: std_logic;
    signal log_map_beta_ended: std_logic;
    
    -- log_map
    signal log_map_input_data_address: k_type;
    signal log_map_input_data_read: std_logic;
    signal log_map_beta_address: beta_address_type;
    signal log_map_beta_read: std_logic;
    signal log_map_llr_output_address: k_type;
    signal log_map_llr_output_data: llr_data_type;
    signal log_map_llr_output_write: std_logic;
    signal log_map_ended: std_logic;
    signal log_map_start: std_logic;
    
    -- interleaver
    signal interleaver_IM_addr0: j_type;
    signal interleaver_IM_data_in0 : log_p_type;
    signal interleaver_IM_write0 : std_logic;
    signal interleaver_IM_read0 : std_logic;
    signal interleaver_IM_addr1: j_type;
    signal interleaver_IM_data_in1 : log_p_type;
    signal interleaver_IM_write1 : std_logic;
    signal interleaver_IM_read1 : std_logic;
    signal interleaver_ended: std_logic;
    signal interleaver_start: std_logic;
    
    -- vnds
    signal vnds_IM_addr0: j_type;
    signal vnds_IM_data_in0 : log_p_type;
    signal vnds_IM_write0 : std_logic;
    signal vnds_IM_read0 : std_logic;
    signal vnds_LVM_addr: l_type;
    signal vnds_LVM_data_in : lv_type;
    signal vnds_LVM_write : std_logic;
    signal vnds_LVM_read : std_logic;
    signal vnds_ended: std_logic;
    signal vnds_start: std_logic;

    -- aux
    signal running_initialization_or_ending: std_logic := '1';
    signal running_log_map_beta: std_logic := '0';
    signal running_log_map: std_logic := '0';
    signal running_interleaver1: std_logic := '0';
    signal running_vnds: std_logic := '0';
    signal running_interleaver2: std_logic := '0';
    signal running_interleaver: std_logic;
    constant bl_iteration : integer := length(total_iterations - 1);
    subtype iteration_type is unsigned(bl_iteration-1 downto 0);
    signal iteration: iteration_type;
    
begin
    running_interleaver <= running_interleaver1 or running_interleaver2;
    IM_data_out <= IM_data_out0;

    -- running log_map_beta or log_map?
    process(clock)
    begin
        if rising_edge(clock) then
            if start = '1' then
                running_log_map_beta <= '1';
                running_initialization_or_ending <= '0';
                iteration <= to_unsigned(0, bl_iteration);
                report "Starting log map beta.";
            elsif log_map_beta_ended = '1' then
                running_log_map_beta <= '0';
                running_log_map <= '1';
                report "Starting log map.";
            elsif boxplus2_ended = '1' then
                running_log_map <= '0';
                running_interleaver1 <= '1';
                report "Starting interleaver.";
            elsif interleaver_ended = '1' and running_interleaver1 = '1' then
                running_interleaver1 <= '0';
                running_vnds <= '1';
                report "Starting variable nodes.";
            elsif vnds_ended = '1' then
                running_vnds <= '0';
                running_interleaver2 <= '1';
                report "Starting interleaver2.";
            elsif interleaver_ended = '1' and running_interleaver2 = '1' then
                if iteration < total_iterations - 1 then
                    running_interleaver2 <= '0';
                    running_log_map_beta <= '1';
                    iteration <= iteration + 1;
                    report "Fininshed iteration " & integer'image(to_integer(iteration)) & ".";
                    report "Starting log map beta.";
                else
                    running_interleaver2 <= '0';
                    running_initialization_or_ending <= '1';
                    report "Fininshed all the iterations.";
                 end if;
            end if;

        end if;
    end process;

    y_read_addr <= log_map_beta_dot_address when log_map_beta_dot_read = '1' else log_map_input_data_address;
    y_read0 <= log_map_beta_dot_read or log_map_input_data_read;
            
    gen1: for bank in 0 to n_group_samples-1 generate
        y_bank: entity work.BRAM1 generic map (data_size => tau, word_length=>bl_y)
            port map (read_addr => y_read_addr, write_addr => y_address, data_in => y_data(bank), data_out => y_output_data0(bank), write => y_write, read => y_read0, clock => clock);
    end generate gen1;

    gen3: for bank in 0 to n_states-1 generate
        beta_bank: entity work.BRAM1 generic map (data_size => tau, word_length=>bl_log_p) 
            port map (read_addr => log_map_beta_address, write_addr => log_map_beta_beta_banks_address, data_in => log_map_beta_beta_banks_data(bank), data_out => beta_data_out(bank), write => log_map_beta_beta_banks_write, read => log_map_beta_read, clock => clock);
    end generate gen3;
    
    IM_addr0 <= IM_addr when running_initialization_or_ending = '1' else
                boxplus1_beta_IM_address when  running_log_map_beta = '1' else
                boxplus1_IM_address when running_log_map = '1' else
                interleaver_IM_addr0 when running_interleaver = '1' else
                vnds_IM_addr0;
                
    IM_data_in0 <= IM_data_in when running_initialization_or_ending = '1' else
                interleaver_IM_data_in0 when running_interleaver = '1' else
                vnds_IM_data_in0;
    
    IM_write0 <= IM_write or interleaver_IM_write0 or vnds_IM_write0;
    
    boxplus1_beta_IM_read <= running_log_map_beta; -- consider changing boxplus
    boxplus1_IM_read <= running_log_map; -- consider changing boxplus
    IM_read0 <= IM_read or boxplus1_beta_IM_read or boxplus1_IM_read 
        or interleaver_IM_read0 or vnds_IM_read0;
                
    IM_addr1 <= boxplus2_IM_address when running_log_map = '1' else
                interleaver_IM_addr1;
                
    IM_data_in1 <= boxplus2_IM_data_in when running_log_map = '1' else
                interleaver_IM_data_in1;
    
    IM_write1 <= boxplus2_IM_write or interleaver_IM_write1;
    IM_read1 <= boxplus2_IM_read or interleaver_IM_read1;
    
    im0: entity work.interleaver_memory1
    port map(
        addr0 => IM_addr0, 
        data_in0 => IM_data_in0,
        data_out0 => IM_data_out0,
        write0 => IM_write0,
        read0 => IM_read0,
        addr1 => IM_addr1,
        data_in1 => IM_data_in1,
        data_out1 => IM_data_out1,
        write1 => IM_write1,
        read1 => IM_read1,
        clock => clock
    );
    
    boxplus1_beta_start <= '1' when start = '1' or 
        (interleaver_ended = '1' and running_interleaver2 = '1' and iteration < total_iterations - 1)
        else '0';
    
    bp1b: entity work.boxplus1_beta port map(
        start_input => boxplus1_beta_start, 
        start_output => boxplus1_beta_start_output,
        L => boxplus1_beta_llr_output, 
        IM_data_out => IM_data_out0, 
        IM_address => boxplus1_beta_IM_address, 
        clock => clock
    );

    log_map_beta0: entity work.log_map_beta port map(
        address => log_map_beta_dot_address,
        y_banks_data => y_output_data0,
        llr_banks_data => boxplus1_beta_llr_output,
        read => log_map_beta_dot_read,

        qinv => qinv,
    
        beta_banks_address => log_map_beta_beta_banks_address,
        beta_banks_data => log_map_beta_beta_banks_data,
        beta_banks_write => log_map_beta_beta_banks_write,

        start => boxplus1_beta_start_output,
        ended => log_map_beta_ended,
        clock => clock
    );

    bp1: entity work.boxplus1 port map(
        start_input => log_map_beta_ended,
        start_output => boxplus1_start_output,
        L => boxplus1_llr_output, 
        IM_data_out => IM_data_out0, 
        IM_address => boxplus1_IM_address, 
        clock => clock
    );
    
    log_map0: entity work.log_map port map(
        input_data_address => log_map_input_data_address,
        y_data => y_output_data0,
        llr_input_data => boxplus1_llr_output,
        input_data_read => log_map_input_data_read,

        qinv => qinv,
        
        beta_address => log_map_beta_address,
        beta_data => beta_data_out,
        beta_read => log_map_beta_read,
        
        llr_output_address => log_map_llr_output_address,
        llr_output_data => log_map_llr_output_data,
        llr_output_write => log_map_llr_output_write,

        start => boxplus1_start_output,
        ended => log_map_ended,
        clock => clock);

    bp2: entity work.boxplus2 port map(
        ended_input => log_map_ended, 
        ended_output => boxplus2_ended,
        start => boxplus1_start_output,
        L => log_map_llr_output_data,
        LW => log_map_llr_output_write,
        LA => boxplus1_llr_output,
        IM_data_in => boxplus2_IM_data_in,
        IM_data_out => IM_data_out1,
        IM_address => boxplus2_IM_address,
        IM_write => boxplus2_IM_write,
        IM_read => boxplus2_IM_read,
        clock => clock
    );
    
    interleaver_start <= boxplus2_ended or vnds_ended;
    
    i0: entity work.interleaver1
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
    
    vnds_start <= interleaver_ended and running_interleaver1;
    
    v0: entity work.vnds port map (
        IM_addr0 => vnds_IM_addr0,
        IM_data_in0 => vnds_IM_data_in0,
        IM_data_out0 => IM_data_out0,
        IM_write0 => vnds_IM_write0,
        IM_read0 => vnds_IM_read0,
        LVM_addr => vnds_LVM_addr,
        LVM_data_in => vnds_LVM_data_in,
        LVM_data_out => LVM_data_out,
        LVM_write => vnds_LVM_write,
        LVM_read => vnds_LVM_read,
        start => vnds_start,
        ended => vnds_ended,
        clock => clock
    );

    LVM_addr <= LVM_addr_access when running_initialization_or_ending = '1' else vnds_LVM_addr;
    LVM_data_out_access <= LVM_data_out;
    LVM_read <= vnds_LVM_read or LVM_read_access;
    
    lvm0: entity work.BRAM generic map (size => n_bits, word_length => lv_type'length)
    port map (addr => LVM_addr,
           data_in => std_logic_vector(vnds_LVM_data_in),
           lv_type(data_out) => LVM_data_out,
           write => vnds_LVM_write,
           read => LVM_read,
           clock => clock
    );

    ended <= '1' when interleaver_ended = '1' and running_interleaver2 = '1' and iteration >= total_iterations - 1 else '0';

end Behavioral;