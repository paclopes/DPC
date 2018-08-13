-- DCP.vhd 2-7-2018
-- Integrates the calculation of beta and LLR.
-- This version uses: 6626 = 12.45 % LUT
-- Paulo Lopes

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utils.all;
use work.some_constants.all;

entity DPC is
    port (
        -- to write llr and y and beta
        input_data_address: in log_map_address_type;
        y_data: in y_data_type;
        llr_data: in llr_data_type;
        y_write: in std_logic; 
        llr_write: in std_logic;
        -- to read the output llr
        llr_ouput_address: in log_map_address_type;
        llr_output_data: out llr_data_type;
        llr_output_read: in std_logic;
        
        qinv: in signed(bl_qinv-1 downto 0);
        start: in std_logic;
        ended: out std_logic;
        clock: in std_logic
    );
end DPC;

architecture Behavioral of DPC is
    signal input_data_read_address: log_map_address_type;
    signal beta_input_data_read_address: log_map_address_type;
    signal log_map_input_data_read_address: log_map_address_type;
    signal y_data_in: y_data_type;
    signal llr_input_data_in: llr_data_type;
    signal input_data_read: std_logic;
    signal beta_input_data_read: std_logic;
    signal log_map_input_data_read: std_logic;
    
    signal beta_read_address: beta_address_type;
    signal beta_write_address: beta_address_type;
    signal beta_data: beta_data_type;
    signal beta_data_out: beta_data_type;
    signal beta_write: std_logic;
    signal beta_read: std_logic;
    
    signal llr_output_write_address: log_map_address_type;
    signal llr_output_data_in: llr_data_type;
    signal llr_output_write: std_logic;
    
    signal beta_end: std_logic;
begin

    log_map_beta0: entity work.log_map_beta port map(
        address => beta_input_data_read_address,
        y_banks_data => y_data_in,
        llr_banks_data => llr_input_data_in,
        read => beta_input_data_read,

        qinv => qinv,
    
        beta_banks_address => beta_write_address,
        beta_banks_data => beta_data,
        beta_banks_write => beta_write,

        start => start,
        ended => beta_end,
        clock => clock
    );

    log_map0: entity work.log_map_alpha port map(
        input_data_address => log_map_input_data_read_address,
        y_data => y_data_in,
        llr_input_data => llr_input_data_in,
        input_data_read => log_map_input_data_read,

        qinv => qinv,
        
        beta_address => beta_read_address,
        beta_data => beta_data_out,
        beta_read => beta_read,
        
        llr_output_address => llr_output_write_address,
        llr_output_data => llr_output_data_in,
        llr_output_write => llr_output_write,

        start => beta_end,
        ended => ended,
        clock => clock);
        
    input_data_read_address <= beta_input_data_read_address when beta_input_data_read = '1' else
        log_map_input_data_read_address;
    
    input_data_read <= beta_input_data_read or log_map_input_data_read;

    gen1: for bank in 0 to n_samples_group-1 generate
        y_bank: entity work.BRAM1 generic map (data_size => tau, word_length=>bl_y)
            port map (read_addr => input_data_read_address, write_addr => input_data_address, data_in => y_data(bank), data_out => y_data_in(bank), write => y_write, read => input_data_read, clock => clock);
    end generate gen1;

    gen2: for bank in 0 to n_input_bits-1 generate
        llr_bank: entity work.BRAM1 generic map (data_size => tau, word_length=>bl_log_p) 
            port map (read_addr => input_data_read_address, write_addr => input_data_address, data_in => llr_data(bank), data_out => llr_input_data_in(bank), write => llr_write, read => input_data_read, clock => clock);
    end generate gen2;

    gen3: for bank in 0 to n_states-1 generate
        beta_bank: entity work.BRAM1 generic map (data_size => tau, word_length=>bl_log_p) 
            port map (read_addr => beta_read_address, write_addr => beta_write_address, data_in => beta_data(bank), data_out => beta_data_out(bank), write => beta_write, read => beta_read, clock => clock);
    end generate gen3;
    
    gen4: for bank in 0 to n_input_bits-1 generate
        llr_output_bank: entity work.BRAM1 generic map (data_size => tau, word_length=>bl_log_p)
            port map (read_addr => llr_ouput_address, write_addr => llr_output_write_address, data_in => llr_output_data_in(bank), data_out => llr_output_data(bank), write => llr_output_write, read => llr_output_read, clock => clock);
    end generate gen4;

end Behavioral;