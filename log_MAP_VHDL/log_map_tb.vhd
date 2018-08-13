library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use work.utils.all;
use work.some_constants.all;

entity log_map_tb is
--  Port ( );
end log_map_tb;

architecture Behavioral of log_map_tb is
    file in_file : TEXT open READ_MODE is "log_map_input_llr.txt";
    file out_file : TEXT open WRITE_MODE is "log_map_output_llr.txt";
    shared variable str: line;
    
    signal write_address: log_map_address_type;
    signal y_banks_data_in: y_data_type;
    signal y_write: std_logic := '0'; 
    signal llr_banks_data_in: llr_data_type;
    signal llr_write: std_logic := '0';
    signal read_address: log_map_address_type;
    signal llr_banks_data_out: llr_data_type;
    signal llr_read: std_logic := '0';
    signal qinv: signed(bl_qinv-1 downto 0);
    signal start: std_logic := '0';
    signal ended: std_logic := '0';
    signal clock: std_logic := '0';
    signal clock_enable : std_logic := '1';

begin

    process
    begin
        if clock_enable='1' then
            clock <= not clock;
            wait for 5 ns;
        else
            wait; 
        end if;
    end process;
    
    DPC1: entity work.log_map port map (
        input_data_address => write_address,
        y_data => y_banks_data_in,
        y_write => y_write,   
        llr_data => llr_banks_data_in, 
        llr_write => llr_write, 
        
        llr_ouput_address => read_address,
        llr_output_data => llr_banks_data_out,
        llr_output_read => llr_read,

        qinv => qinv, 
        start => start,
        ended => ended,
        clock => clock
    );
    
    process
        variable aux_p: log_p_type;
        variable aux_y: y_type;
    begin
    
        report "Running with values: bl_log_p: " & integer'image(bl_log_p) & " bl_y: " & integer'image(bl_y); 

        -- read y
        wait until rising_edge(clock);
        y_write <= '1';
        readline(in_file, str);
        for n in 0 to n_samples/2-1 loop
            write_address <= to_unsigned(n, length(tau-1));
            readline(in_file, str);
            read(str, aux_y);
            y_banks_data_in(0) <= unsigned(aux_y);
            wait for 1 ps;
            readline(in_file, str);
            read(str, aux_y);
            y_banks_data_in(1) <= unsigned(aux_y);
            wait until rising_edge(clock);
        end loop;
        y_write <= '0';

        -- read q
        readline(in_file, str);
        readline(in_file, str);
        read(str, aux_y);
        qinv <= aux_y;
        
        -- read llr
        wait until rising_edge(clock);
        llr_write <= '1';
        readline(in_file, str);
        for n in 0 to n_acc_bits/3-1 loop
            write_address <= to_unsigned(n, length(tau-1));
            readline(in_file, str);
            read(str, aux_p);
            llr_banks_data_in(0) <= unsigned(aux_p);
            wait for 1 ps;
            readline(in_file, str);
            read(str, aux_p);
            llr_banks_data_in(1) <= unsigned(aux_p);
            wait for 1 ps;
            readline(in_file, str);
            read(str, aux_p);
            llr_banks_data_in(2) <= unsigned(aux_p);
            wait until rising_edge(clock);
        end loop;
        llr_write <= '0';
        
        -- simulation
        wait until rising_edge(clock);
        start <= '1';
        wait until rising_edge(clock);
        start <= '0';
        wait until rising_edge(clock) and ended='1';
        
        -- write results
        str := "";
        wait for 1 ps;

        llr_read <= '1';
        for k in 0 to tau-1 loop
            wait for 1 ps;
            read_address <= to_unsigned(k, length(tau-1));
            wait until rising_edge(clock);
            wait for 1 ps;
            
            for i in 0 to n_input_bits-1 loop
                write(str, string'("k: ")); write(str, k); write(str, string'(" i: ")); write(str, i);
                write(str, string'(" llr: ")); write(str, signed(llr_banks_data_out(i)));
                writeline(out_file, str);
                wait for 1 ps;
            end loop;
        end loop;
        
        report "Finished!";
        clock_enable <= '0';    
        wait;
    end process;

end Behavioral;