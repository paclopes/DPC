library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity interleaver is
    Port ( IM_addr0: out j_type;
           IM_data_in0 : out std_logic;
           IM_data_out0 : in std_logic;
           IM_write0 : out std_logic;
           IM_read0 : out std_logic;
           IM_addr1: out j_type;
           IM_data_in1 : out std_logic;
           IM_data_out1 : in std_logic;
           IM_write1 : out std_logic;
           IM_read1 : out std_logic;
           start: in std_logic;
           ended: out std_logic;
           clock : in std_logic);
end interleaver;

architecture Behavioral of interleaver is
    constant k : integer := 127;
    constant h : integer := interleaver_length/2;
    signal running, ended_i : std_logic := '0';
    signal c, cd, b, n: j_type;
    signal n_is_zero: std_logic;
    signal write: std_logic;
begin
    ended <= ended_i;
    
    IM_addr0 <= cd + h;
    IM_addr1 <= c;
    
    IM_data_in0 <= IM_data_out1; 
    IM_data_in1 <= IM_data_out0;
    
    IM_read0 <= running and not n_is_zero and not write;
    IM_read1 <= running and not n_is_zero and not write;
    IM_write0 <= running and not n_is_zero and write;
    IM_write1 <= running and not n_is_zero and write;

    process(clock)
    begin
        if rising_edge(clock) then
            if start = '1' then
                c <= to_unsigned(0, bl_j);
                b <= to_unsigned(k, bl_j);
                n <= to_unsigned(0, bl_j);
                write <= '0';
                n_is_zero <= '1';
                running <= '1';
            end if;
            
            if running = '1' then
                if write = '1' then
                    b <= b + k;
                    c <= c + b;
                    cd <= c;
                                   
                    if n_is_zero = '1' then
                        n_is_zero <= '0';
                    end if;               
                                    
                    n <= n + 1;
                    if n = interleaver_length/2 then
                        running <= '0';
                        ended_i <= '1';
                    end if;
                end if;
                write <= not write;
            end if;
            
            if ended_i = '1' then
                ended_i <= '0';
            end if;
                
        end if;
    end process;

end Behavioral;