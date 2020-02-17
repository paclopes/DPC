library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity interleaver_memory1 is
    Port ( addr0: in j_type;
           data_in0 : in log_p_type;
           data_out0 : out log_p_type;
           write0 : in std_logic;
           read0 : in std_logic;
           addr1: in j_type;
           data_in1 : in log_p_type;
           data_out1 : out log_p_type;
           write1 : in std_logic;
           read1 : in std_logic;
           clock : in std_logic);
end interleaver_memory1;

architecture Behavioral of interleaver_memory1 is
    type ram_type is array (0 to interleaver_length-1) of log_p_type;
    shared variable ram: ram_type;
begin
    process(clock)
    begin
        if rising_edge(clock) then
            if read0 = '1' then
                data_out0 <= ram(to_integer(addr0));
            end if;

            if write0 = '1' then 
                ram(to_integer(addr0)) := data_in0;
            end if;
        end if;

    end process;

    process(clock)
    begin
        if rising_edge(clock) then
            if read1 = '1' then
                data_out1 <= ram(to_integer(addr1));
            end if;

            if write1='1' then 
                ram(to_integer(addr1)) := data_in1;
            end if;
        end if;

    end process;

end Behavioral;