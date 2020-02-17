library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity interleaver_memory is
--    Generic (type std_logic);
    Port ( addr0: in j_type;
           data_in0 : in std_logic;
           data_out0 : out std_logic;
           write0 : in std_logic;
           read0 : in std_logic;
           addr1: in j_type;
           data_in1 : in std_logic;
           data_out1 : out std_logic;
           write1 : in std_logic;
           read1 : in std_logic;
           clock : in std_logic);
end interleaver_memory;

architecture Behavioral of interleaver_memory is
    type ram_type is array (0 to interleaver_length-1) of std_logic;
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