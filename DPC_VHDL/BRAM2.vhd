library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utils.all;

entity BRAM2 is
	generic (size: integer);
    Port ( addr: in unsigned(length(size-1)-1 downto 0);
           data_in : in std_logic;
           data_out : out std_logic;
           write : in std_logic;
           read : in std_logic;
           clock : in std_logic);
end BRAM2;

architecture Behavioral of BRAM2 is
    type ram_type is array (0 to size-1) of std_logic;
    signal ram: ram_type;
begin
    process(clock)
    begin
        if rising_edge(clock) then
            if read = '1' then
                data_out <= ram(to_integer(addr));
            end if;
            if write='1' then 
                ram(to_integer(addr)) <= data_in;
            end if;
        end if;
    end process;
end Behavioral;