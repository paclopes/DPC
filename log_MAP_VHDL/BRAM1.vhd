library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utils.all;

entity BRAM1 is
	generic(data_size: integer; word_length: integer);
    Port ( read_addr: unsigned(length(data_size-1)-1 downto 0);
           write_addr: unsigned(length(data_size-1)-1 downto 0);
           data_in : in unsigned(word_length-1 downto 0);
           data_out : out unsigned(word_length-1 downto 0);
           write : in std_logic;
           read : in std_logic;
           clock : in std_logic);
end BRAM1;

architecture Behavioral of BRAM1 is
    type ram_type is array (0 to data_size-1) of unsigned(word_length-1 downto 0);
    signal ram: ram_type;
    signal addr: unsigned(length(data_size-1)-1 downto 0);
begin
    addr <= read_addr when write='0' else write_addr;

    process(clock)
    begin
        if rising_edge(clock) then
            if write='1' then 
                ram(to_integer(addr)) <= data_in;
            end if;
            if read='1' then
                if addr < data_size then
                        data_out <= ram(to_integer(addr));
                else
                    data_out <= (others => 'X');
                end if;
            end if;
        end if;
    end process;
end Behavioral;