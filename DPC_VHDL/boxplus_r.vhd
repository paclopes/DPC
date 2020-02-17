library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity boxplus_r is
    Port ( la : in log_p_type;
           lb : in log_p_type;
           output : out log_p_type;
           clock : in std_logic);
end boxplus_r;

architecture Behavioral of boxplus_r is
    signal out_unregisted: log_p_type;
begin
    process(clock)
    begin
        if rising_edge(clock) then
            output <= out_unregisted;
        end if;
    end process;

    bp1: entity work.boxplus port map (la => la, lb => lb, output => out_unregisted, clock => clock);

end Behavioral;
