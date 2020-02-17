library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity boxplus is
    Port ( la : in log_p_type;
           lb : in log_p_type;
           output : out log_p_type;
           clock : in std_logic);
end boxplus;

architecture Behavioral of boxplus is
    signal x0, x1, x2, x3: signed(bl_log_p downto 0);
    signal outputy: log_p_type;
    
    function MAX(LEFT, RIGHT: INTEGER) return INTEGER is
    begin
      if LEFT > RIGHT then return LEFT;
      else return RIGHT;
      end if;
    end;
    
begin
    process(clock)
        variable sum_l, dif_l: signed(bl_log_p downto 0); -- one bit extra
    begin
        if rising_edge(clock) then
            sum_l := non_saturating_add(la, lb);
            if (sum_l > 0) then
                x0 <= sum_l;
            else
                x0 <= (others=>'0');
            end if;
            x1 <= resize(read_max1_table(sum_l), x1'length);
            
            if la > lb then
                x2 <= resize(la, bl_log_p+1);
            else
                x2 <= resize(lb, bl_log_p+1);
            end if;
            
            dif_l := non_saturating_sub(la, lb);
            x3 <= resize(read_max1_table(dif_l), x3'length);
        end if; 
    end process;
    
    process(x0, x1, x2, x3)
    begin
        outputy <= saturating_resize(
            non_saturating_sub(non_saturating_add(x0, x1), non_saturating_add(x2, x3)),
            outputy'length
        );
    end process;
    
    output <= outputy;
    
end Behavioral;