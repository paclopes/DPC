-- boxplus1.vhd 29-8-2019
-- 
-- Reads the values from the interleaver output memory and calculates the output to feed the log-MAP.
-- 
-- Runs in runs of cycle_length (22) that read 9 values (at most) from the interleaver memory
-- do the box plus operation and write 3 values to the output buffer in a mixed of serial and parallel operations.
--
-- Paulo Lopes, INESC-ID


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity boxplus1 is
    Port ( start_input : in std_logic;
           start_output : out std_logic;
           L : out llr_data_type;
           IM_data_out : in log_p_type;
           IM_address : out j_type;
           clock : in std_logic);
end boxplus1;

architecture Behavioral of boxplus1 is
    signal j: j_type;
    signal cn: unsigned(length(cycle_length-1) downto 0);
    signal ca: unsigned(1 downto 0);
    signal cb: unsigned(2 downto 0);
    signal la, lb, ly: log_p_type;
    signal Lx : llr_data_type;
    signal starting: std_logic := '0';
    signal ending: std_logic := '0';
    signal running: std_logic := '0';
begin
    
    bp0: entity work.boxplus port map (la => la, lb => lb, output => ly, clock => clock);
    
    -- cb = 0 : lb <= M[0]
    -- cb = 1 : lb <= M[1], la <- lb
    -- cb = 2 : 
    -- cb = 3 : la <= BP(la, lb); lb <= M[2]
    -- cb = 4 : 
    -- cb = 5 : L(ca) <= BP(1, 2, 3)
    -- ...
    
    IM_address <= j;
    lb <= IM_data_out;
    
    process(clock)
        variable is_cnd3: boolean;
    begin
        if rising_edge(clock) then
            is_cnd3 := (j < 3*cnd3);
        
            if running = '1' then
                if cn = cycle_length - 1 and starting = '1' then
                    starting <= '0';
                end if;
            
                -- cn, ca, cb
                if cn < cycle_length - 1 then
                    cn <= cn + 1;
                    
                    if is_cnd3 then
                        if cb < 5 then
                            cb <= cb + 1;
                        else
                            cb <= (others => '0');
                            if ca < 3 then
                                ca <= ca + 1;
                            end if;
                        end if;
                    else
                        if cb < 1 then
                            cb <= cb + 1;
                        else
                            cb <= (others => '0');
                            if ca < 3 then
                                ca <= ca + 1;
                            end if;
                        end if;
                    end if;
                    
                else
                    -- report "(last cycle clock) j: " & integer'image(to_integer(j));
                    cn <= (others => '0');
                    ca <= (others => '0');
                    cb <= (others => '0');
                end if;
                
                -- j
                if ca < 3 then
                    if (is_cnd3 and (cb = 0 or cb = 1 or cb = 3)) or (not is_cnd3 and cb = 1) then
                        j <= j + 1;
                    end if;
                end if;
                
                if j = interleaver_length - 1 then
                    ending <= '1';
                end if; 
                if ending = '1' and cn = cycle_length - 1 then
                    running <= '0';
                    ending <= '0';
                end if; 
                
                -- la, lb, ly
                if ca < 3 then
                    if not is_cnd3 then
                        if cb = 1 then
                            Lx(to_integer(ca)) <= signed(IM_data_out);
                        end if;
                    else
                        if cb = 1 then
                            la <= lb;
                        elsif cb = 3 then
                            la <= ly;
                        elsif cb = 5 then
                            Lx(to_integer(ca)) <= signed(ly);
                        end if;
                    end if;
                end if;
                
                if cn = cycle_length - 1 then
                    L(0) <= signed(saturate(signed(Lx(0)), log_p_type'length-1));
                    L(1) <= signed(saturate(signed(Lx(1)), log_p_type'length-1));
                    L(2) <= signed(saturate(signed(Lx(2)), log_p_type'length-1));
                end if;

            else
                if start_input = '1' then
                    j <=  (others => '0');
                    starting <= '1';
                    running <= '1';
                    
                    cn <= (others => '0');
                    ca <= (others => '0');
                    cb <= (others => '0');
                end if;
            end if;
            
            
        end if;
    end process;
    
    -- Start_output will be '1' in one clock cycle (0) and the log_map pipeline 
    -- will start running in the next (1). The value of L will be available in the start the next (2).
    start_output <= '1' when cn = cycle_length - 2 and starting = '1' else '0';

end Behavioral;
