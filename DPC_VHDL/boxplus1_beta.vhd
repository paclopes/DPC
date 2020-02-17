-- boxplus1.vhd 29-8-2019
-- 
-- Reads the values from the interleaver output memory and calculates the output to feed the log-MAP_beta.
-- 
-- Runs in runs of cycle_length (22) that read 9 values (at most) from the interleaver memory
-- do the box plus operation and write 3 values to the output buffer in a mixed of serial and parallel operations.
--
-- Paulo Lopes, INESC-ID


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity boxplus1_beta is
    Port ( start_input : in std_logic;
           start_output : out std_logic;
           L : out llr_data_type;
           IM_data_out : in log_p_type;
           IM_address : out j_type;
           clock : in std_logic);
end boxplus1_beta;

architecture Behavioral of boxplus1_beta is
    constant bl_beta_j: integer := length(interleaver_length-1) + 1;
    subtype beta_j_type is unsigned(bl_beta_j-1 downto 0);
    signal j: beta_j_type;
    signal cn: unsigned(length(cycle_length-1) downto 0);
    signal ca: unsigned(1 downto 0);
    signal cb: unsigned(2 downto 0);
    signal la, lb, ly: log_p_type;
    signal Lx : llr_data_type;
    signal starting: std_logic := '0';
    signal running: std_logic := '0';
begin
    
    bp0: entity work.boxplus port map (la => la, lb => lb, output => ly, clock => clock);
    
    -- cb = 0 : lb <= M[2]
    -- cb = 1 : lb <= M[1], la <= lb
    -- cb = 2 : 
    -- cb = 3 : la <= BP(la, lb); lb <= M[0]
    -- cb = 4 : 
    -- cb = 5 : la(2-ca) <= BP(1, 2, 3)
    -- ...
    
    IM_address <= j(bl_j-1 downto 0);
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
                        j <= j - 1;
                    end if;
                end if;
                
                if j = 8 and cn = cycle_length - 1 then 
                    -- ends at 9 since log_map_beta.k only goes downto 1 since the the output is beta(k-1)
                    -- but needs to continue until cb=5 (and j=8) to make sure the calculation has finished.
                    running <= '0';
                    report "Ended boxplus1_beta";
                end if;
                
                -- la, lb, ly
                if ca < 3 then
                    if not is_cnd3 then
                        if cb = 1 then
                            if j < interleaver_length then
                                -- report "L(" & integer'image(2-to_integer(ca)) & ") <- " & to_string(IM_data_out) & " = M[" & to_string(j) & "]";
                                Lx(2-to_integer(ca)) <= signed(IM_data_out);
                            else
                                -- report "L(" & integer'image(2-to_integer(ca)) & ") <- 0";
                                Lx(2-to_integer(ca)) <= (others => '0');
                            end if;
                        end if;
                    else
                        if cb = 1 then
                            la <= lb;
                        elsif cb = 3 then
                            la <= ly;
                        elsif cb = 5 then
                            Lx(2-to_integer(ca)) <= signed(ly);
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
                    report "Starting boxplus1_beta";
                    j <= to_unsigned(interleaver_length - 1 + n_termination_bits, bl_beta_j);
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
