library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity boxplus2 is
    Port ( ended_input : in std_logic;
           ended_output : out std_logic;
           start : in std_logic;
           L : in llr_data_type;
           LW : in std_logic;
           LA : in llr_data_type;
           IM_data_in : inout log_p_type;
           IM_data_out : in log_p_type;
           IM_address : inout j_type;
           IM_write : out std_logic;
           IM_read : out std_logic;
           clock : in std_logic);
end boxplus2;

architecture Behavioral of boxplus2 is
    signal j: j_type := (others => '0');
    signal j1, j2, j3, j4, j5, j6, j7, j8: j_type;
    signal buffer0, buffer1, buffer2: log_p_type;
    signal ca, ca1, ca2, ca3: unsigned(1 downto 0) := "11"; -- saturates at "11" 
    signal cb, cb1, cb2, cb3: unsigned(1 downto 0) := "00";
    signal le0, le1, le2: log_p_type;
    signal l1, l2, la4, l6, l8, lb4: log_p_type;
    signal lex1, lex2, lex3, lex4, lex5, lex6: log_p_type;
    signal a1, a2, a3, a4, a5, a6, a7, a8: std_logic := '0';
    signal is_cnd3: std_logic;
    signal lex: log_p_type;
    signal IMW: std_logic;
    signal ending: std_logic := '0';
    signal start_d: std_logic := '0';
    signal start_dd: std_logic := '0';
begin

    IM_write <= IMW;
    
    process (clock)
    begin
        if rising_edge(clock) then

            if ca = 3 and ending = '1' then 
                ending <= '0'; -- to turn off ended_output 
            end if;
            
            -- le
            if LW = '1' then
                le0 <= saturating_sub(signed(L(0)), buffer0);
                le1 <= saturating_sub(signed(L(1)), buffer1);
                le2 <= saturating_sub(signed(L(2)), buffer2);
            end if;
            
            -- start_d goes to 1 as the log_map places a valid address at the bus for the lle input
            -- start_dd goes to 1 as the memory places valid data on the bus
            
            start_d <= start;
            start_dd <= start_d;
            if LW = '1' or start_dd = '1' then
                buffer0 <= signed(LA(0));
                buffer1 <= signed(LA(1));
                buffer2 <= signed(LA(2));
            end if;

            if ended_input = '1' then
                ending <= '1';
            end if;
            
            -- pipeline stage 0
            
            if LW = '1' then
                assert ca = 3 or (ca=2 and cb=2) report "LLR write before the end of the calculataions.";
                ca <= (others => '0');
                cb <= (others => '0');
            elsif a8 = '0' then
                -- when there is a pending write instruction everything waits  
                -- ca
                if ca /= 3 and (is_cnd3 = '0' or cb = 2) then
                    ca <= ca + 1;
                end if;
            
                -- cb
                if is_cnd3 = '1' and ca /= 3 then
                    if cb >= 2 then
                        cb <= "00";
                    else
                        cb <= cb + 1; 
                    end if;
                end if;
            end if; 
                
            if a8 = '0' then
                -- when there is a pending write instruction everything waits  

                -- j
                if start = '1' then
                    j <= (others => '0');
                elsif ca/=3 then
                    j <= j + 1;
                end if;
                
                -- pipeline stage 1
                
                if a1 = '1' then
                    if cb1 = 0 then
                        l1 <= IM_data_out;
                    elsif cb1 = 1 then
                        l2 <= IM_data_out;
                    elsif cb1 = 2 then
                        lb4 <= IM_data_out;
                    end if;
                end if;
                
                -- pipeline stage 3
                    
                if a3 = '1' then
                    if cb3 = 0 then
                        la4 <= l2;
                    elsif cb3 = 1 then
                        la4  <= l1;
                    elsif cb3 = 2 then
                        lb4 <= l2;
                    end if;
                end if;
            
                -- The start of the pipeline stals when a nop is inserted at stage 3 instead of the result from stage 2 
                -- but after stage 3 the pipeline continues to move to propagate the nop and other micro-instructions.
                -- the microinstruction consists in: 
                --   process j (the write address) and lex (reading and storing the corresponding values) is a = '1' (active instruction)

                j1 <= j; j2 <= j1; j3 <= j2;
                lex1 <= lex; lex2 <= lex1; lex3 <= lex2;   
                if ca = 3 or is_cnd3 = '0' then 
                    a1 <= '0';
                else 
                    a1 <= '1'; 
                end if;
                a2 <= a1; a3 <= a2;
                ca1 <= ca; ca2 <= ca1; ca3 <= ca2;
                cb1 <= cb; cb2 <= cb1; cb3 <= cb2;
                
            end if;

            j4 <= j3; j5 <= j4; j6 <= j5; j7 <= j6; j8 <= j7;
            lex4 <= lex3; lex5 <= lex4; lex6 <= lex5;
            a4 <= a3 and not a8; a5 <= a4; a6 <= a5; a7 <= a6; a8 <= a7;
            
        end if;
    end process;
       
    ended_output <= '1' when ca = 3 and ending = '1' else '0'; -- at the end is_cnd3 = '0' 
    
    is_cnd3 <= '1' when j < 3*cnd3 else '0';

    -- IM
    with ca select lex <=
        le0 when "00",
        le1 when "01",
        le2 when "10",
        (others => 'X') when others;
    
    IMW <= '1' when a8 = '1' or (ca /= 3 and is_cnd3 = '0') else '0';
    IM_read <= '1' when ca /= 3 and a8 = '0' and is_cnd3 = '1' else '0';
    IM_data_in <= l8 when a8='1' else lex;
    IM_address <= j8 when a8='1' else j;
    
    -- boxplus
    bp1: entity work.boxplus_r port map (la => la4, lb => lb4, output => l6, clock => clock);
    bp2: entity work.boxplus_r port map (la => l6, lb => lex6, output => l8, clock => clock);

end Behavioral;
