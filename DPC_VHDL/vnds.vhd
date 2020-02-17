library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity vnds is
    Port ( IM_addr0: out j_type;
           IM_data_in0 : out log_p_type;
           IM_data_out0 : in log_p_type;
           IM_write0 : out std_logic := '0';
           IM_read0 : out std_logic := '0';
           LVM_addr: out l_type;
           LVM_data_in : out lv_type;
           LVM_data_out : in lv_type;
           LVM_write : out std_logic := '0';
           LVM_read : out std_logic := '0';
           start: in std_logic;
           ended: out std_logic;
           clock : in std_logic);
end vnds;

architecture Behavioral of vnds is
    signal j: j_type; -- the index of the interleaver output bit
    signal acc: lv_type; -- accumulator
    signal l, ld, ld_d: l_type; -- the index of the output bit (encoder input bit)
    signal i: unsigned(bl_vnd_dg-1 downto 0); -- the position inside a vnd
    signal w: unsigned(1 downto 0);
    constant bl_i_vnd : integer := length(VND(0));
    signal i_vnd: unsigned(bl_i_vnd-1 downto 0); -- the index number of the vnd of a given degree
    constant  bl_i_vnd_type : integer := length(n_VND_dg); 
    signal i_vnd_type: unsigned(bl_i_vnd_type-1 downto 0); -- the index number of the type of vnd (degree)
    signal running_pass_1, running_pass_1d, running_pass_1dd  : std_logic := '0';
    signal running_pass_2 : std_logic := '0';
    signal load_acc, load_acc_d : std_logic := '0';
    signal IM_reg : log_p_type;
    signal LVMR: std_logic := '0'; 
    signal ended_i : std_logic := '0';
begin

    IM_addr0 <= j;
    IM_read0 <= '1' when running_pass_1='1' or (w = 0 and running_pass_2 = '1') else '0'; 
    IM_write0 <= '1' when w = 2 and running_pass_2 = '1'else '0';
    IM_data_in0 <= IM_reg;
    
    LVM_addr <= ld_d when running_pass_1dd = '1' else l;
    LVM_data_in <= acc;
    LVM_write <= load_acc_d;  --- when there is a load in pipeline stage 2 there is always a store in pipeline stage 3 
    LVM_read <= LVMR and running_pass_2;
    ended <= ended_i;

    process(clock)
    begin
        if rising_edge(clock) then
            if start = '1' then
                running_pass_1 <= '1';
                j <= to_unsigned(0, bl_j);
                acc <= to_signed(0, bl_lv);
                l <= to_unsigned(0, bl_l);
                i <= to_unsigned(0, bl_vnd_dg);
                i_vnd <= to_unsigned(0, bl_i_vnd);
                i_vnd_type <= to_unsigned(0, bl_i_vnd_type);
            end if;
            
            if running_pass_1dd = '1' and running_pass_1d = '0' then
                -- starts pass 2
                running_pass_2 <= '1';
                LVMR <= '1';
                j <= to_unsigned(0, bl_j);
                l <= to_unsigned(0, bl_l);
                i <= to_unsigned(0, bl_vnd_dg);
                i_vnd <= to_unsigned(0, bl_i_vnd);
                i_vnd_type <= to_unsigned(0, bl_i_vnd_type);
                w <= "00";
            end if;
                       
            -- Small pipeline stage 1 (read and calc next)
            -- microinstructions can be accumulates M[j] or load M[j]                        
            if running_pass_1 = '1' or running_pass_2 = '1' then
                
                if w < 2 and running_pass_2 = '1' then
                    w <= w + 1;
                else
                    if running_pass_2 = '1' then
                        w <= "00";
                    end if;
                    j <= j + 1;
                    if i < VND_dg(to_integer(i_vnd_type)) - 1 then
                        i <= i + 1;
                    else
                        l <= l + 1;

                        if i_vnd < VND(to_integer(i_vnd_type)) - 1 then 
                            i_vnd <= i_vnd + 1;
                        else
                            if i_vnd_type < n_VND_dg - 1 then
                                i_vnd_type <= i_vnd_type + 1;
                            elsif running_pass_1 = '1' then
                                running_pass_1 <= '0';
                            else
                                running_pass_2 <= '0';
                                ended_i <= '1';
                            end if; 
                            i_vnd <= to_unsigned(0, bl_i_vnd);
                        end if;
                        
                        if running_pass_1 = '1' then
                            load_acc <= '1';
                        else -- running_pass_2 = '1'
                            LVMR <= '1';
                        end if;
                        
                        ld <= l;
                        i <= to_unsigned(0, bl_vnd_dg);
                    end if;
                end if;
            end if;
            
            ld_d <= ld;
            load_acc_d <= load_acc;
            running_pass_1d <= running_pass_1;
            running_pass_1dd <= running_pass_1d;

            -- Pass 1 small pipeline stage 2 (accumulate or load)
            if running_pass_1d = '1' then
                if load_acc_d = '1' then
                    acc <= resize(IM_data_out0, bl_lv);
                else
                    acc <= acc + IM_data_out0;
                end if;
            end if;
            
            if load_acc = '1' then
                load_acc <= '0';
            end if;
            
            if LVMR = '1' then
                LVMR <= '0';
            end if;

            if running_pass_2 = '1' and w = 1 then
                IM_reg <= saturating_resize(
                    LVM_data_out - IM_data_out0, 
                    -- does not requires saturation since there are enough bits 
                    bl_log_p
                );
            end if;
            
            if ended_i = '1' then
                ended_i <= '0';
            end if;

        end if;
    end process;

end Behavioral;