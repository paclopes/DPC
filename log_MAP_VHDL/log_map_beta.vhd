library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;
use work.some_constants.all;

entity log_map_beta is
    port (
        address: out log_map_address_type;
        y_banks_data: in y_data_type;
        llr_banks_data: in llr_data_type;
        read: out std_logic;

        qinv: in signed(bl_qinv-1 downto 0);
        
        beta_banks_address: out beta_address_type;
        beta_banks_data: out beta_data_type;
        beta_banks_write: out std_logic;

        start: in std_logic;
        ended: out std_logic;
        clock: in std_logic
    );
end log_map_beta;

architecture Behavioral of log_map_beta is
    constant bl_k: integer := length(tau-1);
    signal ended_i: std_logic  := '0';
    subtype u_type is unsigned(n_input_bits downto 0);
    subtype s_type is unsigned(length(n_states-1)-1 downto 0);
    subtype k_type is unsigned(bl_k-1 downto 0);
    
    -- stage 0 input registers 
    signal u: u_type;
    signal v: std_logic;
    signal k: k_type;
    signal running: std_logic  := '0';

    -- stage 0 output registers and variables
    signal u0: u_type;
    signal v0: std_logic;
    signal k0: k_type;
    signal running0: std_logic  := '0';
    type bit_array is array (0 to n_states-1) of std_logic;
    shared variable cv1: bit_array;
    shared variable cv2: bit_array;
    shared variable c_acc1: bit_array;
    shared variable c_acc2: bit_array;
    shared variable c_acc3: bit_array;
    type state_array is array (0 to n_states-1) of unsigned(length(n_states-1)-1 downto 0);
    shared variable ss_b: state_array;
    
    signal y0, y1: y_type;
    type y_array is array (0 to n_states-1) of y_type;
    signal y0h, y1h: y_array;
    signal llr0, llr1, llr2: log_p_type;
    
    -- stage 1 output registers
    signal u1: u_type;
    signal v1: std_logic;
    signal k1: k_type;
    signal running1: std_logic  := '0';
    signal dy0, dy1: y_array;
    signal llr0u, llr1u, llr2u: log_p_type;

    -- stage 2 output registers
    signal u2: u_type;
    signal v2: std_logic;
    signal k2: k_type;
    signal running2: std_logic  := '0';
    signal sq_dy0, sq_dy1: y_array;
    signal llr0m, llr1m, llr2m: log_p_type;
    
    -- stage 3 output registers
    signal u3: u_type;
    signal v3: std_logic;
    signal k3: k_type;
    signal running3: std_logic  := '0';
    type log_p_array is array (0 to n_states-1) of log_p_type;
    signal log_gamma_reg0, log_gamma_reg1: log_p_array;
    signal llr_0p1, llr2m_delay1: log_p_type;

    -- stage 4 output registers
    signal u4: u_type;
    signal v4: std_logic;
    signal k4: k_type;
    signal running4: std_logic  := '0';
    signal log_gamma_reg_0p1: log_p_array;
    signal llr_sum: log_p_type;

    -- stage 5 output registers
    signal u5: u_type;
    signal v5: std_logic;
    signal k5: k_type;
    type s_type_array is array (0 to n_states-1) of s_type;
    signal s5: s_type_array;
    signal running5: std_logic  := '0';
    signal log_gamma_regs: log_p_array;
    
    -- stage 6 output registers
    signal u6: u_type;
    signal v6: std_logic;
    signal k6: k_type;
    signal log_gamma_plus_log_beta: log_p_array;
    signal running6: std_logic  := '0';
    
    -- stage 7 output registers
    signal k7: k_type;
    signal beta_acc: log_p_array;
    signal transfer: std_logic;
    signal running7: std_logic  := '0';
    shared variable transfer_var: std_logic;
    
    -- stage 8 output registers
    signal k8m1: k_type;
    signal beta_aux: log_p_array;
    signal write_beta: std_logic;
    signal running8: std_logic  := '0';

begin
    ended <= ended_i;
    
    address <= k;
    y0 <= y_type(y_banks_data(0));
    y1 <= y_type(y_banks_data(1));
    llr0 <= log_p_type(llr_banks_data(0));
    llr1 <= log_p_type(llr_banks_data(1));
    llr2 <= log_p_type(llr_banks_data(2));
    read <= running;
    read <= running;
    
    gen1: for bank in 0 to n_states-1 generate
        beta_banks_data(bank) <= unsigned(beta_aux(bank));
    end generate gen1;    
    beta_banks_address <= k8m1;
    beta_banks_write <= write_beta;
    
    process (clock)
    begin
        if rising_edge(clock) then
            if ended_i = '1' then
                ended_i <= '0';
            end if;
        
            if start = '1' then
                k <= to_unsigned(tau-1, bl_k);
                v <= '0';
                u <= (others=>'0');
                running <= '1';
                ended_i <= '0';
                for ss in 1 to n_states-1 loop
                    beta_aux(ss) <= to_signed(minimum_of_log_p_type, bl_log_p);
                end loop;
                beta_aux(0) <= (others=>'0');

                -- to write beta(ss,tau-1);
                k8m1 <= to_unsigned(tau-1, bl_k);
                write_beta <= '1';
            else
                for ss in 0 to n_states-1 loop
                    ss_b(ss) := to_unsigned(ss, length(n_states-1));
                end loop;
                
                running0 <=  running;
                ------- stage 0
                if running = '1' then
                    -- go the through the values of v, u and k
                    v <= not v;
                    if v = '1' then
                        u <= u + 1;
                        if u > n_inputs then -- goes 0 to 9
                            if k = 1 then
                                running <= '0';
                            else
                                k <= k - 1;
                                u <= (others => '0');
                            end if;            
                        end if;
                    end if;
                    
                    u0 <= u;
                    v0 <= v;
                    k0 <= k;
    
                    -- y0 <= y_bank0(to_integer(k));
                    -- y1 <= y_bank1(to_integer(k));
                    for ss in 0 to n_states-1 loop
                        cv1(ss) := (v and g1(2)) xor (ss_b(ss)(1) and g1(1)) xor (ss_b(ss)(0) and g1(0));
                        cv2(ss) := (v and g2(2)) xor (ss_b(ss)(1) and g2(1)) xor (ss_b(ss)(0) and g2(0));
                        c_acc1(ss) := ss_b(ss)(2) xor u(2);
                        c_acc2(ss) := c_acc1(ss) xor u(1);
                        c_acc3(ss) := c_acc2(ss) xor u(0);
                        y0h(ss)(bl_y-1) <= not (cv1(ss) xor c_acc1(ss));
                        y0h(ss)(bl_y-2) <= c_acc2(ss);
                        y0h(ss)(bl_y-3) <= '1';
                        y0h(ss)(bl_y-4 downto 0) <= (others=>'0');
                        -- y0h = (not (cv1 xor c_acc1)) & c_acc3 & 1 & 000...
                        y1h(ss)(bl_y-1) <= not (cv2(ss) xor c_acc1(ss));
                        y1h(ss)(bl_y-2) <= c_acc3(ss);
                        y1h(ss)(bl_y-3) <= '1';
                        y1h(ss)(bl_y-4 downto 0) <= (others=>'0');
                        -- y1h = (not (cv2 xor c_acc1)) & c_acc3 & 1 & 000...
                    end loop;
                    -- llr0 <= LLR_bank0(to_integer(k));
                    -- llr1 <= LLR_bank1(to_integer(k));
                    -- llr2 <= LLR_bank2(to_integer(k));
                end if;
                
                ------- stage 1
                running1 <=  running0;
                if running0='1' then
                    u1 <= u0;
                    v1 <= v0;
                    k1 <= k0;
    
                    for ss in 0 to n_states-1 loop
                        u1 <= u0;
                        dy0(ss) <= y0 - y0h(ss);
                        dy1(ss) <= y1 - y1h(ss);
                    end loop;
                    -- 2'c of LLR for u=1
                    llr0u <= plus1(xor1(llr0, u0(2)), u0(2));
                    llr1u <= plus1(xor1(llr1, u0(1)), u0(1));
                    llr2u <= plus1(xor1(llr2, u0(0)), u0(0));
                end if;          
                    
                ------- stage 2
                running2 <=  running1;
                if running1='1' then
                    u2 <= u1;
                    v2 <= v1;
                    k2 <= k1;
    
                    for ss in 0 to n_states-1 loop
                        sq_dy0(ss) <= times(dy0(ss), dy0(ss), 1);
                        sq_dy1(ss) <= times(dy1(ss), dy1(ss), 1);
                    end loop;
                    
                    llr0m <= max1_aux(llr0u);
                    llr1m <= max1_aux(llr1u);
                    llr2m <= max1_aux(llr2u);
                end if;
    
                ------- stage 3
                running3 <=  running2;
                if running2='1' then
                    u3 <= u2;
                    v3 <= v2;
                    k3 <= k2;
    
                    for ss in 0 to n_states-1 loop
                        log_gamma_reg0(ss) <= change_fb( ('0' & sq_dy0(ss)) * qinv, fb_y_sq + fb_qinv, fb_log_p, bl_log_p);   
                        log_gamma_reg1(ss) <= change_fb( ('0' & sq_dy1(ss)) * qinv, fb_y_sq + fb_qinv, fb_log_p, bl_log_p);
                    end loop;
                    llr_0p1 <= saturating_add(llr0m, llr1m);
                    llr2m_delay1 <= llr2m;
                end if;
    
                ------- stage 4
                running4 <=  running3;
                if running3='1' then  
                    u4 <= u3;
                    v4 <= v3;
                    k4 <= k3;
                    
                    for ss in 0 to n_states-1 loop
                        log_gamma_reg_0p1(ss) <= saturating_add(log_gamma_reg0(ss), log_gamma_reg1(ss));
                    end loop;
                    if k3 >= tau - n_termination_bits/n_input_bits then
                        llr_sum <= (others => '0');
                    else
                        llr_sum <= saturating_add(llr_0p1, llr2m_delay1);
                    end if;
                end if;
    
                ------- stage 5
                running5 <=  running4;
                if running4='1' then
                    u5 <= u4;
                    v5 <= v4;
                    k5 <= k4;
    
                    for ss in 0 to n_states-1 loop
                        s5(ss) <=   (ss_b(ss)(2) xor u4(2) xor u4(1) xor u4(0))
                                    & v4 & ss_b(ss)(1);
                        if k4 >= tau - n_termination_bits/n_input_bits 
                                and (u4 /= ss / (n_states/2) or v4/='0') then
                            log_gamma_regs(ss)(log_p_type'left) <= '1';
                            log_gamma_regs(ss)(log_p_type'left-1 downto 0) <= (others=>'0');
                        else
                            log_gamma_regs(ss) <= saturating_add(log_gamma_reg_0p1(ss), llr_sum);
                        end if;
                    end loop;
                end if;
                
                ------- stage 6
                running6 <=  running5;
                if running5='1' then
                    u6 <= u5;
                    v6 <= v5;
                    k6 <= k5;
    
                    for ss in 0 to n_states-1 loop
                        log_gamma_plus_log_beta(ss) <=  saturating_add(log_gamma_regs(ss), beta_aux(to_integer(s5(ss))));
                    end loop;
                end if;
                
                ------- stage 7
                running7 <=  running6;
                
                transfer_var := '0';
                if running6='1' then
                    k7 <= k6;
                    for ss in 0 to n_states-1 loop
                        if u6 = 0 and v6 = '0' then
                        -- load beta
                            beta_acc(ss) <= log_gamma_plus_log_beta(ss);
                        elsif u6 < n_inputs then
                        -- calc beta
                            beta_acc(ss) <= max1(beta_acc(ss), log_gamma_plus_log_beta(ss));
                        elsif u6 = n_inputs and v6 = '0' then
                        -- beta normalization
                            transfer_var := '1';
                            beta_acc(ss) <= beta_acc(ss) - beta_acc(0);
                        end if;
                    end loop;
                end if;
                transfer <= transfer_var; -- Needs to be updated with running='0' to set to zero at the end.

                ------- stage 8
                running8 <=  running7;
                write_beta <= transfer;   -- Needs to be updated with running='0' to set to zero at the end.
                if running7='1' and transfer='1' then
                    k8m1 <= k7-1;
                    beta_aux <= beta_acc;
                end if;
                                
                ------- stage 9
                if running8='1' then
                    if running7='0' then
                        ended_i <= '1';
                    end if;
                end if;
            end if; -- if start = '1' then ... else
        end if; -- if rising_edge(clock) then
    end process;    
end Behavioral;