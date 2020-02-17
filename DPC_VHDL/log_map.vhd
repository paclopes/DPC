-- log_map.vhd 13-7-2018
--
-- Paulo Lopes, INESC-ID

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity log_map is
    port (
        input_data_address: out k_type;
        y_data: in y_pair_type;
        llr_input_data: in llr_data_type;
        input_data_read: out std_logic;

        qinv: in signed(bl_qinv-1 downto 0);
        
        beta_address: out beta_address_type;
        beta_data: in beta_data_type;
        beta_read: out std_logic;
        
        llr_output_address: out k_type;
        llr_output_data: out llr_data_type;
        llr_output_write: out std_logic;

        start: in std_logic;
        ended: out std_logic;
        clock: in std_logic
    );
end log_map;

architecture Behavioral of log_map is
    constant bl_k: integer := length(tau-1);
    signal ended_i: std_logic  := '0';
    subtype u_type is unsigned(n_group_input_bits-1 downto 0);
    subtype uz_type is unsigned(n_group_input_bits-2 downto 0);
    subtype s_type is unsigned(length(n_states-1)-1 downto 0);
    subtype ssz_type is unsigned(length(n_states-1)-2 downto 0);
    subtype k_type is unsigned(bl_k-1 downto 0);
    subtype cs_type is unsigned(2 downto 0);
    type u_array is array (0 to n_states-1) of u_type;
    type ss_array is array (0 to n_states-1) of s_type;
    type bit_array is array (0 to n_states-1) of std_logic;
    
    -- stage 0 input registers/signals
    signal uz: uz_type;
    signal u: u_array;
    signal v: bit_array;
    signal k: k_type;
    signal ssz: ssz_type;
    signal ss: ss_array;
    signal cs: cs_type;  -- calculations state
    signal running: std_logic  := '0';
    
    -- common pipeline registers
    constant pipeline_stages: integer := 15;
    type common_pipeline_registers is record
        u: u_array;
        v: bit_array;
        k: k_type;
        ssz: ssz_type;
        ss: ss_array;
        cs: cs_type;  -- calculations state
        running: std_logic;
    end record common_pipeline_registers;
    type pr_type is array (0 to pipeline_stages-2) of common_pipeline_registers;
    signal pr: pr_type := (others=>(
        u=>(others=>(others=>'0')),
        v=>(others=>'0'),
        k=>(others=>'0'),
        ssz=>(others=>'0'),
        ss=>(others=>(others=>'0')),
        cs=>(others=>'0'),
        running=>'0')
        );

    -- stage 0 output registers and variables
    shared variable cv1: bit_array;
    shared variable cv2: bit_array;
    shared variable c_acc1: bit_array;
    shared variable c_acc2: bit_array;
    shared variable c_acc3: bit_array;
    type state_array is array (0 to n_states-1) of unsigned(length(n_states-1)-1 downto 0);
    signal s_b: state_array;
    
    signal y0, y1: y_type;
    type y_array is array (0 to n_states-1) of y_type;
    signal y0h, y1h: y_array;
    signal llr0, llr1, llr2: log_p_type;
    
    -- stage 1 output registers
    signal dy0, dy1: y_array;
    type log_p_array is array (0 to n_states-1) of log_p_type;
    signal llr0u, llr1u, llr2u: log_p_array;

    -- stage 2 output registers
    signal sq_dy0, sq_dy1: y_array;
    signal llr0m, llr1m, llr2m: log_p_array;
    
    -- stage 3 output registers
    signal log_gamma0, log_gamma1: log_p_array;
    signal llr_0p1, llr2m_delay1: log_p_array;

    -- stage 4 output registers
    signal log_gamma_0p1: log_p_array;
    signal llr_sum: log_p_array;

    -- stage 5 output registers
    type s_type_array is array (0 to n_states-1) of s_type;
    signal log_gamma: log_p_array;
    
    -- stage 6 output registers
    signal log_gamma_plus_log_alpha: log_p_array;
    signal log_beta_s: log_p_array;
    
    -- stage 7 output registers
    signal alpha_acc: log_p_array;
    signal transfer: std_logic;
    shared variable transfer_var: std_logic;
    signal llr_x: log_p_array;
    
    -- stage 8 output registers
    signal alpha_aux: log_p_array;
    signal llr_x2: log_p_array;
    shared variable sx: state_array;
    
    -- stage 9 output registers
    type log_p_array_of_4 is array (0 to 3) of log_p_type;
    signal llr_x_reduction1: log_p_array_of_4; 
    
    -- stage 10 output registers
    type log_p_array_of_2 is array (0 to 1) of log_p_type;
    signal llr_x_reduction2: log_p_array_of_2; 
    
    -- stage 11 output registers
    signal llr_x_reduction2_delay1: log_p_array_of_2; 
    signal llr_x_reduction: log_p_type; 
    
    -- stage 12 output registers
    type log_p_array_of_6 is array (0 to 5) of log_p_type;
    signal llr_acc: log_p_array_of_6;

    -- stage 13 output registers
    type log_p_array_of_3 is array (0 to 2) of log_p_type;
    signal llr_aux: log_p_array_of_3;

begin
    ended <= ended_i;
    
    -- common pipeline registers
    process (clock)
    begin
        if rising_edge(clock) then
            -- for some reasson loop doesn't seam to work!
            pr(1) <= pr(0);
            pr(2) <= pr(1);
            pr(3) <= pr(2);
            pr(4) <= pr(3);
            pr(5) <= pr(4);
            pr(6) <= pr(5);
            pr(7) <= pr(6);
            pr(8) <= pr(7);
            pr(9) <= pr(8);
            pr(10) <= pr(9);
            pr(11) <= pr(10);
            pr(12) <= pr(11);
            pr(13) <= pr(12);
        end if;
    end process;
    
    -- connections the memory banks
    input_data_address <= k;
    y0 <= y_type(y_data(0));
    y1 <= y_type(y_data(1));
    llr0 <= log_p_type(llr_input_data(0));
    llr1 <= log_p_type(llr_input_data(1));
    llr2 <= log_p_type(llr_input_data(2));
    input_data_read <= running;
    
    gen1: for bank in 0 to n_states-1 generate
        log_beta_s(bank) <= signed(beta_data(bank));
    end generate gen1;    
    beta_address <= pr(5).k;
    beta_read<= pr(5).running;
    
    llr_output_address <= pr(13).k;
    llr_output_data(0) <= signed(llr_aux(0));
    llr_output_data(1) <= signed(llr_aux(1));
    llr_output_data(2) <= signed(llr_aux(2));
    llr_output_write <=  '1' when pr(13).cs = 1 and pr(13).running='1' else '0';
    
    -- define some "alias"
    gena: for s in 0 to n_states-1 generate
        s_b(s) <= to_unsigned(s, length(n_states-1));
        ss(s)(2) <= ssz(1);
        ss(s)(1) <= s_b(s)(0);
        ss(s)(0) <= ssz(0);
        v(s) <= s_b(s)(1);
        u(s)(2 downto 1) <= uz;
        u(s)(0) <= ss(s)(2) xor u(s)(2) xor u(s)(1) xor s_b(s)(2);
    end generate gena;    
    
    process (clock)
    begin
        if rising_edge(clock) then
            if ended_i = '1' then
                ended_i <= '0';
            end if;

            for s in 0 to n_states-1 loop
            
                if start = '1' then
                    k <= to_unsigned(0, bl_k);
                    ssz <= (others=>'0');
                    uz <= (others=>'0');
                    cs <= (others=>'0');
                    running <= '1';
                    ended_i <= '0';
                    
                    if s=0 then
                        alpha_aux(s) <= (others=>'0');
                    else
                        alpha_aux(s) <= to_signed(minimum_of_log_p_type, bl_log_p);
                    end if;
                else
                    
                    pr(0).running <=  running;
                    ------- stage 0
                    if running = '1' then
                        -- go the through the values of v, u and k
                        if cs=0 then
                            ssz <= ssz + 1;
                            if ssz = "11" then
                                uz <= uz + 1;
                                if uz = "11" then
                                    cs <= cs + 1;
                                end if;
                            end if;
                        else
                            if cs = 6 then
                                -- if (s = 0) then report "(last cycle clock) k: " & integer'image(to_integer(k)); end if; 
                                cs <= (others=>'0');
                                if k < to_unsigned(tau - 1 - n_termination_bits/n_group_input_bits, bl_k) then
                                    -- if (s = 0) then report "tau: " & integer'image(tau) & " k: " & integer'image(to_integer(k)); end if;
                                    k <= k + 1;
                                else
                                    running <= '0';
                                end if;
                            else
                                cs <= cs + 1;
                            end if;
                        end if;
                        
                        pr(0).u(s) <= u(s);
                        pr(0).v(s) <= v(s);
                        pr(0).k <= k;
                        pr(0).ss(s) <= ss(s);
                        pr(0).cs <= cs;
        
                        -- y0 <= y_bank0(to_integer(k));
                        -- y1 <= y_bank1(to_integer(k));
                        cv1(s) := (v(s) and g1(2)) xor (ss(s)(1) and g1(1)) xor (ss(s)(0) and g1(0));
                        cv2(s) := (v(s) and g2(2)) xor (ss(s)(1) and g2(1)) xor (ss(s)(0) and g2(0));
                        c_acc1(s) := ss(s)(2) xor u(s)(2);
                        c_acc2(s) := c_acc1(s) xor u(s)(1);
                        c_acc3(s) := c_acc2(s) xor u(s)(0);
                        y0h(s)(bl_y-1) <= not (cv1(s) xor c_acc1(s));
                        y0h(s)(bl_y-2) <= c_acc2(s);
                        y0h(s)(bl_y-3) <= '1';
                        y0h(s)(bl_y-4 downto 0) <= (others=>'0');
                        -- y0h = (not (cv1 xor c_acc1)) & c_acc3 & 1 & 000...
                        y1h(s)(bl_y-1) <= not (cv2(s) xor c_acc1(s));
                        y1h(s)(bl_y-2) <= c_acc3(s);
                        y1h(s)(bl_y-3) <= '1';
                        y1h(s)(bl_y-4 downto 0) <= (others=>'0');
                        -- y1h = (not (cv2 xor c_acc1)) & c_acc3 & 1 & 000...

                        -- llr0 <= LLR_bank0(to_integer(k));
                        -- llr1 <= LLR_bank1(to_integer(k));
                        -- llr2 <= LLR_bank2(to_integer(k));
                    end if;
                    
                    ------- stage 1
                    if pr(0).running='1' then
        
                        dy0(s) <= y0 - y0h(s);
                        dy1(s) <= y1 - y1h(s);
                            
                        -- 2'c of LLR for u=1
                        llr0u(s) <= saturating_twos_complement(llr0, pr(0).u(s)(2));
                        llr1u(s) <= saturating_twos_complement(llr1, pr(0).u(s)(1));
                        llr2u(s) <= saturating_twos_complement(llr2, pr(0).u(s)(0));
                    end if;          
                        
                    ------- stage 2
                    if pr(1).running='1' then
                        sq_dy0(s) <= times(dy0(s), dy0(s), 1);
                        sq_dy1(s) <= times(dy1(s), dy1(s), 1);
                        
                        llr0m(s) <= max1_aux(llr0u(s));
                        llr1m(s) <= max1_aux(llr1u(s));
                        llr2m(s) <= max1_aux(llr2u(s));
                    end if;
        
                    ------- stage 3
                    if pr(2).running='1' then
                        log_gamma0(s) <= change_fb( ('0' & sq_dy0(s)) * qinv, fb_y_sq + fb_qinv, fb_log_p, bl_log_p);
                        log_gamma1(s) <= change_fb( ('0' & sq_dy1(s)) * qinv, fb_y_sq + fb_qinv, fb_log_p, bl_log_p);

                        llr_0p1(s) <= saturating_add(llr0m(s), llr1m(s));
                        llr2m_delay1(s) <= llr2m(s);
                    end if;
        
                    ------- stage 4
                    if pr(3).running='1' then  
                        log_gamma_0p1(s) <= saturating_add(log_gamma0(s), log_gamma1(s));
                        if pr(3).k >= tau - n_termination_bits/n_group_input_bits then
                            llr_sum(s) <= (others => '0');
                        else
                            llr_sum(s) <= saturating_add(llr_0p1(s), llr2m_delay1(s));
                        end if;
                    end if;
        
                    ------- stage 5
                    if pr(4).running='1' then
        
                        if pr(4).k >= tau - n_termination_bits/n_group_input_bits 
                                and (pr(4).u(s) /= to_integer(pr(4).ss(s)(2)) or pr(4).v(s)/='0') then
                            log_gamma(s)(log_p_type'left) <= '1';
                            log_gamma(s)(log_p_type'left-1 downto 0) <= (others=>'0');
                        else
                            log_gamma(s) <= saturating_add(log_gamma_0p1(s), llr_sum(s));
                        end if;
                    end if;
                    
                    ------- stage 6
                    if pr(5).running='1' then
                        log_gamma_plus_log_alpha(s) <=  saturating_add(log_gamma(s), alpha_aux(to_integer(pr(5).ss(s))));
                        -- beta_s(s) <= beta_bank(s)(pr(5).k)
                    end if;
                    
                    ------- stage 7
                    transfer_var := '0';
                    if pr(6).running='1' then
                        if pr(6).cs = 0 and pr(6).u(s)(2 downto 1) = "00" and pr(6).ss(s)(2)='0' and pr(6).ss(s)(0)='0' then
                        -- load alpha
                            alpha_acc(s) <= log_gamma_plus_log_alpha(s);
                        elsif pr(6).cs = 0 then
                        -- calc alpha
                            alpha_acc(s) <= max1(alpha_acc(s), log_gamma_plus_log_alpha(s));
                        elsif pr(6).cs = 1 then
                        -- alpha normalization
                            transfer_var := '1';
                            alpha_acc(s) <= saturating_sub( 
                                alpha_acc(s),
                                saturating_resize(alpha_acc(0), bl_log_p - normalization_value_bit_reduction)
                                );
                        end if;
                        llr_x(s) <= saturating_add(log_gamma_plus_log_alpha(s), log_beta_s(s));
                    end if;
                    transfer <= transfer_var; -- Needs to be updated with running='0' to set to zero at the end.
    
                    ------- stage 8
                    if pr(7).running='1' then 
                        if transfer='1' then
                            alpha_aux <= alpha_acc;
                        end if;

                        sx(s) := s_b(s);
                        sx(s)(2) := pr(7).u(s)(0);
                        -- for s(2) = 0 the inputs with u(0)=0 should be choosen and 
                        -- for s(2) = 1 the inputs with u(0)=1 should be choosen =>
                        -- u(0) = s(2) => sx(s)(2) = ss(2) xor u(2) xor u(1) xor s(2) = u(0)
                        llr_x2(s) <= llr_x(to_integer(sx(s)));
                        -- note that two bits of sx(s) are constant so there is only one selection
                        -- bit for the mux, resulting in a mux with two inputs.
                    end if;         

                    ------- stage 9
                    if pr(8).running='1' then
                        if s<4 then
                        llr_x_reduction1(s) <= max1(llr_x2(2*s), llr_x2(2*s+1));
                    end if; 
                    end if;         

                    ------- stage 10
                    if pr(9).running='1' then
                        if s<2 then
                        llr_x_reduction2(s) <= max1(llr_x_reduction1(2*s), llr_x_reduction1(2*s+1));
                    end if; 
                    end if;
                    
                    ------- stage 11
                    if pr(10).running='1' then
                        if s<1 then -- to sppeed up simualtions a bit
                            llr_x_reduction2_delay1 <= llr_x_reduction2;
                            llr_x_reduction <= max1(llr_x_reduction2(0), llr_x_reduction2(1));
                        end if;
                    end if;         

                    ------- stage 12
                    if pr(11).running='1' then
                        if pr(11).cs = 0 and pr(11).u(s)(2 downto 1) = "00" 
                                and pr(11).ss(s)(2)='0' and pr(11).ss(s)(0)='0' then
                            -- load llr
                            if s<4 then
                                if pr(11).u(s)(2)='0' then
                                    llr_acc(0) <= llr_x_reduction;
                                    llr_acc(1) <= to_signed(minimum_of_log_p_type, bl_log_p);
                                else
                                    llr_acc(0) <= to_signed(minimum_of_log_p_type, bl_log_p);
                                    llr_acc(1) <= llr_x_reduction;
                                end if;
                                
                                if pr(11).u(s)(1)='0' then
                                    llr_acc(2) <= llr_x_reduction;
                                    llr_acc(3) <= to_signed(minimum_of_log_p_type, bl_log_p);
                                else
                                    llr_acc(2) <= to_signed(minimum_of_log_p_type, bl_log_p);
                                    llr_acc(3) <= llr_x_reduction;
                                end if;
                            elsif s<6 then
                                llr_acc(s) <= llr_x_reduction2_delay1(s-4);
                            end if;
                        elsif pr(11).cs = 0 then
                            -- calc llr
                            if s<4 then
                                if pr(11).u(s)(2)='0' then
                                    llr_acc(0) <= max1(llr_acc(0), llr_x_reduction);
                                else
                                    llr_acc(1) <= max1(llr_acc(1), llr_x_reduction);
                                end if;
                                if pr(11).u(s)(1)='0' then
                                    llr_acc(2) <= max1(llr_acc(2), llr_x_reduction);
                                else
                                    llr_acc(3) <= max1(llr_acc(3), llr_x_reduction);
                                end if;
                            elsif s<6 then
                                llr_acc(s) <= max1(llr_acc(s), llr_x_reduction2_delay1(s-4));
                            end if;
                        end if;
                    end if;         

                    ------- stage 13
                    if pr(12).running='1' then
                        if s < 3 and pr(12).cs = 1 then
                            llr_aux(s) <= saturating_sub(llr_acc(2*s), llr_acc(2*s+1));
                        end if;
                    end if;         

                    ------- stage 14
                    if pr(13).running='1' then
                        if pr(12).running = '0' then
                            ended_i <= '1';
                        end if;
                    end if;         
                end if; -- if start = '1' then ... else
            end loop; -- for s in 0 to n_states-1 loop
        end if; -- if rising_edge(clock) then
    end process;    
end Behavioral;