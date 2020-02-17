-- DC_codec.vhd 29-8-2019
-- 
-- Dirty Paper Codding CODEC and channel simulator.
-- Simulates codding decoding of bit packets after passing by a channel additive gaussian noise and transmitter known interference. 
-- Outputs show the number of simulated packets and bit and the number of bit and packet errors. 
-- The value for the standard deviation of the gaussian noise is controlled in real time by an additional input. 
-- This component also implements a small part of the decoder, namely multiplication of the received signal by alpha and dither addition. 
-- The transmitted signal, the noise and the interference are all internally generated.
--
-- Paulo Lopes, INESC-ID

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utils.all;

entity DP_codec is
port(
	ok: out std_logic;

    bit_errors_access: out unsigned(31 downto 0);       -- number of bit errors
    packet_errors_access: out unsigned(31 downto 0);    -- number of packet errors
    bits_access: out unsigned(31 downto 0);             -- number of bits
    packets_access: out unsigned(31 downto 0);          -- number of packets
    noise_std_access: in unsigned(31 downto 0);         -- gaussian noise standar deviation in real time and Q2.6 format.
    version: out unsigned(31 downto 0);                 -- 16 bit major | 16 bit minor 

	signal clock: in std_logic);
end DP_codec;

architecture Behavioral of DP_codec is

    constant major_version: natural := 1;
    constant minor_version: natural := 10;

    -- signal for component connections 
    
    signal DP_coder_w_addr_access: l_type;
    signal DP_coder_w_data_in_access: std_logic;
    signal DP_coder_w_data_out_access: std_logic;
    signal DP_coder_w_write_access: std_logic;
    signal DP_coder_w_read_access: std_logic;
    signal DP_coder_s_addr_access: k_type;
    signal DP_coder_s_data_in_access: x_pair_type;
    signal DP_coder_s_write_access: std_logic;
    signal DP_coder_s_read_access: std_logic;
    signal DP_coder_x_addr_access: k_type;
    signal DP_coder_x_data_out_access: x_pair_type;
    signal DP_coder_x_read_access: std_logic;
    signal DP_coder_start: std_logic;
    signal DP_coder_ended: std_logic;
    signal DP_coder_clock: std_logic;
    
    signal DP_decoder_y_address: k_type;
    signal DP_decoder_y_data: y_pair_type;
    signal DP_decoder_y_write: std_logic;
    signal DP_decoder_IM_addr: j_type;
    signal DP_decoder_IM_data_in: log_p_type;
    signal DP_decoder_IM_data_out: log_p_type;
    signal DP_decoder_IM_write: std_logic;
    signal DP_decoder_IM_read: std_logic;
    signal DP_decoder_LVM_addr_access: l_type;
    signal DP_decoder_LVM_data_out_access: lv_type;
    signal DP_decoder_LVM_read_access: std_logic;
    signal DP_decoder_qinv: signed(bl_qinv-1 downto 0);
    signal DP_decoder_start: std_logic;
    signal DP_decoder_ended: std_logic;
    signal DP_decoder_clock: std_logic;
    
    signal s_memory_addr: k_type;
    signal s_memory_data_in: y0_pair_type;
    signal s_memory_data_out: y0_pair_type;
    signal s_memory_data_in_x: std_logic_vector(2*bl_y0-1 downto 0);
    signal s_memory_data_out_x: std_logic_vector(2*bl_y0-1 downto 0);
    signal s_memory_write: std_logic;
    signal s_memory_read: std_logic;
    signal s_memory_clock: std_logic;    
        
    -- auxiliary signals
    
    signal generating_w: std_logic := '1';
    signal generating_s: std_logic := '0';
    signal coding: std_logic := '0';
    signal simulating_channel: std_logic := '0';
    signal erasing_IM: std_logic := '0';
    signal decoding: std_logic := '0';
    signal checking: std_logic := '0';
    
    -- pseudo random number generator
    
    constant mlbs_n: integer := 12;
    type psrn_type is array (0 to mlbs_n-1) of std_logic_vector(bl_psrn-1 downto 0);
    constant psrng: psrn_type := (x"8016", x"801C", x"801F", x"8029", x"805E", x"8097", x"809E", x"80A7", x"80AE", x"80CB", x"80D0", x"80D6");
    constant psrn_seeds: psrn_type := (x"937F", x"08BB", x"231E", x"E669", x"AE25", x"0E53", x"A912", x"DE66", x"A395", x"E0DE", x"3383", x"58DF");
    signal psrn: psrn_type := psrn_seeds;
    
    type gaussian_red_stage2_type is array (0 to mlbs_n/2 - 1) of y0_type;
    signal gaussian_red_stage2: gaussian_red_stage2_type; 
    type gaussian_red_stage3_type is array (0 to mlbs_n/4 - 1) of y0_type;
    signal gaussian_red_stage3: gaussian_red_stage3_type; 
    signal gaussian_value: y0_type;
    signal gaussian_noise0: y0_type;
    signal gaussian_noise1: y0_type;
    
    -- w and s generator
    signal single_input: std_logic := '0';
    signal l: l_type := to_unsigned(0, l_type'length);
    signal k: k_type := to_unsigned(0, k_type'length);
    signal s: y0_pair_type;
    signal s_x_alpha: x_pair_type;
    
    -- channel simulator
    signal cn: unsigned(1 downto 0) := "00";
    signal u: x_type;
    signal psrn_u: unsigned(15 downto 0) := to_unsigned(random_number_seed, bl_psrn);
    signal noise_std: noise_std_type;
    
    -- erasing interlever memory
    signal j: j_type := to_unsigned(0, j_type'length);
    
    -- checking
    signal ok_up_to_now: std_logic := '1';
    signal bit_errors: unsigned(31 downto 0) := to_unsigned(0, 32);
    signal packet_errors: unsigned(31 downto 0) := to_unsigned(0, 32);
    signal bits: unsigned(31 downto 0) := to_unsigned(0, 32);
    signal packets: unsigned(31 downto 0) := to_unsigned(0, 32);
    
begin

    noise_std <= noise_std_type(noise_std_access(bl_noise_std-1 downto 0));
    version <= to_unsigned(major_version, 16) & to_unsigned(minor_version, 16);

    bit_errors_access <= bit_errors;
    packet_errors_access <= packet_errors;
    bits_access <= bits;
    packets_access <= packets;

    DP_coder0: entity work.DP_coder port map (
        w_addr_access => DP_coder_w_addr_access,
        w_data_in_access => DP_coder_w_data_in_access,
        w_data_out_access => DP_coder_w_data_out_access,
        w_write_access => DP_coder_w_write_access,
        w_read_access => DP_coder_w_read_access,
        s_addr_access => DP_coder_s_addr_access,
        s_data_in_access => DP_coder_s_data_in_access,
        s_write_access => DP_coder_s_write_access,
        s_read_access => DP_coder_s_read_access,
        x_addr_access => DP_coder_x_addr_access,
        x_data_out_access => DP_coder_x_data_out_access,
        x_read_access => DP_coder_x_read_access,
        start => DP_coder_start,
        ended => DP_coder_ended,
        clock => clock
    );
    
    DP_coder_w_addr_access <= l;
    DP_coder_w_data_in_access <= single_input;
    DP_coder_w_write_access <= generating_w;
    DP_coder_w_read_access <= '1' when (cn = 0 and checking = '1') else '0';
    DP_coder_s_addr_access <= k;
    DP_coder_s_data_in_access <= s_x_alpha;
    DP_coder_s_write_access <= '1' when generating_s = '1' and cn = 1 else '0';
    DP_coder_s_read_access <= '0';
    DP_coder_x_addr_access <= k;
    DP_coder_x_read_access <= '1' when (cn = 0 and simulating_channel = '1') else '0';
        
    DP_decoder0: entity work.DP_decoder port map (
        y_address => DP_decoder_y_address,
        y_data => DP_decoder_y_data,
        y_write => DP_decoder_y_write,
        IM_addr => DP_decoder_IM_addr,
        IM_data_in => DP_decoder_IM_data_in,
        IM_data_out => DP_decoder_IM_data_out,
        IM_write => DP_decoder_IM_write,
        IM_read => DP_decoder_IM_read,
        LVM_addr_access => DP_decoder_LVM_addr_access,
        LVM_data_out_access => DP_decoder_LVM_data_out_access,
        LVM_read_access => DP_decoder_LVM_read_access,
        qinv => DP_decoder_qinv,
        start => DP_decoder_start,
        ended => DP_decoder_ended,
        clock => clock
    );
    DP_decoder_y_address <= k;
    DP_decoder_y_write <= '1' when cn = 2 and simulating_channel = '1' else '0';
    DP_decoder_IM_addr <= j;
    DP_decoder_IM_data_in <= to_signed(0, log_p_type'length);
    DP_decoder_IM_write <= erasing_IM;
    DP_decoder_IM_read <= '0';
    DP_decoder_LVM_addr_access <= l;
    DP_decoder_LVM_read_access <= '1' when (cn = 0 and checking = '1') else '0';
    DP_decoder_qinv <= qinv;
    
    s_memory_data_in_x <= std_logic_vector(s_memory_data_in(0)) & std_logic_vector(s_memory_data_in(1));
    s_memory_data_out_x <= std_logic_vector(s_memory_data_out(0)) & std_logic_vector(s_memory_data_out(1));
    
    s_memory: entity work.BRAM
    generic map(size => tau, word_length => 2 * y0_type'length) 
    port map (
        addr => s_memory_addr,
        data_in => s_memory_data_in_x,
        data_out => s_memory_data_out_x,
        write => s_memory_write,
        read => s_memory_read,
        clock => clock
    );
    
    s(0) <= resize(signed(psrn(3)(bl_y downto 0)), bl_y0);
    s(1) <= resize(signed(psrn(7)(bl_y downto 0)), bl_y0);
    
    s_memory_addr <= k;
    s_memory_data_in <= s;
    s_memory_write <= '1' when generating_s = '1' and cn = 0 else '0';
    s_memory_read <=  '1' when (cn = 0 and simulating_channel = '1') else '0';

    process(clock)
        variable input_aux : std_logic;
        variable psrn_u1: unsigned(15 downto 0);
        variable u: x_type;
        variable u1: x_type;
--        variable linex: line;
    begin
        if rising_edge(clock) then
            
            -- random number generators for input, interference and gaussian noise
            for i in 0 to mlbs_n-1 loop
                psrn(i) <= psrn(i)(14 downto 0) & xor_reduction(psrn(i) and psrng(i));
            end loop;
            
            for i in 0 to mlbs_n/2-1 loop
                gaussian_red_stage2(i) <= resize(signed(psrn(2*i)(bl_y-1 downto 0)), bl_y0) + resize(signed(psrn(2*i+1)(bl_y-1 downto 0)), bl_y0);
            end loop;
            for i in 0 to mlbs_n/4-1 loop
                gaussian_red_stage3(i) <= gaussian_red_stage2(2*i) + gaussian_red_stage2(2*i+1);
            end loop;
            gaussian_value <= gaussian_red_stage3(0) + gaussian_red_stage3(1) + gaussian_red_stage3(2);
            -- the variance of gaussian_value is: 
            --      variance = mlbs_n * ( 2**bl_y )^2 / 12
            -- this simplifies to:
            --      variance = ( 2**bl_y )^2, and std = 2**bl_y 
            gaussian_noise0 <=                                  -- fractional bits = fb_y = 2
                    truncate_left(round(  gaussian_value        -- fractional bits = bl_y = 4 (so that std = 1)
                                        * noise_std             -- fractional bits = fb_noise_std = 6
                                        , fb_noise_std + bl_y - fb_y), bl_y0);
            gaussian_noise1 <= gaussian_noise0;                 -- the gaussian noise signal with the correct variance
            
            if generating_w = '1' then
                input_aux := '0';
                for i in 0 to 11 loop
                    input_aux := input_aux xor psrn(i)(0);
                end loop;
                single_input <= input_aux;
                if l < n_bits - 1 then
                    l <= l + 1;
                else
                    generating_w <= '0';
                    generating_s <= '1';
                    l <= to_unsigned(0, l'length);
                end if;
            elsif generating_s = '1' then
                if cn = 1 then
                    cn <= "00";
                    if k < tau - 1 then
                        k <= k + 1;
                    else
                        k <= to_unsigned(0, k'length);
                        coding <= '1';
                        DP_coder_start <= '1';
                        generating_s <= '0';
                    end if;
                else
                    s_x_alpha(0) <= truncate_left(round(alpha*s(0), bl_alpha-1), bl_y);
                    s_x_alpha(1) <= truncate_left(round(alpha*s(1), bl_alpha-1), bl_y);
                    cn <= "01";
                end if;
                
            elsif coding = '1' then
                DP_coder_start <= '0';
                if DP_coder_ended = '1' then
                    simulating_channel <= '1';
                    coding <= '0';
                end if;
            elsif simulating_channel = '1' then
                if cn < 2 then
                    cn <= cn + 1;
                else
                    cn <= "00";
                    if k < tau - 1 then 
                        k <= k + 1;
                    else
                        k <= to_unsigned(0, k'length);
                        erasing_IM <= '1';
                        simulating_channel <= '0';
                        psrn_u <= to_unsigned(random_number_seed, psrn_u'length);
                    end if;
                end if;
                if cn = 1 then
                    psrn_u1 := psrn_u(14 downto 0) & (psrn_u(15) xor psrn_u(14) xor psrn_u(12) xor psrn_u(3));
                    psrn_u <= psrn_u1(14 downto 0) & (psrn_u1(15) xor psrn_u1(14) xor psrn_u1(12) xor psrn_u1(3));
                    u := x_type(psrn_u(u'length-1 downto 0));
                    u1 := x_type(psrn_u1(u'length-1 downto 0));
 
                    -- y = u + alpha * (x + s + n)
                    DP_decoder_y_data(0) <= u + truncate_left(round(alpha*(
                            DP_coder_x_data_out_access(0) + s_memory_data_out(0) + gaussian_noise0
                        ), bl_alpha -1), bl_y);
                    DP_decoder_y_data(1) <= u1 + truncate_left(round(alpha*(
                            DP_coder_x_data_out_access(1) + s_memory_data_out(1) + gaussian_noise1
                        ), bl_alpha -1), bl_y);
                end if;
            elsif erasing_IM = '1' then
                if j < interleaver_length - 1 then
                    j <= j + 1;
                else
                    j <= to_unsigned(0, j'length);
                    decoding <= '1';
                    erasing_IM <= '0';
                    DP_decoder_start <= '1';
                end if;
            elsif decoding = '1' then
                DP_decoder_start <= '0';
                if DP_decoder_ended = '1' then
                    checking <= '1';
                    decoding <= '0';
                end if;
            elsif checking = '1' then
                if cn = 1 then
                    cn <= "00";
                    if l < n_bits - 1 then
                        bits <= bits + 1;
                        if DP_coder_w_data_out_access /= DP_decoder_LVM_data_out_access(DP_decoder_LVM_data_out_access'left) then
                            ok_up_to_now <= '0';
                            bit_errors <= bit_errors + 1;
                        end if;
                        l <= l + 1;
                    else
                        ok <= ok_up_to_now;
                        packets <= packets + 1;
                        if ok_up_to_now = '0' then
                            packet_errors <= packet_errors + 1; 
                        end if;
                        ok_up_to_now <= '1';
                        generating_w <= '1';
                        checking <= '0';
                    end if;
                else
                    cn <= "01";
                end if;
            end if;
        end if;
    end process;
        
end Behavioral;