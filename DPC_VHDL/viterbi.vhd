library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity viterbi is
	generic(
		data_size: integer; -- number of input samples
		generator1: std_logic_vector;
		generator2: std_logic_vector);
	port(
	    x_data_in: out x_pair_type;
        x_data_out: in x_pair_type;
        x_addr: out k_type;
        x_read: out std_logic := '0';
        x_write: out std_logic := '0';
		start: in std_logic;
		ended: out std_logic;
		clock: in std_logic);
end viterbi;

	architecture behavior of viterbi is

	constant code_memory: integer := generator1'length - 1;
	constant n_states: integer := 2**code_memory; 
	-- the maximum metrix is (K-1)v = code_memory*3
	constant metric_length : integer := length(code_memory*3) + bl_x - 1; -- bl_x - 1 fractional bits
	constant i_bl: integer := length(data_size/2-1);

	subtype metric_type is signed(metric_length downto 0); -- one more bit for the sign 
	subtype state_bv_type is unsigned(code_memory-1 downto 0);
	subtype metric_ext_type is signed(metric_length+1 downto 0);
	type metrics_ext_type is array (0 to n_states-1) of metric_ext_type;
	type metric_ext_vector is array (NATURAL range <>) of metric_ext_type;
	subtype i_type is unsigned(i_bl-1 downto 0);

	procedure viterbi_BP_processor(
		variable data: in x_pair_type;
		variable state: in state_bv_type;
		variable metric0aux: in metric_type;
		variable metric1aux: in metric_type;
		variable termination: in boolean;
		variable decision: out std_logic;
		variable metric: out metric_ext_type) is

		variable metric0: metric_ext_type;
		variable metric1: metric_ext_type;
		variable output1_in0, output2_in0, output1_in1, output2_in1 : std_logic;
	begin
		output1_in0 := xor_reduction(generator1 and std_logic_vector('0' & state));
		output2_in0 := xor_reduction(generator2 and std_logic_vector('0' & state));
		
		metric0 :=  signed(-- cannot add signed with unsigned directly
                    unsigned(square(output1_in0 & to_signed(0, bl_x-1) - data(0),1)) + 
                    unsigned(square(output2_in0 & to_signed(0, bl_x-1) - data(1),1)) +
		            -- the difference is mod 2 and output is unsigned from 0 to 1 inclusive with bl_x-1 fractional bits
                    unsigned(resize(metric0aux, metric_length+2)));
					
		output1_in1 := xor_reduction(generator1 and std_logic_vector('1' & state));
		output2_in1 := xor_reduction(generator2 and std_logic_vector('1' & state));

		metric1 :=  signed(-- cannot add signed with unsigned directly
                    unsigned(square(output1_in1 & to_signed(0, bl_x-1) - data(0),1)) + 
                    unsigned(square(output2_in1 & to_signed(0, bl_x-1) - data(1),1)) +
		            -- the difference is mod 2 and output is unsigned from 0 to 1 inclusive with bl_x-1 fractional bits
                    unsigned(resize(metric1aux, metric_length+2)));

		if (metric0 <= metric1 or termination) then
			metric := metric0;
			decision := '0';
		else
			metric := metric1;
			decision := '1';
		end if;
	end procedure;
	
	type metrics_type is array (0 to n_states-1) of metric_type;
	signal metrics: metrics_type -- metrics(s): metric at the previous iteration (after time i)
		:= (others => (others => '0'));
	signal fp_state: state_bv_type;
	
	signal i: i_type := (others=>'0');
	signal backward_pass: std_logic := '0';
	signal forward_pass: std_logic := '0';
	signal int_ended: std_logic := '0';
	signal counter: unsigned(1 downto 0);
	signal metrics_ext: metrics_ext_type;
	
	-- decisions memory
	-- decision(i,s): decision for the state s at time instante i
	signal decisions_memory_data_in: std_logic_vector(n_states-1 downto 0);
	signal decisions_memory_data_out: std_logic_vector(n_states-1 downto 0);
    signal decisions_memory_write: std_logic := '0';
    signal decisions_memory_read: std_logic := '0';
	
begin
    
    x_addr <= i;
    
    decisions_memory: entity work.BRAM_LV
        generic map (size => data_size/2, word_length => n_states) 
        port map (addr => i,
           data_in => decisions_memory_data_in,
           data_out => decisions_memory_data_out,
           write => decisions_memory_write,
           read => decisions_memory_read,
           clock => clock);
    
	process(clock)
        variable data_i: x_pair_type;
		variable metric0aux, metric1aux : metric_type;
		variable state_bv: state_bv_type;
		variable termination: boolean;
		variable decision_out: std_logic;
		variable metric_out: metric_ext_type;
		variable metric_state0: metric_ext_type;
		variable metrics_diff: metric_ext_type;
	begin
		if rising_edge(clock) then
			if start='1' then
				forward_pass <= '0';
				backward_pass <= '1';
				int_ended <= '0';
				metrics <= (others=>(others=>'0'));
				counter <= to_unsigned(0, counter'length);
				decisions_memory_write <= '0';
                decisions_memory_read <= '0';

				-- backward pass initializations
				i <= to_unsigned(data_size/2-1, i'length);
				x_read <= '1';

			------------------- backward pass
			elsif backward_pass = '1' then
				
				if counter = 0 then
				    -- read x_data: x_data_out <= ...
				    counter <= to_unsigned(1, counter'length);
                    x_read <= '0';

				elsif counter = 1 then
				    -- calculates new metrics and decisions
				    counter <= to_unsigned(2, counter'length);
				    data_i := x_data_out;
				    
				    for state in 0 to n_states-1 loop
                        state_bv := to_unsigned(state, code_memory);
                    
                        metric0aux := metrics(
                            to_integer('0' & state_bv(state_bv'left downto 1)));
                        metric1aux := metrics(
                            to_integer('1' & state_bv(state_bv'left downto 1)));
                            
                        termination := (i >= data_size/2 - code_memory - 1);
                        viterbi_BP_processor(data_i, state_bv, metric0aux, metric1aux, 
                                termination , decision_out, metric_out);
                        
                        decisions_memory_data_in(state) <= decision_out;
                        metrics_ext(state) <= metric_out;
                    end loop;
                    decisions_memory_write <= '1';

				else -- counter = 2
				    -- write decisions to memory and normalize metrics
				    counter <= to_unsigned(0, counter'length);
				    decisions_memory_write <= '0';

                    metric_state0 := metrics_ext(0);
                    for state in 0 to n_states-1 loop
                        metrics_diff := metrics_ext(state) - metric_state0;
                        metrics(state) <= metrics_diff(metric_length downto 0);
                    end loop;

                    x_read <= '1'; -- to read the input BRAM at counter = 0

                    if i = 0 then 
                        backward_pass <= '0';
                        forward_pass <= '1';
                        
                        -- forward pass initializations
                        i <= to_unsigned(0, i_bl);
                        fp_state <= (others => '0');
                        counter <= to_unsigned(0, counter'length);
                        decisions_memory_read <= '1';
                        -- x_read is already at 1
                    else 
                        i <= i - 1;
                    end if;
                    
				end if;
				
			------------------- forward pass
			elsif (forward_pass='1') then
                if counter = 0 then
                    -- read decisions and old x
   				    counter <= to_unsigned(1, counter'length);
                    decisions_memory_read <= '0';
                    x_read <= '0';

                elsif counter = 1 then
                    -- calculate new x and update state
   				    counter <= to_unsigned(2, counter'length);
                    decision_out := decisions_memory_data_out(to_integer(fp_state));
                    x_data_in(0) <=  (xor_reduction(generator1 and std_logic_vector(decision_out & fp_state)) 
                                        xor x_data_out(0)(bl_x-1)) & x_data_out(0)(bl_x-2 downto 0);
		            x_data_in(1) <=  (xor_reduction(generator2 and std_logic_vector(decision_out & fp_state))
		                                xor x_data_out(1)(bl_x-1)) & x_data_out(1)(bl_x-2 downto 0);
				    fp_state <= decision_out & fp_state(fp_state'left downto 1);
				    x_write <= '1';

				else -- counter = 2
                    -- store x and update i
   				    counter <= to_unsigned(0, counter'length);
				    x_write <= '0';
                    if i = data_size/2 - 1 then
                        int_ended <= '1';
                        forward_pass <= '0';
                    else
                        decisions_memory_read <= '1';
                        x_read <= '1';
                        i <= i + 1;
                    end if;
                end if;			 

			end if;
			if (int_ended='1') then
				int_ended <= '0';
			end if;
		end if;
	end process;
	
	ended <= int_ended;
end;