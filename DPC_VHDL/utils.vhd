-- utils 11-9-2019
--
-- Several  constants and utility functions to use in the project.
--
-- Paulo Lopes, INESC-ID

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use ieee.math_real.all;

package utils is
    -- parameters
    constant EbN0: real := 3.0;                     -- design EbN0, the codec will also work for higher values
    constant bl_y: natural := 6;                    -- bit length of decoder input signal y
    constant bl_log_p: natural := 8;                -- bit length of the decoder log of probability signals (LLR and others)
    
    -- Other fixed codec parameters
    constant lscm: natural := 2;                    -- lattice_shapping_code_memory 
    constant g1: std_logic_vector(lscm downto 0) := "101";      -- generator polinomial 1
    constant g2: std_logic_vector(lscm downto 0) := "111";      -- generator polinomial 2
    constant n_bits: integer := 975;                -- number of input bits per packet/block (k in a rate k/n block code)  
    constant cnd3: integer := 1171;                 -- number of order 3 check nodes (CNDs) 
    constant cnd1: integer := 4679;                 -- number of order 1 check nodes (CNDs)
    constant n_VND_dg: integer := 4;                -- number of variabel nodes (VNDs) types
    type VND_tables_type is array (0 to n_VND_dg-1) of integer;
    constant VND: VND_tables_type := (627, 305, 42, 1);         -- number of VNDs of each type
    constant VND_dg: VND_tables_type := (3, 10, 76, 69);        -- degres of each type of VND
    constant n_group_input_bits: integer := 3;      -- number of input bits processed together by the decoder 
    constant n_group_samples: integer := 2;         -- number of samples processed together by the decoder
    constant n_termination_bits: integer := 9;      -- number of termination bits
    constant n_bits_per_symbol: integer := 2;       -- number of bits per symbol
    constant n_PAM: natural := 2**n_bits_per_symbol;        -- Number of nevels of PAM modulation
    constant sgdb: real := 0.98;                            -- Lattice shaping gain in dBs
    
    -- Implemantation specific constans
    constant cycle_length : natural := 22;           -- the number of clock cycles required to process a 2 samples / 3 bits group by the BCJR algorithm of the decoder
    constant random_number_seed : natural := 2623;   -- the seed of the random number generator used for the dither at the coder and decoder
    constant normalization_value_bit_reduction: integer := 3; -- beta and alpha normalization value bit reduction
    
    -- Other constants
    constant Sx: real := (real(n_PAM)/2.0)**2.0 * 2.0 * 10.0**(-sgdb/10.0)/12.0;            -- Signal Power
    constant Nx: real := 2.0*Sx/10.0**(EbN0/10.0);                                          -- Noise power
    constant n_states : integer := 2**(lscm+1);                 -- number of lattice code states
    constant n_inputs : integer := 2**n_group_input_bits;       -- number of input bit combinations
    constant n_acc_bits : integer := n_bits*6+9;                -- number of accumulator bits
    constant n_samples : integer := n_acc_bits/3*2;             -- total number of samples per block
    constant tau: integer := n_acc_bits/3;                      -- number of sample groups (tau of the BCJR algorithm)
    constant interleaver_length: integer := 3*cnd3 + cnd1;      -- interleaver length
    
	-- length : Calculates the number of bits required to store i using log2.
	function length(i : natural) return integer;

    -- bit length of several signals
    -- bl: bit (total) length 
    -- fb: fractional part length
    -- ib: fractional part length
    
    -- bit length of several coder signals

    constant bl_x : integer := bl_y;
    subtype x_type is signed(bl_x-1 downto 0);
    type x_pair_type is array (0 to 1) of x_type;
    
    -- the number of bits required the store the greathest variable node degree
    constant bl_vnd_dg : integer := 8;
    constant bl_lv: integer := bl_log_p + bl_vnd_dg;
    subtype lv_type is signed(bl_lv-1 downto 0);
    constant bl_l: integer := length(n_bits);
    subtype l_type is unsigned(bl_l-1 downto 0);
    
    constant bl_k: integer := length(tau-1);
    subtype k_type is unsigned(bl_k-1 downto 0);
    constant bl_j: integer := length(interleaver_length-1);
    subtype j_type is unsigned(bl_j-1 downto 0);
    
    -- bit length of several channel signals
    
    -- extra bits for y0 to prevent saturation of the AD due to the interference that could result in a non-linear channel
    constant xb_y0: integer := 4;                
    constant bl_y0: integer := bl_y + xb_y0;
    subtype y0_type is signed(bl_y0-1 downto 0);
    type y0_pair_type is array (0 to 1) of y0_type;

    -- bit length of several decoder signals

    constant ib_y: integer := 2;        -- one sign bit and one integer bit since -2 < y < 2
    constant fb_y: integer := bl_y - ib_y;
    subtype y_type is signed(bl_y-1 downto 0);

    constant bl_y_sq: integer := bl_y;
    constant ib_y_sq: integer := 3;     -- the maximum value of y^2 is 4 and y_sq is unsigned
    constant fb_y_sq: integer := bl_y_sq - ib_y_sq;
    subtype y_sq_type is signed(bl_y_sq-1 downto 0);  -- TODO: change to unsigned  
    
    constant bl_qinv: integer := bl_y;
    constant ib_qinv: integer := 3;   -- qinv = -1/(2*q_VQ)
    -- we use 2 integer bits (plus sign) since qinv>= 1.5452 (Eb/No=1.1 dB) so values up to 4 are enough
    constant fb_qinv: integer := bl_qinv - ib_qinv;
    subtype qinv_type is signed(bl_qinv-1 downto 0);  
        
    constant bl_noise_std: integer := 8;
    constant fb_noise_std: integer := 6;
    subtype noise_std_type is signed(bl_noise_std-1 downto 0);
    
    constant bl_psrn: integer := 16;  

    constant ib_log_p: integer := 6;    -- allow at least -20 to +20 (note that - qinv*y^2 < 16) 
    constant fb_log_p: integer := bl_log_p - ib_log_p;
    subtype log_p_type is signed(bl_log_p-1 downto 0);
    constant minimum_of_log_p_type: integer := -2**(bl_log_p-1);        -- approximation to -inf

    constant bl_alpha: integer := 8;    -- this will results in bl_alpha x bl_y0 multiplier  
    constant fb_alpha: integer := 7;      
    subtype alpha_type is signed(bl_alpha-1 downto 0);

    -- additional constants
    constant alpha_real: real := Sx/(Sx+Nx);
    constant q_VQ_real: real := (1.0-alpha_real)**2.0 * Sx + alpha_real**2.0 * Nx;
    constant qinv_real: real := -1.0/(2.0*q_VQ_real);
    constant alpha: alpha_type := to_signed(integer(round(alpha_real * real(2**fb_alpha))), bl_alpha);
    constant qinv: qinv_type := to_signed(integer(round(qinv_real * real(2**fb_qinv))), bl_qinv); 
    
    -- max1 correction table
	constant max1_table_size: integer := 2 * 4 * 2**fb_log_p;
    type max1_table_type is array (0 to max1_table_size-1) of log_p_type;
    function generate_max1_table return max1_table_type;
    constant max1_table: max1_table_type := generate_max1_table;
    
    -- some types

    type y_pair_type is array (0 to n_group_samples-1) of y_type;
    type llr_data_type is array (0 to n_group_input_bits-1) of signed(bl_log_p-1 downto 0);
    subtype beta_address_type is unsigned(length(tau)-1 downto 0);
    type beta_data_type is array (0 to n_states-1) of signed(bl_log_p-1 downto 0);
    type viterbi_ram_type is array (0 to tau-1) of x_type;
    
    -- utility functions
    
    procedure print(arg : in string);
        
	function to_string(x: std_logic_vector) return string;
	function to_string(x: signed) return string;
	function to_string(x: unsigned) return string;
	
	function check(
		signal a: in std_logic_vector;
		signal b: in std_logic_vector
		) return boolean;

	-- log2 : Calculates floor(log2(i)).
    function log2(i : natural) return integer;
	
	function xor_reduction(A: std_logic_vector) return std_logic;
	
	function to_unsigned(A: std_logic) return unsigned;
	function to_unsigned(A: std_logic_vector) return unsigned;
    
    function xor1 (L: signed; R: std_logic) return signed;
    function plus1 (L: signed; R: std_logic) return signed;
    function to_integer (L: std_logic) return integer;
    function times(L, R: signed; sl: integer) return signed;
    function square(L: signed; sl: integer) return signed;
    function saturating_add(L, R: signed) return signed;
	function saturating_sub(L, R: signed) return signed;
    function non_saturating_add(L, R: signed) return signed;
    function non_saturating_sub(L, R: signed) return signed;
    function round(X: signed; n: natural) return signed;
    function truncate_left(X: signed; n: natural) return signed;

    function read_max1_table(X: signed) return signed;
    function max1_aux(R: log_p_type) return log_p_type;
    function max1(L, R: log_p_type) return log_p_type;
    function change_fb(a: signed; fb_in, fb_out, bl_out: integer) return signed;
    
    function saturating_resize(x: signed; new_length: integer) return signed;
    function saturating_twos_complement(x: signed; complement: std_logic) return signed;
    function saturate(x: signed; at_bit_length: integer) return signed;

    procedure READ(L:inout LINE; R:out signed);
    procedure WRITE(L:inout LINE; R:in signed);
    
end utils;

package body utils is

    procedure print(arg : in string) is
    begin
      std.textio.write(std.textio.output, arg);
    end procedure print;

    function generate_max1_table return max1_table_type is
        variable x: real;
        variable y: real;
        variable table: max1_table_type;
    begin
        for i in 0 to max1_table_size-1 loop
            if i >=  max1_table_size / 2 then
                x := real(i) * 8.0 / real(max1_table_size) - 8.0;
            else
                x := real(i) * 8.0 / real(max1_table_size);
            end if;
            
            y := log(1.0 + exp(-abs(x)));
            table(i) := to_signed(integer(round(y * real(2**fb_log_p))), bl_log_p);
        end loop;
        return table;
    end;

	function to_string(x:std_logic_vector) return string is
		variable str: string(1 to x'length);
		variable element : string(1 to 3);
		variable j: integer := 1;
	begin
		for i in x'range loop
			element := std_logic'image(x(i));
			str(j) := element(2);
			j := j + 1;
		end loop;
		return str;
	end;
	
	
	function to_string(x: signed) return string is
	begin
	   return to_string(std_logic_vector(x));
	end;

	function to_string(x: unsigned) return string is
	begin
	   return to_string(std_logic_vector(x));
	end;
	
	function check(
		signal a: in std_logic_vector;
		signal b: in std_logic_vector
		) return boolean is
		
		constant input_size: integer := a'length;
		variable result: boolean := true;
	begin
		for i in 0 to input_size-1 loop
			if a(i) /= b(i) then 
				result := false;
			end if;
		end loop;
		return result;
	end function;
	
	-- log2 : Calculates floor(log2(i)).
    function log2(i : natural) return integer is
        variable temp    : integer := i;
        variable ret_val : integer := 0;
    begin                    
        while temp >= 2 loop
          ret_val := ret_val + 1;
          temp    := temp / 2;     
        end loop;
        
        return ret_val;
    end function;
	
	-- length : Calculates the number of bits required to store i using log2.
	function length(i : natural) return integer is
		variable temp    : integer := i;
		variable ret_val : integer := 0; 
	begin
        while temp > 1 loop
          ret_val := ret_val + 1;
          temp    := temp / 2;     
        end loop;
        return ret_val + 1;
    end function;

	function xor_reduction(A: std_logic_vector) return std_logic is
		variable out_data : std_logic := '0';
	begin
		for i in A'low to A'high loop
			out_data := out_data xor A(i);
		end loop;
		return out_data;
	end;
	
	function to_unsigned(A: std_logic) return unsigned is
		variable out_data :unsigned(0 downto 0);
	begin
		out_data(0) := A;
		return out_data;
	end;
	
	function to_unsigned(A: std_logic_vector) return unsigned is
		variable out_data :unsigned(A'length-1 downto 0);
	begin
		out_data := unsigned(A);
		return out_data;
	end;
	
	function xor1 (L: signed; R: std_logic) return signed is
	   variable Rx: std_logic_vector(L'length-1 downto 0);
	begin
	   Rx := (others => R);
	   return signed(std_logic_vector(L) xor Rx);
	end xor1;

	function plus1 (L: signed; R: std_logic) return signed is
	   variable Rx: signed(L'length-1 downto 0);
	begin
	   Rx(0) := R;
	   Rx(L'length-1 downto 1) := (others => '0');
	   return L + Rx;
	end plus1;
	
	function to_integer (L: std_logic) return integer is
	begin
	   if L='1' then
	       return 1;
	   else
	       return 0;
	   end if;
    end to_integer;
	
	function times(L, R: signed; sl: integer) return signed is
       variable aux: signed(L'length+R'length-1 downto 0);
       variable aux2: signed(R'length downto 0); -- one extra bit
    begin
       -- The length of the result id equal to R'length
       aux := shift_left(L * R, sl);
       aux2 := plus1(aux(L'length+R'length-1 downto L'length-1), '1');
       return aux2(R'length downto 1);
    end times;
    
    function square(L: signed; sl: integer) return signed is
    begin
        return times(L, L, sl); 
    end square;

    function MAX(LEFT, RIGHT: INTEGER) return INTEGER is
    begin
      if LEFT > RIGHT then return LEFT;
      else return RIGHT;
      end if;
    end;
    
    function MIN(LEFT, RIGHT: INTEGER) return INTEGER is
    begin
      if LEFT < RIGHT then return LEFT;
      else return RIGHT;
      end if;
    end;
   
	function saturating_add(L, R: signed) return signed is
    begin
       return saturating_resize(
            resize(L, L'length+1) + 
            resize(R, R'length+1),
            max(L'length, R'length)
        );
    end saturating_add;
    
	function saturating_sub(L, R: signed) return signed is
    begin
       return saturating_resize(
            resize(L, L'length+1) - 
            resize(R, R'length+1),
            max(L'length, R'length)
        );
    end saturating_sub;

    function non_saturating_add(L, R: signed) return signed is
        constant size : natural := max(L'length, R'length) + 1;
    begin
        return resize(L,size) + resize(R,size); 
    end;

    function non_saturating_sub(L, R: signed) return signed is
        constant size : natural := max(L'length, R'length) + 1;
    begin
        return resize(L,size) - resize(R,size); 
    end;

    procedure READ(L:inout LINE; R:out signed) is
        variable aux: std_logic_vector(R'length-1 downto 0);
    begin
        read(L, aux);
        R := signed(aux);
    end read;
		
    procedure WRITE(L:inout LINE; R:in signed) is
        variable aux: std_logic_vector(R'length-1 downto 0);
    begin
        aux := std_logic_vector(R);
        write(L, aux);
    end WRITE;

    function read_max1_table(X: signed) return log_p_type is
        constant table_address_bit_length : integer := length(max1_table_size-1);
        variable sign_bits: signed (X'left downto table_address_bit_length) := (others => X(table_address_bit_length-1));
    begin
        if X(X'left downto table_address_bit_length) = sign_bits then
            return max1_table(to_integer(unsigned(X(table_address_bit_length-1 downto 0))));
        else
            return to_signed(0, log_p_type'length);
        end if;
    end read_max1_table;
   
    -- -max1(0,-x)
    -- This function is easier to implement than -max(0,x) since it does not complement the input 
    function max1_aux(R: log_p_type) return log_p_type is
    begin
        if R > 0 then
            return -read_max1_table(R);
        else
            return saturating_sub(R, read_max1_table(R));
        end if;
    end max1_aux;
    
    -- max1(a,b)
    function max1(L, R: log_p_type) return log_p_type is
        variable dif: signed(max(L'length,R'length) downto 0);
    begin
        dif := non_saturating_sub(L,R);
        if dif > 0 then
            return saturating_add(L, read_max1_table(dif));
        else
            return saturating_add(R, read_max1_table(dif));
        end if;
    end max1;
    
    function change_fb(a: signed; fb_in, fb_out, bl_out: integer) return signed is 
        variable left: integer := bl_out - 1 + fb_in - fb_out;
        variable rigth: integer := fb_in - fb_out;
        variable x: signed(a'length-1 downto 0) := a;
        variable aux0: signed(max(-rigth-1, 0) downto 0) := (others => '0');
        variable aux1: signed(max(left - x'left - 1, 0) downto 0) :=  (others => x(x'left));
    begin
        if rigth > 0 then
            x := x + 1; -- rounding
        end if; 
    
        if  left <= x'left and rigth >= 0 then
            return x(left downto rigth);
        elsif left <= x'left and rigth < 0 then
            return x(left downto 0) & aux0;
        elsif left > x'left and rigth >= 0 then
            return aux1 & x(x'left downto rigth);
        else -- left > x'left and rigth < 0
            return aux1 & x(x'left downto 0) & aux0;
        end if;
    end change_fb;

    function saturating_resize(x: signed; new_length: integer) return signed is
    begin
        assert(x'length - 1 = x'left); assert(0 = x'right);
        if x'length <= new_length then
            return resize(x, new_length); 
        else
            if x(x'length-1) = '0' and 
                x(x'length-1 downto new_length-1) /= 0 then 
                    -- positive saturation
                    return to_signed(2**(new_length-1)-1, new_length);
            elsif x(x'length-1) = '1' and 
                x(x'length-1  downto new_length-1) /= -1 then 
                    -- negative saturation
                    return to_signed(-(2**(new_length-1))+1, new_length);
                   -- Carefull negative saturation should not be at -2**(new_length-1) because 
                   -- the 2'c complement of this value is not positive.
                   -- Note that, however, inputs values equal to -2**(new_length-1) are not detected.
            else 
                return x(new_length-1 downto 0);
            end if;
        end if;
    end saturating_resize;
    
    function saturating_twos_complement(x: signed; complement: std_logic) return signed is
    begin
        if x = -(2**(x'length-1)) and complement = '1' then
            -- saturate
            return to_signed(2**(x'length-1) - 1, x'length);
        else
            return plus1(xor1(x, complement), complement);
        end if;
    end;    

    function saturate(x: signed; at_bit_length: integer) return signed is
    begin
        return resize(saturating_resize(x, at_bit_length), x'length);
    end;
    
    function round(X: signed; n: natural) return signed is
        variable aux: signed(X'length-n downto 0);
        variable res: signed(X'length-1-n downto 0);
    begin
        aux := X(X'length-1 downto n-1) + 1;
        res := aux(aux'left downto 1);
        return res;
    end;

    function truncate_left(X: signed; n: natural) return signed is
    begin
        return X(n-1 downto 0); 
    end;

end utils;
