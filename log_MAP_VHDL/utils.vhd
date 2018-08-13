-- utils v1.3 25-7-2018
-- Changing the max1 functions to use a table half the size, and make it more natural.
--
-- utils v1.2 4-7-2018
-- Increasing the word size of some signals.
--
-- utils v1.1
-- changed the order of the indexes of gamma.
--
-- Paulo Lopes, INESC-ID
--
-- utils 1.0
-- Several utils and constants to use in the project.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use work.some_constants.all;

package utils is
    constant g1: std_logic_vector(2 downto 0) := "101";
    constant g2: std_logic_vector(2 downto 0) := "111";
    
    constant n_states : integer := 8;
    constant n_input_bits : integer := 3;
    constant n_inputs : integer := 2**n_input_bits;
    constant n_samples_group : integer := 2;
    constant n_termination_bits : integer := 9;

    -- fb: fractional part length
    -- ib: fractional part length
    -- bl: bit (total) length 

    constant ib_log_p: integer := 6;    -- allow at least -20 to +20
    constant fb_log_p: integer := bl_log_p - ib_log_p;
    -- The maximum value of y^2 is 4 and qinv*y^2 is 16 
    -- subtype log_p_type is signed(bl_log_p-1 downto 0);
    constant minimum_of_log_p_type: integer := -2**(bl_log_p-1);
    
    constant ib_qinv: integer := 3;   -- qinv = -1/(2*q_VQ)
    -- we use 2 integer bits (plus sign) since qinv>= 1.5452 (Eb/No=1.1 dB) so values up to 4 are enough
    constant fb_qinv: integer := bl_qinv - ib_qinv;
    subtype qinv_type is signed(bl_qinv-1 downto 0);  
    
    constant ib_y_sq: integer := 3;
    constant fb_y_sq: integer := bl_y_sq - ib_y_sq;
    subtype y_sq_type is signed(bl_y_sq-1 downto 0);  
    
    constant ib_y: integer := 2;      -- one sign bit and one integer bit since -2<y<2
    constant fb_y: integer := bl_y - ib_y;
    -- the same total bits to have the same relative error as LLR
    subtype y_type is signed(bl_y-1 downto 0);  
    
    -- note that all the values are signed 8 bits
    -- y_sq <- shift left high of y*y (integer part = 3 = 2 + 2 - 1)  
    -- llr (log_gamma) <- high of qinv*y (integer part = 6 = 3 + 3, no shift)
    
    constant n_acc_bits : integer := n_bits*6+9;
    constant n_samples : integer := n_acc_bits/3*2;
    constant tau: integer := n_acc_bits/3;
            
    type y_mem_type is array (0 to n_samples/2-1) of y_type; -- two banks
    type LLR_mem_type is array (0 to tau-1) of log_p_type; -- three banks
    type gamma_ext_mem_type is array (0 to 2**n_input_bits-1, 0 to 1, 0 to tau-1) of log_p_type;
    -- the state is unsiged(2 downto 0) : c_acc(n-1) & v(n-1) & v(n-2)
    
	function to_string(x: std_logic_vector) return string;
	
	function check(
		signal a: in std_logic_vector;
		signal b: in std_logic_vector
		) return boolean;

	-- log2 : Calculates floor(log2(i)).
    function log2(i : natural) return integer;

	-- length : Calculates the number of bits required to store i using log2.
	function length(i : natural) return integer;
	
	function xor_reduction(A: std_logic_vector) return std_logic;
	
	function to_unsigned(A: std_logic) return unsigned;
	
	function to_unsigned(A: std_logic_vector) return unsigned;

	constant input_size : integer := 1024;
	
	subtype addr_type is unsigned(length(input_size-1)-1 downto 0);
    
    type BRAM1_interface is
    record 
        addr : addr_type; 
        data_in : std_logic;
        data_out : std_logic;
        wr : std_logic;
    end record;
    
    type BRAM2_interface is
    record 
        addr : addr_type;
        data_in : std_logic_vector(1 downto 0);
        data_out : std_logic_vector(1 downto 0);
        wr : std_logic;
    end record;
    
    function xor1 (L: signed; R: std_logic) return signed;
    function plus1 (L: signed; R: std_logic) return signed;
    function to_integer (L: std_logic) return integer;
    function times(L, R: signed; sl: integer) return signed;
    function saturating_add(L, R: signed) return signed;
	function saturating_sub(L, R: signed) return signed;
    
    -- constant table_size: integer := 32;
    -- type max1_table_type is array (0 to table_size-1) of log_p_type;
    -- table size 128, ib_log_p=6 fb_log_p=10
    -- constant max1_table: max1_table_type := ("1111110100111010", "1111110101011010", "1111110101111000", "1111110110010110", "1111110110110010", "1111110111001110", "1111110111101000", "1111111000000010", "1111111000011011", "1111111000110010", "1111111001001001", "1111111001011111", "1111111001110100", "1111111010001000", "1111111010011011", "1111111010101110", "1111111010111111", "1111111011010000", "1111111011100000", "1111111011101111", "1111111011111110", "1111111100001100", "1111111100011001", "1111111100100110", "1111111100110010", "1111111100111101", "1111111101001000", "1111111101010010", "1111111101011100", "1111111101100101", "1111111101101110", "1111111101110110", "1111111101111110", "1111111110000101", "1111111110001100", "1111111110010011", "1111111110011001", "1111111110011111", "1111111110100101", "1111111110101010", "1111111110101111", "1111111110110100", "1111111110111000", "1111111110111101", "1111111111000001", "1111111111000100", "1111111111001000", "1111111111001011", "1111111111001110", "1111111111010001", "1111111111010100", "1111111111010111", "1111111111011001", "1111111111011011", "1111111111011110", "1111111111100000", "1111111111100010", "1111111111100011", "1111111111100101", "1111111111100111", "1111111111101000", "1111111111101010", "1111111111101011", "1111111111101100", "1110111111101101", "1111000000101100", "1111000001101011", "1111000010101010", "1111000011101000", "1111000100100111", "1111000101100101", "1111000110100011", "1111000111100010", "1111001000100000", "1111001001011110", "1111001010011011", "1111001011011001", "1111001100010111", "1111001101010100", "1111001110010001", "1111001111001110", "1111010000001011", "1111010001001000", "1111010010000100", "1111010011000001", "1111010011111101", "1111010100111000", "1111010101110100", "1111010110101111", "1111010111101010", "1111011000100101", "1111011001011111", "1111011010011001", "1111011011010011", "1111011100001100", "1111011101000101", "1111011101111110", "1111011110110110", "1111011111101110", "1111100000100101", "1111100001011100", "1111100010010010", "1111100011001000", "1111100011111101", "1111100100110010", "1111100101100110", "1111100110011001", "1111100111001100", "1111100111111110", "1111101000101111", "1111101001100000", "1111101010010000", "1111101010111111", "1111101011101110", "1111101100011011", "1111101101001000", "1111101101110100", "1111101110011111", "1111101111001001", "1111101111110010", "1111110000011011", "1111110001000010", "1111110001101000", "1111110010001110", "1111110010110010", "1111110011010110", "1111110011111000", "1111110100011010");
    -- table size 32, ib_log_p=6 fb_log_p=10
    -- constant max1_table: max1_table_type := ("1111110100111010", "1111110110110010", "1111111000011011", "1111111001110100", "1111111010111111", "1111111011111110", "1111111100110010", "1111111101011100", "1111111101111110", "1111111110011001", "1111111110101111", "1111111111000001", "1111111111001110", "1111111111011001", "1111111111100010", "1111111111101000", "1110111111101101", "1111000011101000", "1111000111100010", "1111001011011001", "1111001111001110", "1111010011000001", "1111010110101111", "1111011010011001", "1111011101111110", "1111100001011100", "1111100100110010", "1111100111111110", "1111101010111111", "1111101101110100", "1111110000011011", "1111110010110010");
    -- table size 32, ib_log_p=6 fb_log_p=2
    -- constant max1_table: max1_table_type := ("11111101", "11111110", "11111110", "11111110", "11111111", "11111111", "11111111", "11111111", "11111111", "00000000", "00000000", "00000000", "00000000", "00000000", "00000000", "00000000", "11110000", "11110001", "11110010", "11110011", "11110100", "11110101", "11110110", "11110111", "11110111", "11111000", "11111001", "11111010", "11111011", "11111011", "11111100", "11111101");
    -- table size 32, ib_log_p=6 fb_log_p=4
    -- constant max1_table: max1_table_type := ("1111110101", "1111110111", "1111111000", "1111111010", "1111111011", "1111111100", "1111111101", "1111111101", "1111111110", "1111111110", "1111111111", "1111111111", "1111111111", "1111111111", "0000000000", "0000000000", "1111000000", "1111000100", "1111001000", "1111001011", "1111001111", "1111010011", "1111010111", "1111011010", "1111011110", "1111100001", "1111100101", "1111101000", "1111101011", "1111101110", "1111110000", "1111110011");

    procedure READ(L:inout LINE; R:out signed);
    procedure WRITE(L:inout LINE; R:in signed);
    
    subtype log_map_address_type is unsigned(length(tau-1)-1 downto 0);
    type y_data_type is array (0 to n_samples_group-1) of unsigned(bl_y-1 downto 0);
    type llr_data_type is array (0 to n_input_bits-1) of unsigned(bl_log_p-1 downto 0);
    subtype beta_address_type is unsigned(length(tau)-1 downto 0);
    type beta_data_type is array (0 to n_states-1) of unsigned(bl_log_p-1 downto 0);
    
    function max1_aux(R: log_p_type) return log_p_type;
    function max1(L, R: log_p_type) return log_p_type;
    function change_fb(x: signed; fb_in, fb_out, bl_out: integer) return signed;
    
end utils;

package body utils is

	function to_string(x:std_logic_vector) return string is
		variable str: string(x'range) :="";
		variable element : string(1 to 3);
	begin
		for i in x'range loop
			element := std_logic'image(x(i));
			str(i) := element(2);
		end loop;
		return str;
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
       variable sum: signed(max(L'length, R'length)-1 downto 0);
       variable sum_s: signed(max(L'length, R'length)-1 downto 0);
    begin
       sum := L + R;
       
       if L(L'left) = '0' and R(R'left) = '0' and sum(sum'left) = '1' then
          sum_s(sum'left) := '0';
          sum_s(sum'left-1 downto sum'right) := (others=>'1');
       elsif L(L'left) = '1' and R(R'left) = '1' and sum(sum'left) = '0' then
          sum_s(sum'left) := '1';
          sum_s(sum'left-1 downto sum'right) := (others=>'0');
       else
          sum_s := sum;
       end if;
       
       return sum_s;
    end saturating_add;
    
	function saturating_sub(L, R: signed) return signed is
       variable sum: signed(max(L'length, R'length)-1 downto 0);
       variable sum_s: signed(max(L'length, R'length)-1 downto 0);
    begin
       sum := L - R;
       
       if L(L'left) = '0' and R(R'left) = '1' and sum(sum'left) = '1' then
          sum_s(sum'left) := '0';
          sum_s(sum'left-1 downto sum'right) := (others=>'1');
       elsif L(L'left) = '1' and R(R'left) = '0' and sum(sum'left) = '0' then
          sum_s(sum'left) := '1';
          sum_s(sum'left-1 downto sum'right) := (others=>'0');
       else
          sum_s := sum;
       end if;
       
       return sum_s;
    end saturating_sub;

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
    
    -- -max1(0,-x)
    -- This function is easier to implement than -max(0,x) since it does not complement the input 
    function max1_aux(R: log_p_type) return log_p_type is
        variable max1: log_p_type;
        variable abs_dif: unsigned(bl_log_p-1 downto 0);
    begin
        if  R >= 0 then
            abs_dif := unsigned(R);
            if abs_dif >= 2**(fb_log_p+2) then          -- 0000 100.0 0000 is not in the table
                return (others=>'0');
            else
                return - max1_table(
                    to_integer(unsigned(abs_dif(fb_log_p+1 downto fb_log_p+2-length(table_size-1))))
                    );
            end if;
        else
            abs_dif := unsigned(-R);
            if abs_dif >= 2**(fb_log_p+2) then          -- 0000 100.0 0000 is not in the table
                return R;
            else
                return R - max1_table(
                    to_integer(unsigned(abs_dif(fb_log_p+1 downto fb_log_p+2-length(table_size-1))))
                    );
            end if;
        end if;
    end max1_aux;
    
    -- max1(a,b)
    function max1(L, R: log_p_type) return log_p_type is
        variable max1: log_p_type;
        variable abs_dif: unsigned(bl_log_p-1 downto 0);
    begin
        if L > R then
            abs_dif := unsigned(L - R);
            if abs_dif >= 2**(fb_log_p+2) then          -- 0000 100.0 0000 is not in the table
                return L;
            else
                return L + max1_table(
                    to_integer(unsigned(abs_dif(fb_log_p+1 downto fb_log_p+2-length(table_size-1))))
                    );
            end if;
        else
            abs_dif := unsigned(R - L);
            if abs_dif >= 2**(fb_log_p+2) then          -- 0000 100.0 0000 is not in the table
                return R;
            else
                return R + max1_table(
                    to_integer(unsigned(abs_dif(fb_log_p+1 downto fb_log_p+2-length(table_size-1))))
                    );
            end if;
        end if;
    end max1;
    
    function change_fb(x: signed; fb_in, fb_out, bl_out: integer) return signed is 
    -- x is assumed to be in the form n left downto 0
        variable left: integer := bl_out - 1 + fb_in - fb_out;
        variable rigth: integer := fb_in - fb_out;
        variable aux0: signed(max(-rigth-1, 0) downto 0) := (others => '0');
        variable aux1: signed(max(left - x'left - 1, 0) downto 0) :=  (others => x(x'left));
    begin
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

end utils;
