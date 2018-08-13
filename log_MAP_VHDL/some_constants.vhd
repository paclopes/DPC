-- Generated from Matlab in 13-Aug-2018
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
package some_constants is
    constant bl_log_p: integer :=  8;
    constant bl_qinv: integer := 4;
    constant bl_y_sq: integer := 4;
    constant bl_y: integer := 4;
    constant table_size: integer := 16;
    subtype log_p_type is signed(bl_log_p-1 downto 0);
    type max1_table_type is array (0 to table_size-1) of log_p_type;
    constant max1_table: max1_table_type := ("00000011", "00000010", "00000010", "00000010", "00000001", "00000001", "00000001", "00000001", "00000001", "00000000", "00000000", "00000000", "00000000", "00000000", "00000000", "00000000");
    constant n_bits: integer := 600;
end some_constants;
