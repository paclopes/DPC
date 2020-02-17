# DPC

A Matlab and VHDL implementations of the Dirty Paper Coding (DPC) MODEM described in:

Paulo A. C. Lopes and José A. B. Gerald, A FPGA Implementation of the Log-MAP Algorithm for a Dirty Paper Coding MODEM (to be published)

and

U. Erez and S. Ten Brink, “A close-to-capacity dirty paper coding scheme,” IEEE Transactions on information theory, vol. 51, no. 10, pp. 3417–3432, 2005.


Files and folders description

### DPC_Matlab

Contains the files for the Matlab DCP CODEC implementation.

Files:

dpc.m:     Main file

others:    see file

### DPC_VHDL

DP_codec.vhd:      Dirty Paper Codding CODEC and channel simulator.

DP_coder.vhd:      Dirty Paper Codding CODEC Coder

DP_decoder.vhd:    Dirty Paper Codding CODEC Decoder

- Coder files

viterbi.vhd:        Viterbi algorithm.

interleaver.vhd:    Interleaver for word length size 1.

BRAM2.vhd:         BRAM with one-bit word size

BRAM3.vhd:         BRAM with generic size and word length (equal to BRAM.vhd).

BRAM_LV.vhd:       BRAM with generic size and word length (equal to BRAM.vhd).

interleaver_memory.vhd    Interleaver two port memory with word size 1.

- Decoder files

log_map_beta.vhd:  Calculation of the beta of the log-MAP algorithm.

log_map.vhd	:      Remaining calculations of the log-MAP: alpha and LLR values.  

boxplus1.vhd:      CNDS, reads the interleaver memory and feeds the log-MAP.

boxplus1_beta.vhd: CNDS, reads the interleaver memory and feeds log-MAP_beta.

boxplus2.vhd:      Reads from log-MAP and writes to interleaver memory.

boxplus.vhd:       Box Plus component (unregistered).

boxplus_r.vhd:     Box Plus component (registered, for boxplus2).

vnds.vhd:          VNDS nodes that reads and writes from the interleaver

BRAM.vhd:          BRAM with generic size and word length.

BRAM1.vhd:         BRAM with a MUX to select from the read and write address.

interleaver1.vhd   Interleaver for word length size bl_log_p.

interleaver_memory1.vhd   Interleaver two port memory with word size bl_log_p.

- Library files

utils.vhd          CODEC global constants and utility functions