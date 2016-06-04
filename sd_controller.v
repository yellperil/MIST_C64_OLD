
module sd_controller
(
	input         clk,
	input         reset,

	output [31:0] io_lba,
	output reg    io_rd,
	output reg    io_wr,
	input			  io_ack,
	output		  io_conf,
	output		  io_sdhc,

	input	[7:0]	  io_din,
	input 		  io_din_strobe,
	output [7:0]  io_dout,
	input 		  io_dout_strobe,

	output [12:0] ram_write_addr,
	output  [7:0] ram_di,
	output        ram_we,
	input         change,
	input   [5:0] track,
	output        busy
); 

endmodule
