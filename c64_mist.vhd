---------------------------------------------------------------------------------
-- DE2-35 Top level for FPGA64_027 by Dar (darfpga@aol.fr)
-- http://darfpga.blogspot.fr
--
-- FPGA64 is Copyrighted 2005-2008 by Peter Wendrich (pwsoft@syntiac.com)
-- http://www.syntiac.com/fpga64.html
--
-- Main features
--  15KHz(TV) / 31Khz(VGA) : board switch(0)
--  PAL(50Hz) / NTSC(60Hz) : board switch(1) and F12 key
--  PS2 keyboard input with portA / portB joystick emulation : F11 key
--  wm8731 sound output
--  64Ko of board SRAM used
--  External IEC bus available at gpio_1 (for drive 1541 or IEC/SD ...)
--   activated by switch(5) (activated with no hardware will stuck IEC bus)
--
--  Internal emulated 1541 on raw SD card : D64 images start at 25x6KB boundaries
--  Use hexidecimal disk editor such as HxD (www.mh-nexus.de) to build SD card.
--  Cut D64 file and paste at 0x00000 (first), 0x40000 (second), 0x80000 (third),
--  0xC0000(fourth), 0x100000(fith), 0x140000 (sixth) and so on.
--  BE CAREFUL NOT WRITING ON YOUR OWN HARDDRIVE
--
-- Uses only one pll for 32MHz and 18MHz generation from 50MHz
-- DE1 and DE0 nano Top level also available
--     
---------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

entity c64_mist is port
(
	-- Clocks
   CLOCK_27   : in    std_logic;

   -- LED
   LED        : out   std_logic;

   -- VGA
   VGA_R      : out   std_logic_vector(5 downto 0);
   VGA_G      : out   std_logic_vector(5 downto 0);
   VGA_B      : out   std_logic_vector(5 downto 0);
   VGA_HS     : out   std_logic;
   VGA_VS     : out   std_logic;

   -- SDRAM
   SDRAM_A    : out   std_logic_vector(12 downto 0);
   SDRAM_DQ   : inout std_logic_vector(15 downto 0);
   SDRAM_DQML : out   std_logic;
   SDRAM_DQMH : out   std_logic;
   SDRAM_nWE  : out   std_logic;
   SDRAM_nCAS : out   std_logic;
   SDRAM_nRAS : out   std_logic;
   SDRAM_nCS  : out   std_logic;
   SDRAM_BA   : out   std_logic_vector(1 downto 0);
   SDRAM_CLK  : out   std_logic;
   SDRAM_CKE  : out   std_logic;

   -- AUDIO
   AUDIO_L    : out   std_logic;
   AUDIO_R    : out   std_logic;

   -- SPI interface to io controller
   SPI_SCK    : in    std_logic;
   SPI_DO     : inout std_logic;
   SPI_DI     : in    std_logic;
   SPI_SS2    : in    std_logic;
   SPI_SS3    : in    std_logic;
   CONF_DATA0 : in    std_logic
);
end c64_mist;

architecture struct of c64_mist is

component sdram is port
(
   -- interface to the MT48LC16M16 chip
   sd_addr    : out   std_logic_vector(12 downto 0);
   sd_cs      : out   std_logic;
   sd_ba      : out   std_logic_vector(1 downto 0);
   sd_we      : out   std_logic;
   sd_ras     : out   std_logic;
   sd_cas     : out   std_logic;

   -- system interface
   clk        : in    std_logic;
   init       : in    std_logic;

   -- cpu/chipset interface
   addr       : in    std_logic_vector(15 downto 0);
   refresh    : in    std_logic;
   we         : in    std_logic;
   ce         : in    std_logic
);
end component;

component sram is port
(
	init       : in    std_logic;
	clk        : in    std_logic;
   SDRAM_DQ   : inout std_logic_vector(15 downto 0);
   SDRAM_A    : out   std_logic_vector(12 downto 0);
   SDRAM_DQML : out   std_logic;
   SDRAM_DQMH : out   std_logic;
   SDRAM_BA   : out   std_logic_vector(1 downto 0);
   SDRAM_nCS  : out   std_logic;
   SDRAM_nWE  : out   std_logic;
   SDRAM_nRAS : out   std_logic;
   SDRAM_nCAS : out   std_logic;
   SDRAM_CKE  : out   std_logic;

   wtbt       : in    std_logic_vector(1 downto 0);
   addr       : in    std_logic_vector(24 downto 0);
   dout       : out   std_logic_vector(15 downto 0);
   din        : in    std_logic_vector(15 downto 0);
   we         : in    std_logic;
   rd         : in    std_logic;
   ready      : out   std_logic
);
end component;

---------
-- User IO
---------

-- config string used by the io controller to fill the OSD
constant CONF_STR : string := "C64;PRG;S1,D64;O2,Video standard,PAL,NTSC;O3,Joysticks,normal,swapped;O4,Scanlines,Off,On;O6,Audio filter,On,Off;T5,Reset";

-- convert string to std_logic_vector to be given to user_io
function to_slv(s: string) return std_logic_vector is 
  constant ss: string(1 to s'length) := s; 
  variable rval: std_logic_vector(1 to 8 * s'length); 
  variable p: integer; 
  variable c: integer; 
begin 
  for i in ss'range loop
    p := 8 * i;
    c := character'pos(ss(i));
    rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8)); 
  end loop; 
  return rval; 
end function; 

component user_io generic ( STRLEN : integer := 0 ); port
(
	SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
	SPI_MISO     : out std_logic;
	conf_str     : in  std_logic_vector(8*STRLEN-1 downto 0);

	switches     : out std_logic_vector(1 downto 0);
	buttons      : out std_logic_vector(1 downto 0);
	scandoubler_disable : out std_logic;

	joystick_0   : out std_logic_vector(7 downto 0);
	joystick_1   : out std_logic_vector(7 downto 0);
	joystick_analog_0 : out std_logic_vector(15 downto 0);
	joystick_analog_1 : out std_logic_vector(15 downto 0);
	status       : out std_logic_vector(7 downto 0);

	sd_lba       : in  std_logic_vector(31 downto 0);
	sd_rd        : in  std_logic;
	sd_wr        : in  std_logic;
	sd_ack       : out std_logic;
	sd_conf      : in  std_logic;
	sd_sdhc      : in  std_logic;
	sd_dout      : out std_logic_vector(7 downto 0);
	sd_dout_strobe: out std_logic;
	sd_din       : in  std_logic_vector(7 downto 0);
	sd_din_strobe: out std_logic;
	sd_change    : out std_logic;

	ps2_clk      : in  std_logic;
	ps2_kbd_clk  : out std_logic;
	ps2_kbd_data : out std_logic
);
end component user_io;

---------
-- sd card
---------

component sd_card port
(
	io_lba    : out std_logic_vector(31 downto 0);
	io_rd     : out std_logic;
	io_wr     : out std_logic;
	io_ack    : in  std_logic;
	io_sdhc   : out std_logic;
	io_conf   : out std_logic;
	io_din    : in  std_logic_vector(7 downto 0);
	io_din_strobe: in  std_logic;
	io_dout   : out std_logic_vector(7 downto 0);
	io_dout_strobe : in std_logic;
	allow_sdhc: in  std_logic;

	sd_cs     : in  std_logic;
	sd_sck    : in  std_logic;
	sd_sdi    : in  std_logic;
	sd_sdo    : out std_logic
);
end component sd_card;

---------
-- OSD
---------

component osd generic ( OSD_COLOR : std_logic_vector(2 downto 0)); port
(
	clk_sys   : in std_logic;
	ce_pix    : in std_logic;

	SPI_SCK   : in std_logic;
	SPI_SS3   : in std_logic;
	SPI_DI    : in std_logic;
		
	-- VGA signals coming from core
	VGA_Rx    : in std_logic_vector(5 downto 0);
	VGA_Gx    : in std_logic_vector(5 downto 0);
	VGA_Bx    : in std_logic_vector(5 downto 0);
	OSD_HS    : in std_logic;
	OSD_VS    : in std_logic;

	-- VGA signals going to video connector
	VGA_R     : out std_logic_vector(5 downto 0);
	VGA_G     : out std_logic_vector(5 downto 0);
	VGA_B     : out std_logic_vector(5 downto 0)
);
end component osd;

---------
-- Scan doubler
---------
component scandoubler is port
(
	clk_sys   : in std_logic;
	ce_x2     : in std_logic;
	ce_x1     : in std_logic;
	scanlines : in std_logic_vector(1 downto 0);

	-- c64 input
	r_in      : in std_logic_vector(5 downto 0);
	g_in      : in std_logic_vector(5 downto 0);
	b_in      : in std_logic_vector(5 downto 0);
	hs_in     : in std_logic;
	vs_in     : in std_logic;
		
	-- vga output
	r_out     : out std_logic_vector(5 downto 0);
	g_out     : out std_logic_vector(5 downto 0);
	b_out     : out std_logic_vector(5 downto 0);
	hs_out    : out std_logic;
	vs_out    : out std_logic
);
end component;
	
----------
-- data_io
----------

component data_io port
(
	-- io controller spi interface
	sck       : in  std_logic;
	ss        : in  std_logic;
	sdi       : in  std_logic;

	downloading: out std_logic;
	size      : out std_logic_vector(15 downto 0);
   index     : out std_logic_vector(4 downto 0);

	-- external ram interface
	clk       : in  std_logic;
	wr        : out std_logic;
	a         : out std_logic_vector(15 downto 0);
	d         : out std_logic_vector(7 downto 0)
);
end component data_io;

---------
-- audio
---------

component sigma_delta_dac port
(
	CLK      : in std_logic;
	RESET    : in std_logic;
	DACin    : in std_logic_vector(14 downto 0);
	DACout   : out std_logic
);

end component sigma_delta_dac;

	signal pll_locked_in: std_logic_vector(1 downto 0);
	signal pll_locked: std_logic;
	signal c1541_reset: std_logic;
	signal idle: std_logic;
	signal ces: std_logic_vector(3 downto 0);
	signal iec_cycle: std_logic;
	signal iec_cycleD: std_logic;
	signal buttons: std_logic_vector(1 downto 0);
	
	-- signals to connect "data_io" for direct PRG injection
	signal ioctl_wr: std_logic;
	signal ioctl_addr: std_logic_vector(15 downto 0);
	signal ioctl_data: std_logic_vector(7 downto 0);
	signal ioctl_index: std_logic_vector(4 downto 0);
	signal ioctl_ram_addr: std_logic_vector(15 downto 0);
	signal ioctl_ram_data: std_logic_vector(7 downto 0);
	signal ioctl_load_addr: std_logic_vector(15 downto 0);
	signal ioctl_ram_wr: std_logic;
	signal ioctl_iec_cycle_used: std_logic;
	signal c64_addr: std_logic_vector(15 downto 0);
	signal c64_data_in: std_logic_vector(7 downto 0);
	signal c64_data_out: std_logic_vector(7 downto 0);
	signal sdram_addr: std_logic_vector(15 downto 0);
	signal sdram_data_out: std_logic_vector(7 downto 0);
	
	signal c1541rom_wr   : std_logic;
	signal c64rom_wr     : std_logic;

	signal joyA : std_logic_vector(7 downto 0);
	signal joyB : std_logic_vector(7 downto 0);
	signal joyA_int : std_logic_vector(5 downto 0);
	signal joyB_int : std_logic_vector(5 downto 0);
	signal joyA_c64 : std_logic_vector(5 downto 0);
	signal joyB_c64 : std_logic_vector(5 downto 0);
	signal reset_key : std_logic;
	
	signal c64_r  : std_logic_vector(5 downto 0);
	signal c64_g  : std_logic_vector(5 downto 0);
	signal c64_b  : std_logic_vector(5 downto 0);
	signal c64_ro : std_logic_vector(5 downto 0);
	signal c64_go : std_logic_vector(5 downto 0);
	signal c64_bo : std_logic_vector(5 downto 0);

	signal status : std_logic_vector(7 downto 0);

	signal sd_lba         : std_logic_vector(31 downto 0);
	signal sd_rd          : std_logic;
	signal sd_wr          : std_logic;
	signal sd_ack         : std_logic;
	signal sd_conf        : std_logic;
	signal sd_sdhc        : std_logic;
	signal sd_dout        : std_logic_vector(7 downto 0);
	signal sd_dout_strobe : std_logic;
	signal sd_din         : std_logic_vector(7 downto 0);
	signal sd_din_strobe  : std_logic;
	signal sd_change      : std_logic;
	
	-- these need to be redirected to the SDRAM
	signal sdram_we : std_logic;
	signal sdram_ce : std_logic;

	signal ps2_clk : std_logic;
	signal ps2_dat : std_logic;
	
	signal sd_dat  : std_logic;
	signal sd_dat3 : std_logic;
	signal sd_cmd  : std_logic;
	signal sd_clk  : std_logic;
	
	signal c64_iec_atn_i  : std_logic;
	signal c64_iec_clk_o  : std_logic;
	signal c64_iec_data_o : std_logic;
	signal c64_iec_atn_o  : std_logic;
	signal c64_iec_data_i : std_logic;
	signal c64_iec_clk_i  : std_logic;

	signal c1541_iec_atn_i  : std_logic;
	signal c1541_iec_clk_o  : std_logic;
	signal c1541_iec_data_o : std_logic;
	signal c1541_iec_atn_o  : std_logic;
	signal c1541_iec_data_i : std_logic;
	signal c1541_iec_clk_i  : std_logic;

	signal tv15Khz_mode   : std_logic;
	signal ntsc_init_mode : std_logic;

	alias  c64_addr_int : unsigned is unsigned(c64_addr);
	alias  c64_data_in_int   : unsigned is unsigned(c64_data_in);
	signal c64_data_in16: std_logic_vector(15 downto 0);
	alias  c64_data_out_int   : unsigned is unsigned(c64_data_out);

	signal clk_ram : std_logic;
	signal clk32 : std_logic;
	signal clk16 : std_logic;
	signal ce_8  : std_logic;
	signal osdclk : std_logic;
	signal clkdiv : std_logic_vector(9 downto 0);

	signal ram_ce : std_logic;
	signal ram_we : std_logic;
	signal r : unsigned(7 downto 0);
	signal g : unsigned(7 downto 0);
	signal b : unsigned(7 downto 0);
	signal hsync : std_logic;
	signal vsync : std_logic;
	signal blank : std_logic;

	signal r_sd  : std_logic_vector(5 downto 0);
	signal g_sd  : std_logic_vector(5 downto 0);
	signal b_sd  : std_logic_vector(5 downto 0);
	signal hsync_out : std_logic;
	signal vsync_out : std_logic;
	signal hsync_osd : std_logic;
	signal vsync_osd : std_logic;
	signal hsync_sd : std_logic;
	signal vsync_sd : std_logic;
	
	signal audio_data : std_logic_vector(17 downto 0);
	
	signal reset_counter    : integer;
	signal reset_n          : std_logic;
	
	signal led_disk         : std_logic_vector(7 downto 0);

begin

	-- 1541 activity led
	LED <= not led_disk(6);

	iec_cycle <= '1' when ces = "1011" else '0';
		
	-- User io
	user_io_d : user_io
	generic map (STRLEN => CONF_STR'length)
	port map (
		SPI_CLK => SPI_SCK,
		SPI_SS_IO => CONF_DATA0,
		SPI_MISO => SPI_DO,
		SPI_MOSI => SPI_DI,

		joystick_0 => joyA,
		joystick_1 => joyB,
					 
		conf_str => to_slv(CONF_STR),

		status => status,
		buttons => buttons,
		scandoubler_disable => tv15Khz_mode,

		sd_lba => sd_lba,
		sd_rd => sd_rd,
		sd_wr => sd_wr,
		sd_ack => sd_ack,
		sd_conf => sd_conf,
		sd_sdhc => sd_sdhc,
		sd_dout => sd_dout,
		sd_dout_strobe => sd_dout_strobe,
		sd_din => sd_din,
		sd_din_strobe => sd_din_strobe,
		sd_change => sd_change,

		ps2_clk => clkdiv(9),
		ps2_kbd_clk => ps2_clk,
		ps2_kbd_data => ps2_dat
	);
		  
	-- rearrange joystick contacta for c64
	joyA_int <= "0" & joyA(4) & joyA(0) & joyA(1) & joyA(2) & joyA(3);
	joyB_int <= "0" & joyB(4) & joyB(0) & joyB(1) & joyB(2) & joyB(3);

	-- swap joysticks if requested
	joyA_c64 <= joyB_int when status(3)='1' else joyA_int;
	joyB_c64 <= joyA_int when status(3)='1' else joyB_int;

	data_io_d : data_io
	port map (
		-- SPI interface
		sck => SPI_SCK,
		ss => SPI_SS2,
		sdi => SPI_DI,
			
		-- ram interface
		index => ioctl_index,
		clk => clk32,
		wr => ioctl_wr,
		a => ioctl_addr,
		d => ioctl_data
	);

	-- multiplex ram port between c64 core and data_io (io controller dma)
	sdram_addr <= c64_addr when iec_cycle='0' else ioctl_ram_addr;
	sdram_data_out <= c64_data_out when iec_cycle='0' else ioctl_ram_data;
	-- ram_we and ce are active low
	sdram_ce <= not ram_ce when iec_cycle='0' else ioctl_iec_cycle_used;
	sdram_we <= not ram_we when iec_cycle='0' else ioctl_iec_cycle_used;

   -- address
	process(clk32)
	begin
		if falling_edge(clk32) then
			iec_cycleD <= iec_cycle;

			if(iec_cycle='1' and iec_cycleD='0' and ioctl_ram_wr='1') then
				ioctl_ram_wr <= '0';
				ioctl_iec_cycle_used <= '1';
				ioctl_ram_addr <= std_logic_vector(unsigned(ioctl_load_addr) + unsigned(ioctl_addr) - 2);
				ioctl_ram_data <= ioctl_data;
			else 
				if(iec_cycle='0') then
					ioctl_iec_cycle_used <= '0';
				end if;
			end if;

			if ioctl_wr='1' and (ioctl_index /=X"0") then
				if(ioctl_addr = 0) then
					ioctl_load_addr(7 downto 0) <= ioctl_data;
				elsif(ioctl_addr = 1) then
					ioctl_load_addr(15 downto 8) <= ioctl_data;
				else 
					-- io controller sent a new byte. Store it until it can be
					--	saved in RAM
					ioctl_ram_wr <= '1';
				end if;
			end if;
		end if;
	end process;

	c64rom_wr   <= ioctl_wr when (ioctl_index = "00000") and (ioctl_addr(14) = '0') else '0';
	c1541rom_wr <= ioctl_wr when (ioctl_index = "00000") and (ioctl_addr(14) = '1') else '0';

	sd_card_d: sd_card
	port map
	(
		-- connection to io controller
		io_lba => sd_lba,
		io_rd  => sd_rd,
		io_wr  => sd_wr,
		io_ack => sd_ack,
		io_conf => sd_conf,
		io_sdhc => sd_sdhc,
		io_din => sd_dout,
		io_din_strobe => sd_dout_strobe,
		io_dout => sd_din,
		io_dout_strobe => sd_din_strobe,
 
		allow_sdhc  => '1',   -- esxdos supports SDHC

		-- connection to host
		sd_cs  => sd_dat3,
		sd_sck => sd_clk,
		sd_sdi => sd_cmd,
		sd_sdo => sd_dat
	);

	process(clk32)
	begin
		if rising_edge(clk32) then
			clk16 <= not clk16;
			clkdiv <= std_logic_vector(unsigned(clkdiv)+1);
			if(clkdiv(1 downto 0) = "00") then
				ce_8 <= '1';
			else
				ce_8 <= '0';
			end if;
		end if;
	end process;

	ntsc_init_mode <= status(2);

   -- second  to generate 64mhz clock and phase shifted ram clock	
	clk_c64 : entity work.pll
	port map(
		inclk0 => CLOCK_27,
		c0 => clk_ram,
		c1 => SDRAM_CLK,
		c2 => clk32,
		locked => pll_locked
	);

	process(clk32)
	begin
		if rising_edge(clk32) then
			reset_n <= '0';
			-- Reset by:
			-- Button at device, IO controller reboot, OSD or FPGA startup
			if status(0)='1' then
				reset_counter <= 16000000;
			elsif buttons(1)='1' or status(5)='1' or pll_locked = '0' or reset_key = '1' then
				reset_counter <= 255;
			else
			  if reset_counter = 0 then
					reset_n <= '1';
				else
					reset_counter <= reset_counter - 1;
				end if;
			end if;
		end if;
	end process;

	SDRAM_DQ(15 downto 8) <= (others => 'Z') when sdram_we='0' else (others => '0');
	SDRAM_DQ(7 downto 0) <= (others => 'Z') when sdram_we='0' else sdram_data_out;

	-- read from sdram
	c64_data_in <= SDRAM_DQ(7 downto 0);
	-- clock is always enabled and memory is never masked as we only
	-- use one byte
	SDRAM_CKE <= '1';
	SDRAM_DQML <= '0';
	SDRAM_DQMH <= '0';

	sdr: sdram port map(
		sd_addr => SDRAM_A,
		sd_ba => SDRAM_BA,
		sd_cs => SDRAM_nCS,
		sd_we => SDRAM_nWE,
		sd_ras => SDRAM_nRAS,
		sd_cas => SDRAM_nCAS,

		clk => clk_ram,
		addr => sdram_addr,
		init => not pll_locked,
		we => sdram_we,
		refresh => idle,       -- refresh ram in idle state
		ce => sdram_ce
	);

	-- decode audio
   dac_l : sigma_delta_dac
   port map (
      CLK => clk32,
      DACin => not audio_data(17) & audio_data(16 downto 3),
		DACout => AUDIO_L,
		RESET => '0'
	);

   dac_r : sigma_delta_dac
   port map (
      CLK => clk32,
      DACin => not audio_data(17) & audio_data(16 downto 3),
		DACout => AUDIO_R,
		RESET => '0'
	);

	fpga64 : entity work.fpga64_sid_iec
	port map(
		clk32 => clk32,
		reset_n => reset_n,
		kbd_clk => not ps2_clk,
		kbd_dat => ps2_dat,
		ramAddr => c64_addr_int,
		ramDataOut => c64_data_out_int,
		ramDataIn => c64_data_in_int,
		ramCE => ram_ce,
		ramWe => ram_we,
		ntscInitMode => ntsc_init_mode,
		hsync => hsync,
		vsync => vsync,
		r => r,
		g => g,
		b => b,
		game => '1',
		exrom => '1',
		irq_n => '1',
		nmi_n => '1',
		dma_n => '1',
		ba => open,
		joyA => unsigned(joyA_c64),
		joyB => unsigned(joyB_c64),
		serioclk => open,
		ces => ces,
		SIDclk => open,
		still => open,
		idle => idle,
		audio_data => audio_data,
		extfilter_en => not status(6),
		iec_data_o => c64_iec_data_o,
		iec_atn_o  => c64_iec_atn_o,
		iec_clk_o  => c64_iec_clk_o,
		iec_data_i => not c64_iec_data_i,
		iec_clk_i  => not c64_iec_clk_i,
		iec_atn_i  => not c64_iec_atn_i,
		disk_num => open,
		c64rom_addr => ioctl_addr(13 downto 0),
		c64rom_data => ioctl_data,
		c64rom_wr => c64rom_wr,
		reset_key => reset_key
	);


   c64_iec_atn_i  <= not ((not c64_iec_atn_o)  and (not c1541_iec_atn_o) );
   c64_iec_data_i <= not ((not c64_iec_data_o) and (not c1541_iec_data_o));
	c64_iec_clk_i  <= not ((not c64_iec_clk_o)  and (not c1541_iec_clk_o) );

	c1541_iec_atn_i  <= c64_iec_atn_i;
	c1541_iec_data_i <= c64_iec_data_i;
	c1541_iec_clk_i  <= c64_iec_clk_i;

	-- 1541 reset is delayed by 2 seconds to give mist firmware enough
	-- time to prepare the sd card
	process(clk32, reset_n)
		variable reset_cnt : integer range 0 to 32000000;
	begin
		if reset_n = '0' then
			reset_cnt := 32000000;
		elsif rising_edge(clk32) then
			if reset_cnt /= 0 then
				reset_cnt := reset_cnt - 1;
			end if;
		end if;

		if reset_cnt = 0 then
			c1541_reset <= '0';
		else 
			c1541_reset <= '1';
		end if;
	end process;

	c1541_sd : entity work.c1541_sd
	port map
	(
		clk32 => clk32,
		clk18 => clk32, -- MiST uses virtual SPI SD, so any clock can be used.
		reset => c1541_reset,

		c1541rom_addr => ioctl_addr(13 downto 0),
		c1541rom_data => ioctl_data,
		c1541rom_wr => c1541rom_wr,

		disk_change => sd_change, 
		disk_num => (others => '0'),   -- not seletable by f8 or similar

		iec_atn_i  => c1541_iec_atn_i,
		iec_data_i => c1541_iec_data_i,
		iec_clk_i  => c1541_iec_clk_i,

		iec_atn_o  => c1541_iec_atn_o,
		iec_data_o => c1541_iec_data_o,
		iec_clk_o  => c1541_iec_clk_o,

		sd_dat  => sd_dat,
		sd_dat3 => sd_dat3,
		sd_cmd  => sd_cmd,
		sd_clk  => sd_clk,

		led => led_disk
	);

	comp_sync : entity work.composite_sync
	port map(
		clk32 => clk32,
		hsync => hsync,
		vsync => vsync,
		ntsc  => ntsc_init_mode,
		hsync_out => hsync_out,
		vsync_out => vsync_out,
		blank => blank
	);

	-- route video through osd
   osd_d : osd generic map (OSD_COLOR => "100")
   port map 
	(
      clk_sys => clk32,
		ce_pix  => ce_8,
      SPI_SCK => SPI_SCK,
      SPI_SS3 => SPI_SS3,
      SPI_DI  => SPI_DI,

      VGA_Rx  => c64_r,
      VGA_Gx  => c64_g,
      VGA_Bx  => c64_b,
      OSD_HS  => hsync_out,
      OSD_VS  => vsync_out,

      VGA_R   => c64_ro,
      VGA_G   => c64_go,
      VGA_B   => c64_bo
   );

	c64_r <= (others => '0') when blank = '1' else std_logic_vector(r(7 downto 2));
	c64_g <= (others => '0') when blank = '1' else std_logic_vector(g(7 downto 2));
	c64_b <= (others => '0') when blank = '1' else std_logic_vector(b(7 downto 2));

	sd: scandoubler
	port map(
		clk_sys => clk32,
		ce_x2 => clk16,
		ce_x1 => ce_8,
		scanlines => '0' & status(4),

		r_in => c64_ro,
		g_in => c64_go,
		b_in => c64_bo,
		hs_in => hsync_out,
		vs_in => vsync_out,

		r_out => r_sd,
		g_out => g_sd,
		b_out => b_sd,
		hs_out => hsync_sd,
		vs_out => vsync_sd
	);

	VGA_R <= c64_ro when tv15Khz_mode='1' else r_sd;
	VGA_G <= c64_go when tv15Khz_mode='1' else g_sd;
	VGA_B <= c64_bo when tv15Khz_mode='1' else b_sd;

	-- synchro composite/ synchro horizontale
	VGA_HS <= not (hsync_out xor vsync_out) when tv15Khz_mode = '1' else not hsync_sd;
	-- commutation rapide / synchro verticale
	VGA_VS <= '1' when tv15Khz_mode = '1' else not vsync_sd;

end struct;
