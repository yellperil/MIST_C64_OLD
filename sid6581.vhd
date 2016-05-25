-------------------------------------------------------------------------------------
--
--                                 SID 6581
--
--     A fully functional SID chip implementation in VHDL
--
-------------------------------------------------------------------------------------
--	to do:		- filter
--					- smaller implementation, use multiplexed channels
--
-- Synthesize		WARNING:Xst:1988 - Unit <vic_II>: instances <Mcompar__n0411>, <Mcompar__n0337> of unit <LPM_COMPARE_15> and unit <LPM_COMPARE_13> are dual, second instance is removed
--
--"The Filter was a classic multi-mode (state variable) VCF design. There was no way to create a variable transconductance amplifier in our NMOS process, so I simply used FETs as voltage-controlled resistors to control the cutoff frequency. An 11-bit D/A converter generates the control voltage for the FETs (it's actually a 12-bit D/A, but the LSB had no audible affect so I disconnected it!)."
-- "Filter resonance was controlled by a 4-bit weighted resistor ladder. Each bit would turn on one of the weighted resistors and allow a portion of the output to feed back to the input. The state-variable design provided simultaneous low-pass, band-pass and high-pass outputs. Analog switches selected which combination of outputs were sent to the final amplifier (a notch filter was created by enabling both the high and low-pass outputs simultaneously)."
-- "The filter is the worst part of SID because I could not create high-gain op-amps in NMOS, which were essential to a resonant filter. In addition, the resistance of the FETs varied considerably with processing, so different lots of SID chips had different cutoff frequency characteristics. I knew it wouldn't work very well, but it was better than nothing and I didn't have time to make it better."
--
--	- Devide 32MHz <- Dit kun je makkelijker doen door bovenste bit van busCycle te pakken (deze deelt de klok al door 32)
--
--
--
-------------------------------------------------------------------------------------
-- Dar 08/03/2014
--
-- roughly modify voice_volume computation to avoid saturation
-------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
--use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;

-------------------------------------------------------------------------------------

entity sid6581 is
	port (
		clk32			: in std_logic;								--	main clock signal
		reset			: in std_logic;								-- high active signal (reset when reset = '1')
		cs				: in std_logic;								--	"chip select", when this signal is '1' this model can be accessed
		we				: in std_logic;								-- when '1' this model can be written to, otherwise access is considered as read

		addr			: in unsigned(4 downto 0);					-- address lines
		di				: in std_logic_vector(7 downto 0);		--	data in (to chip)
		do				: out std_logic_vector(7 downto 0);		--	data out	(from chip)

		audio_data	: out std_logic_vector(17 downto 0);

		extfilter_en : in std_logic
	);
end sid6581;


architecture Behavioral of sid6581 is

signal clk_1MHz			: std_logic;
signal devide_0			: std_logic_vector(31 downto 0);
signal voice_volume		: signed(17 downto 0);

-------------------------------------------------------------------------------------

begin

clk_1MHz				<= devide_0(31);
audio_data        <= std_logic_vector(voice_volume);

-- Devide 32MHz clock back to 1MHz for internal use within the SID
devider0: process(clk32)				-- this process devides 32 MHz to 1MHz (for the SID)
begin									
	if (rising_edge(clk32)) then			    			
		if (reset = '1') then				
			devide_0 	<= "00000000000000000000000000000001";
		else
			devide_0(31 downto 1)	<= devide_0(30 downto 0); --devide(31) now has a freq. that is 1/32 of clk32, while having a duty cycle of 50%
			devide_0(0)					<= devide_0(31);
		end if;
	end if;
end process;

i_sid_engine: entity work.sid_top
port map (
	clock         => clk32,
	reset         => reset,
                      
	addr          => "000" & addr,
	wren          => cs and we,
	wdata         => di,
	rdata         => do,

	comb_wave_l   => '0',
	comb_wave_r   => '0',

	extfilter_en  => extfilter_en,

	start_iter    => clk_1MHz,
	sample_left   => voice_volume,
	sample_right  => open
);

end Behavioral;
