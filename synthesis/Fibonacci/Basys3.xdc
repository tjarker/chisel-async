set_property IOSTANDARD LVCMOS33 [get_ports *]

set_property CONFIG_VOLTAGE 3.3 [current_design]
set_property CFGBVS VCCO [current_design]

## Clock signal
set_property PACKAGE_PIN W5 [get_ports {clock}]
create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports {clock}]
 
## Switches
#set_property PACKAGE_PIN V17 [get_ports {io_switches[0]}]
#set_property PACKAGE_PIN V16 [get_ports {io_switches[1]}]
#set_property PACKAGE_PIN W16 [get_ports {io_switches[2]}]
#set_property PACKAGE_PIN W17 [get_ports {io_switches[3]}]
#set_property PACKAGE_PIN W15 [get_ports {io_switches[4]}]
#set_property PACKAGE_PIN V15 [get_ports {io_switches[5]}]
#set_property PACKAGE_PIN W14 [get_ports {io_switches[6]}]
#set_property PACKAGE_PIN W13 [get_ports {io_switches[7]}]
#set_property PACKAGE_PIN V2  [get_ports {io_switches[8]}]
#set_property PACKAGE_PIN T3  [get_ports {io_switches[9]}]
#set_property PACKAGE_PIN T2  [get_ports {io_switches[10]}]
#set_property PACKAGE_PIN R3  [get_ports {io_switches[11]}]
#set_property PACKAGE_PIN W2  [get_ports {io_switches[12]}]
#set_property PACKAGE_PIN U1  [get_ports {io_switches[13]}]
#set_property PACKAGE_PIN T1  [get_ports {io_switches[14]}]
#set_property PACKAGE_PIN R2  [get_ports {io_switches[15]}]
 

## LEDs
set_property PACKAGE_PIN U16 [get_ports {io_out_payload[0]}]
set_property PACKAGE_PIN E19 [get_ports {io_out_payload[1]}]
set_property PACKAGE_PIN U19 [get_ports {io_out_payload[2]}]
set_property PACKAGE_PIN V19 [get_ports {io_out_payload[3]}]
set_property PACKAGE_PIN W18 [get_ports {io_out_payload[4]}]
set_property PACKAGE_PIN U15 [get_ports {io_out_payload[5]}]
set_property PACKAGE_PIN U14 [get_ports {io_out_payload[6]}]
set_property PACKAGE_PIN V14 [get_ports {io_out_payload[7]}]
#set_property PACKAGE_PIN V13 [get_ports {io_leds[8]}]
#set_property PACKAGE_PIN V3  [get_ports {io_leds[9]}]
#set_property PACKAGE_PIN W3  [get_ports {io_leds[10]}]
#set_property PACKAGE_PIN U3  [get_ports {io_leds[11]}]
#set_property PACKAGE_PIN P3  [get_ports {io_leds[12]}]
#set_property PACKAGE_PIN N3  [get_ports {io_leds[13]}]
set_property PACKAGE_PIN P1  [get_ports {io_ack}]
set_property PACKAGE_PIN L1  [get_ports {io_out_req}]
	
	
##Buttons
#set_property PACKAGE_PIN U18 [get_ports btnC]						
#set_property PACKAGE_PIN T18 [get_ports btnU]						
#set_property PACKAGE_PIN W19 [get_ports btnL]						
#set_property PACKAGE_PIN T17 [get_ports reset]
#set_property PACKAGE_PIN U17 [get_ports btnD]						


##7 segment display
#set_property PACKAGE_PIN W7 [get_ports {seg[0]}]					
#set_property PACKAGE_PIN W6 [get_ports {seg[1]}]					
#set_property PACKAGE_PIN U8 [get_ports {seg[2]}]					
#set_property PACKAGE_PIN V8 [get_ports {seg[3]}]					
#set_property PACKAGE_PIN U5 [get_ports {seg[4]}]					
#set_property PACKAGE_PIN V5 [get_ports {seg[5]}]					
#set_property PACKAGE_PIN U7 [get_ports {seg[6]}]					
#set_property PACKAGE_PIN V7 [get_ports {dp}]							
#set_property PACKAGE_PIN U2 [get_ports {an[0]}]					
#set_property PACKAGE_PIN U4 [get_ports {an[1]}]					
#set_property PACKAGE_PIN V4 [get_ports {an[2]}]					
#set_property PACKAGE_PIN W4 [get_ports {an[3]}]					


##VGA Connector
#set_property PACKAGE_PIN G19 [get_ports {vgaRed[0]}]				
#set_property PACKAGE_PIN H19 [get_ports {vgaRed[1]}]				
#set_property PACKAGE_PIN J19 [get_ports {vgaRed[2]}]				
#set_property PACKAGE_PIN N19 [get_ports {vgaRed[3]}]				
#set_property PACKAGE_PIN N18 [get_ports {vgaBlue[0]}]				
#set_property PACKAGE_PIN L18 [get_ports {vgaBlue[1]}]				
#set_property PACKAGE_PIN K18 [get_ports {vgaBlue[2]}]				
#set_property PACKAGE_PIN J18 [get_ports {vgaBlue[3]}]				
#set_property PACKAGE_PIN J17 [get_ports {vgaGreen[0]}]				
#set_property PACKAGE_PIN H17 [get_ports {vgaGreen[1]}]				
#set_property PACKAGE_PIN G17 [get_ports {vgaGreen[2]}]				
#set_property PACKAGE_PIN D17 [get_ports {vgaGreen[3]}]				
#set_property PACKAGE_PIN P19 [get_ports {Hsync}]						
#set_property PACKAGE_PIN R19 [get_ports {Vsync}]						

 
##USB-RS232 Interface
#set_property PACKAGE_PIN B18 [get_ports {io_uart_rx}]
#set_property PACKAGE_PIN A18 [get_ports {io_uart_tx}]


##USB HID (PS/2)
#set_property PACKAGE_PIN C17 [get_ports PS2Clk]						
#set_property PULLUP true [get_ports PS2Clk]
#set_property PACKAGE_PIN B17 [get_ports PS2Data]					
#set_property PULLUP true [get_ports PS2Data]


##Pmod Header JA
#set_property PACKAGE_PIN J1 [get_ports {JA[0]}]					
#set_property PACKAGE_PIN L2 [get_ports {JA[1]}]					
#set_property PACKAGE_PIN J2 [get_ports {JA[2]}]					
#set_property PACKAGE_PIN G2 [get_ports {JA[3]}]					
#set_property PACKAGE_PIN H1 [get_ports {JA[4]}]					
#set_property PACKAGE_PIN K2 [get_ports {JA[5]}]					
#set_property PACKAGE_PIN H2 [get_ports {JA[6]}]					
#set_property PACKAGE_PIN G3 [get_ports {JA[7]}]					


##Pmod Header JB
set_property PACKAGE_PIN A14 [get_ports {reset}]					
set_property PACKAGE_PIN A16 [get_ports {io_out_ack}]					
set_property PACKAGE_PIN B15 [get_ports {io_start}]					
#set_property PACKAGE_PIN B16 [get_ports {JB[3]}]					
#set_property PACKAGE_PIN A15 [get_ports {JB[4]}]					
#set_property PACKAGE_PIN A17 [get_ports {JB[5]}]					
#set_property PACKAGE_PIN C15 [get_ports {JB[6]}]					
#set_property PACKAGE_PIN C16 [get_ports {JB[7]}]					
 

##Pmod Header JC
#set_property PACKAGE_PIN K17 [get_ports {JC[0]}]					
#set_property PACKAGE_PIN M18 [get_ports {JC[1]}]					
#set_property PACKAGE_PIN N17 [get_ports {JC[2]}]					
#set_property PACKAGE_PIN P18 [get_ports {JC[3]}]					
#set_property PACKAGE_PIN L17 [get_ports {JC[4]}]					
#set_property PACKAGE_PIN M19 [get_ports {JC[5]}]					
#set_property PACKAGE_PIN P17 [get_ports {JC[6]}]					
#set_property PACKAGE_PIN R18 [get_ports {JC[7]}]