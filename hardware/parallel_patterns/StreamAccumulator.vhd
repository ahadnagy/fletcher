----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Ákos Hadnagy
-- 
-- Create Date: 05/29/2020 03:41:48 PM
-- Design Name: 
-- Module Name: SequenceStream - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

-- This unit is a simple accumulator.

entity StreamAccumulator is
  generic (
    
    -- Width of the stream data vector.
    DATA_WIDTH                  : natural
    
  );
  port (

    -- Rising-edge sensitive clock.
    clk                         : in  std_logic;

    -- Active-high synchronous reset.
    reset                       : in  std_logic;
    
    -- Init value
    -- Loaded at reset and on 'last'.
    init_value                  : in std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Input stream.
    in_valid                    : in  std_logic;
    in_ready                    : out std_logic;
    in_data                     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    in_last                     : in  std_logic;
    in_dvalid                   : in  std_logic := '1';

    -- Output stream.
    out_valid                   : out std_logic;
    out_ready                   : in  std_logic;
    out_data                    : out std_logic_vector(DATA_WIDTH-1 downto 0)
    
  );
end StreamAccumulator;

architecture Behavioral of StreamAccumulator is  
  
  -- Initialization status regsiter.
  signal initialized            : std_logic;
  signal saved_last             : std_logic;

  -- Holding register for the accumulator data
  signal data                   : std_logic_vector(DATA_WIDTH-1 downto 0);

begin

-- Input is always ready.
in_ready <= '1';

-- Output data vector is always the accumulator register.
out_data <= data;

reg_proc: process (clk) is
  begin
    if rising_edge(clk) then
    
      out_valid <= initialized;
      
      if in_valid = '1' and in_dvalid = '1' then
        data <= in_data;
        initialized <= '1';
        saved_last <= '0';
        if in_last = '1' then
          saved_last <= '1';
          initialized <= '0';
          data <= init_value;
        end if;
      end if;
      
      if reset = '1' then
        initialized <= '1';
        saved_last  <= '0';
        data <= init_value;
      end if;
    end if;
  end process;
    
end Behavioral;
