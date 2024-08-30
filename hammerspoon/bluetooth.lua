------------------------
-- Bluetooth Devices  --
------------------------

-- Source: https://github.com/drn/dots/blob/master/hammerspoon/bluetooth.lua

local bluetooth = {}

local alert = require 'alert'

function bluetooth.toggle()
  local output = hs.execute("")
