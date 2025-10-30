local wezterm = require 'wezterm'

local config = wezterm.config_builder()

--config.color_scheme = 'AdventureTime'
config.color_scheme = 'Catppuccin Mocha'
config.font_size = 13
config.harfbuzz_features = {"calt=0", "clig=0", "liga=0"}

config.keys = {
  {key="Enter", mods="SHIFT", action=wezterm.action{SendString="\x1b\r"}}
}

return config