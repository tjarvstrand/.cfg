local wezterm = require 'wezterm'
local act = wezterm.action

local config = wezterm.config_builder()

--config.color_scheme = 'AdventureTime'
config.color_scheme = 'Catppuccin Mocha'
config.font_size = 13
config.harfbuzz_features = {"calt=0", "clig=0", "liga=0"}

-- Use C-x as leader key (emacs prefix)
config.leader = { key = 'x', mods = 'CTRL', timeout_milliseconds = 1000 }

config.keys = {
  { key = 'Enter', mods = 'SHIFT', action = act.SendString '\x1b\r' },

  -- Emacs-style pane splitting
  -- C-x 2: split below (like split-window-below)
  { key = '2', mods = 'LEADER', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
  -- C-x 3: split right (like split-window-right)
  { key = '3', mods = 'LEADER', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },

  -- Emacs-style pane closing
  -- C-x 0: close current pane (like delete-window)
  { key = '0', mods = 'LEADER', action = act.CloseCurrentPane { confirm = true } },
  -- C-x 1: zoom/toggle pane (like delete-other-windows)
  { key = '1', mods = 'LEADER', action = act.TogglePaneZoomState },

  -- C-x x: send C-x to terminal (for emacs)
  { key = 'x', mods = 'LEADER', action = act.SendKey { key = 'x', mods = 'CTRL' } },

  -- C-x o: switch to next pane (like other-window)
  { key = 'o', mods = 'LEADER', action = act.ActivatePaneDirection 'Next' },

  -- Emacs-style directional pane navigation (C-x + arrow or C-x + f/b/n/p)
  { key = 'LeftArrow',  mods = 'LEADER', action = act.ActivatePaneDirection 'Left' },
  { key = 'RightArrow', mods = 'LEADER', action = act.ActivatePaneDirection 'Right' },
  { key = 'UpArrow',    mods = 'LEADER', action = act.ActivatePaneDirection 'Up' },
  { key = 'DownArrow',  mods = 'LEADER', action = act.ActivatePaneDirection 'Down' },
  { key = 'b', mods = 'LEADER', action = act.ActivatePaneDirection 'Left' },
  { key = 'f', mods = 'LEADER', action = act.ActivatePaneDirection 'Right' },
  { key = 'p', mods = 'LEADER', action = act.ActivatePaneDirection 'Up' },
  { key = 'n', mods = 'LEADER', action = act.ActivatePaneDirection 'Down' },

  -- C-x r: enter resize mode
  { key = 'r', mods = 'LEADER', action = act.ActivateKeyTable { name = 'resize_pane', one_shot = false } },
}

-- Resize mode: use C-f/C-b/C-n/C-p (emacs movement) or arrow keys, Escape/C-g to exit
config.key_tables = {
  resize_pane = {
    { key = 'LeftArrow',  action = act.AdjustPaneSize { 'Left', 2 } },
    { key = 'RightArrow', action = act.AdjustPaneSize { 'Right', 2 } },
    { key = 'UpArrow',    action = act.AdjustPaneSize { 'Up', 2 } },
    { key = 'DownArrow',  action = act.AdjustPaneSize { 'Down', 2 } },
    { key = 'b', action = act.AdjustPaneSize { 'Left', 2 } },
    { key = 'f', action = act.AdjustPaneSize { 'Right', 2 } },
    { key = 'p', action = act.AdjustPaneSize { 'Up', 2 } },
    { key = 'n', action = act.AdjustPaneSize { 'Down', 2 } },
    { key = 'Escape', action = 'PopKeyTable' },
    { key = 'g', mods = 'CTRL', action = 'PopKeyTable' },  -- C-g to cancel (emacs quit)
  },
}

return config