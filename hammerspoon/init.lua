------------------------
--  Hammerspoon Init  --
------------------------

-- Global Variables
-- editor = "emacs"

-- Imports
require "normal"
require "modal"


------------------------
-- Config Management  --
------------------------

-- Auto Config Reload
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
        hs.notify.new({title="Hammerspoon", informativeText="Config Reloaded"}):send():release()
    end
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()

-- Manual Config Reload
hs.hotkey.bind({"cmd", "ctrl"}, "R", function()
    hs.reload()
    hs.notify.new({title="Hammerspoon", informativeText="Config Reloaded"}):send():release()
end)
