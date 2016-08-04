------------------------
--  Hammerspoon Init  --
------------------------

-- Global Variables
-- editor = "Atom"

-- Imports
require "normal"
require "modal"


-- Welcome Messages
hs.hotkey.bind({"cmd", "ctrl"}, "W", function()
    hs.alert.show("hello world")
end)

hs.hotkey.bind({"cmd", "ctrl", "shift"}, "W", function()
    hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send():release()
end)

-- URL test
hs.urlevent.bind("someAlert", function(eventName, params)
    hs.alert.show("Received someAlert")
end)


------------------------
-- Config Management  --
------------------------

-- Config Reload
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config Reloaded")
