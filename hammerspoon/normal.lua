---------------------
--  Non-Modal Init --
---------------------

-- Global Variables
gutter = 20
editor = "Atom"

-- Welcome Messages
hs.hotkey.bind({"cmd", "ctrl"}, "W", function()
    hs.alert.show("hello world", { textSize = 24, radius = 10, textStyle = { paragraphStyle = { alignment = "center" }}})
end)

hs.hotkey.bind({"cmd", "ctrl", "shift"}, "W", function()
    hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send():release()
end)

-- URL test
hs.urlevent.bind("someAlert", function(eventName, params)
    hs.alert.show("Received someAlert")
end)

---------------------
--  Window Sizing  --
---------------------

-- Maximize
hs.hotkey.bind({"cmd", "ctrl"}, "M", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + gutter
    f.y = max.y + gutter
    f.w = max.w - (gutter * 2)
    f.h = max.h - (gutter * 2)
    win:setFrame(f)
end)

-- Maximize All
hs.hotkey.bind({"cmd", "ctrl", "alt"}, "M", function()
    local wins = hs.window.visibleWindows()

    for key,win in pairs(wins) do
        local f = win:frame()
        local screen = win:screen()
        local max = screen:frame()

        f.x = max.x + gutter
        f.y = max.y + gutter
        f.w = max.w - (gutter * 2)
        f.h = max.h - (gutter * 2)
        win:setFrame(f)
    end
end)

-- Left Half
hs.hotkey.bind({"cmd", "ctrl"}, "H", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + gutter
    f.y = max.y + gutter
    f.w = (max.w / 2) - (gutter * 1.5)
    f.h = max.h - (gutter * 2)
    win:setFrame(f)
end)

-- Right Half
hs.hotkey.bind({"cmd", "ctrl"}, "L", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + (max.w / 2) + (gutter / 2)
    f.y = max.y + gutter
    f.w = (max.w / 2) - (gutter * 1.5)
    f.h = max.h - (gutter * 2)
    win:setFrame(f)
end)

-- Top Half
hs.hotkey.bind({"cmd", "ctrl"}, "K", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + gutter
    f.y = max.y + gutter
    f.w = max.w - (gutter * 2)
    f.h = (max.h / 2) - (gutter * 1.5)
    win:setFrame(f)
end)

-- Bottom Half
hs.hotkey.bind({"cmd", "ctrl"}, "J", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + gutter
    f.y = (max.h / 2) + (gutter * 2)
    f.w = max.w - (gutter * 2)
    f.h = (max.h / 2) - (gutter * 1.5)
    win:setFrame(f)
end)

-- Top Left Quarter
hs.hotkey.bind({"cmd", "ctrl"}, "Y", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + gutter
    f.y = max.y + gutter
    f.w = (max.w / 2) - (gutter * 1.5)
    f.h = (max.h / 2) - (gutter * 1.5)
    win:setFrame(f)
end)

-- Top Right Quarter
hs.hotkey.bind({"cmd", "ctrl"}, "U", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = (max.w / 2) + (gutter / 2)
    f.y = max.y + gutter
    f.w = (max.w / 2) - (gutter * 1.5)
    f.h = (max.h / 2) - (gutter * 1.5)
    win:setFrame(f)
end)

-- Bottom Left Quarter
hs.hotkey.bind({"cmd", "ctrl"}, "B", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + gutter
    f.y = (max.h / 2) + (gutter / 2)
    f.w = (max.w / 2) - (gutter * 1.5)
    f.h = (max.h / 2) - (gutter * 1.5)
    win:setFrame(f)
end)

-- Bottom Right Quarter
hs.hotkey.bind({"cmd", "ctrl"}, "N", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = (max.w / 2) + (gutter / 2)
    f.y = (max.h / 2) + (gutter / 2)
    f.w = (max.w / 2) - (gutter * 1.5)
    f.h = (max.h / 2) - (gutter * 1.5)
    win:setFrame(f)
end)

-- Right Third
hs.hotkey.bind({"cmd", "ctrl"}, ".", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + (max.w * 0.6) + (gutter / 2)
    f.y = max.y + gutter
    f.w = (max.w * 0.4) - (gutter * 1.5)
    f.h = max.h - (gutter * 2)
    win:setFrame(f)
end)

-- Right Two/Thirds
hs.hotkey.bind({"cmd", "ctrl", "shift"}, ".", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + (max.w * 0.4) + (gutter / 2)
    f.y = max.y + gutter
    f.w = (max.w * 0.6) - (gutter * 1.5)
    f.h = max.h - (gutter * 2)
    win:setFrame(f)
end)

-- Left Third
hs.hotkey.bind({"cmd", "ctrl"}, ",", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + gutter
    f.y = max.y + gutter
    f.w = (max.w * 0.4) - (gutter * 1.5)
    f.h = max.h - (gutter * 2)
    win:setFrame(f)
end)

-- Left Two/Thirds
hs.hotkey.bind({"cmd", "ctrl", "shift"}, ",", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + gutter
    f.y = max.y + gutter
    f.w = (max.w * 0.6) - (gutter * 1.5)
    f.h = max.h - (gutter * 2)
    win:setFrame(f)
end)

-- Middle
hs.hotkey.bind({"cmd", "ctrl"}, "/", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.w * 0.2
    f.y = max.h * 0.2
    f.w = max.w * 0.6
    f.h = max.h * 0.6
    win:setFrame(f)
end)

-----------------------
--  Window Movement  --
-----------------------

-- Move Up and Left
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "Y", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x - gutter
  f.y = f.y - gutter
  win:setFrame(f)
end)

-- Move Up
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "K", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.y = f.y - gutter
  win:setFrame(f)
end)

-- Move Up and Right
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "U", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x + gutter
  f.y = f.y - gutter
  win:setFrame(f)
end)

-- Move Left
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x - gutter
  win:setFrame(f)
end)

-- Move Right
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "L", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x + gutter
  win:setFrame(f)
end)

-- Move Down and Left
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "B", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x - gutter
  f.y = f.y + gutter
  win:setFrame(f)
end)

-- Move Down
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "J", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.y = f.y + gutter
  win:setFrame(f)
end)

-- Move Down and Right
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "N", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x + gutter
  f.y = f.y + gutter
  win:setFrame(f)
end)

-------------------------
-- Display Management  --
-------------------------

-- Move Window to West Display
hs.hotkey.bind({"cmd", "ctrl", "alt"}, "H", function()
  local win = hs.window.focusedWindow()

  win:moveOneScreenWest()
end)

-- Move Window to East Display
hs.hotkey.bind({"cmd", "ctrl", "alt"}, "L", function()
  local win = hs.window.focusedWindow()

  win:moveOneScreenEast()
end)

-- Move Window to North Display
hs.hotkey.bind({"cmd", "ctrl", "alt"}, "K", function()
  local win = hs.window.focusedWindow()

  win:moveOneScreenNorth()
end)

-- Move Window to South Display
hs.hotkey.bind({"cmd", "ctrl", "alt"}, "J", function()
  local win = hs.window.focusedWindow()

  win:moveOneScreenSouth()
end)


---------------------
-- Audio Switcher  --
---------------------
function cycleOutputDevice()
  -- load all output devices into a table
  local allDevices = hs.audiodevice.allOutputDevices()
  local current = 0 -- key of currently selected output
  local devices = 0 -- number of all devices in table

  for key,device in ipairs(allDevices) do
    devices = devices + 1 -- update device count
    if device:name() == hs.audiodevice.current().name then
      current = key -- set the current output device
    end
  end

  if current < devices then
    current = current + 1 -- increment key to cycle to next device
  else
    current = 1 -- or wrap around to beginning of table
  end

  -- set the new output device and alert user, or show error
  if allDevices[current]:setDefaultOutputDevice() then
    hs.alert.show("Audio Output: " .. allDevices[current]:name())
  else
    hs.alert.show("Error Selecting Audio Output")
  end
end

hs.hotkey.bind({"cmd", "ctrl"}, "S", cycleOutputDevice)

---------------------
-- Pomodoro Timer  --
---------------------

-- Cancel Scheduled Notification
function cancelTimer(silent)
    if timer and timer:running() then
        timer:stop()
        if not silent then
            hs.alert(" Pomodoro Canceled ")
        end
    else
        if not silent then
            hs.alert(" No Pomodoro Running ")
        end
    end
end

-- Schedule Notification
function startTimer(work)
    cancelTimer(true)

    if work then
        timer = hs.timer.doAfter(25 * 60, function()
            hs.alert(" Pomodoro Complete ", 4)
            hs.notify.new({
                title="Pomodoro Complete",
                informativeText="Did you complete your One Thing?"
            }):send()
        end)
        hs.alert(" Pomodoro Timer Started ", 2)
    else
        timer = hs.timer.doAfter(5 * 60, function()
            hs.alert(" Break Over ", 4)
            hs.notify.new({
                title="Pomodoro Break Over",
                informativeText="Did you stretch? Look away? Enjoy your rest?"
            }):send()
        end)
        hs.alert(" Break Started ", 2)
    end
end

-- Simple Callbacks
function startWork() startTimer(true) end
function startBreak() startTimer(false) end

hs.hotkey.bind({"cmd", "ctrl"}, "P", startWork)
hs.hotkey.bind({"cmd", "ctrl", "shift"}, "P", startBreak)
hs.hotkey.bind({"cmd", "ctrl", "alt"}, "P", cancelTimer)


--------------------------------
--  Here be some nonsense...  --
--------------------------------

-- Find The Cursor
local mouseCircle = nil
local mouseCircleTimer = nil

function mouseHighlight()
    -- Delete an existing highlight if it exists
    if mouseCircle then
        mouseCircle:delete()
        if mouseCircleTimer then
            mouseCircleTimer:stop()
        end
    end
    -- Get the current co-ordinates of the mouse pointer
    mousepoint = hs.mouse.get()
    -- Prepare a big red circle around the mouse pointer
    mouseCircle = hs.drawing.circle(hs.geometry.rect(mousepoint.x-50, mousepoint.y-50, 100, 100))
    mouseCircle:setStrokeColor({["red"]=0.85,["blue"]=0.2,["green"]=0.1,["alpha"]=0.9})
    mouseCircle:setFill(false)
    mouseCircle:setStrokeWidth(8)
    mouseCircle:show()

    -- Set a timer to delete the circle after 3 seconds
    mouseCircleTimer = hs.timer.doAfter(3, function() mouseCircle:delete() end)
end
hs.hotkey.bind({"cmd","alt","shift"}, "D", mouseHighlight)
