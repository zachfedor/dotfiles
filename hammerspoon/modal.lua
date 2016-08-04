-----------------
--  Modal Init --
-----------------
-- just to see what this modal editing is

-- show window hints
hs.hotkey.bind({"cmd", "ctrl"}, "Tab", function()
    hs.hints.windowHints()
end)

-- toggle modes
modal = hs.hotkey.modal.new({"ctrl"}, "Space")
function modal:entered() hs.alert.show(" Window Manager Active ", 999999) end
function modal:exited() hs.alert.closeAll() end

modal:bind('', 'Escape', function() modal:exit() end)
modal:bind('', 'Return', function() modal:exit() end)
