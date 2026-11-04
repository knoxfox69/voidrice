-- Keybinds (migrated from keybinds.conf)

-- $TERMINAL / $BROWSER came from the environment in the old config
local terminal = os.getenv("TERMINAL") or "foot"
local browser  = os.getenv("BROWSER") or "firefox"

local exec = hl.dsp.exec_cmd

-- Programs
hl.bind("SUPER + Return", exec(terminal))
hl.bind("SUPER + w", exec(browser .. " --enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=WebRTCPipeWireCapturer"))
hl.bind("SUPER + r", exec(terminal .. " -e lf"))
hl.bind("SUPER + SHIFT + r", exec(terminal .. " -e htop"))
hl.bind("SUPER + SHIFT + m", exec("LD_PRELOAD=/usr/lib/spotify-adblock.so spotify"))
hl.bind("SUPER + m", exec("tidal-hifi"))
-- Old: bind = SHIFTALT,, exec, ~/.config/eww/scripts/language --cycle
-- (modifier-only bind with no key, and the script does not exist; disabled)
-- hl.bind("Shift_L + Alt_L", exec("~/.config/eww/scripts/language --cycle"))

-- Actions
hl.bind("SUPER + SHIFT + period", exec("pkill wofi || wofi-emoji"))
hl.bind("SUPER + grave", exec("pkill fuzzel || ~/.local/bin/fuzzel-emoji"))

-- Pop terminal
hl.bind("SUPER + SHIFT + Return", exec("pypr toggle term"))
-- Pop calculator
hl.bind("SUPER + code:48", exec("pypr toggle qalc"))

-- Window manip
hl.bind("SUPER + q", hl.dsp.window.close())
hl.bind("SUPER + SHIFT + space", hl.dsp.window.float({ action = "toggle" }))

-- Move workspace and focus monitor (on key release, as before)
hl.bind("SUPER + CTRL + left",  hl.dsp.focus({ workspace = "-1" }), { release = true })
hl.bind("SUPER + CTRL + right", hl.dsp.focus({ workspace = "+1" }), { release = true })
hl.bind("SUPER + left",  hl.dsp.focus({ monitor = "l" }), { release = true })
hl.bind("SUPER + right", hl.dsp.focus({ monitor = "r" }), { release = true })
hl.bind("SUPER + Tab",   hl.dsp.focus({ workspace = "previous" }), { release = true })

-- Resize
hl.bind("SUPER + h", hl.dsp.window.resize({ x = -50, y = 0, relative = true }), { release = true })
hl.bind("SUPER + l", hl.dsp.window.resize({ x = 50,  y = 0, relative = true }), { release = true })

hl.bind("SUPER + j", hl.dsp.window.cycle_next(), { release = true })
hl.bind("SUPER + k", hl.dsp.window.cycle_next({ next = false }), { release = true })
hl.bind("SUPER + SHIFT + j", hl.dsp.window.swap({ next = true }), { release = true })
hl.bind("SUPER + SHIFT + k", hl.dsp.window.swap({ prev = true }), { release = true })
hl.bind("SUPER + space", hl.dsp.window.swap({ next = true })) -- old: swapnext,master ("master" arg was ignored)
hl.bind("SUPER + CTRL + j", hl.dsp.focus({ workspace = "-1" }), { release = true })
hl.bind("SUPER + CTRL + k", hl.dsp.focus({ workspace = "+1" }), { release = true })
-- resize by a percentage of the active window's width (the Lua resize dispatcher only takes pixels)
local function resize_percent(pct)
    return function()
        local win = hl.get_active_window()
        if not win then return end
        local width = win.size.x or win.size.w or win.size[1]
        if not width then return end
        hl.dispatch(hl.dsp.window.resize({ x = math.floor(width * pct / 100), y = 0, relative = true }))
    end
end
hl.bind("SUPER + ALT + h", resize_percent(-10), { repeating = true })
hl.bind("SUPER + ALT + l", resize_percent(10),  { repeating = true })

-- Move/resize windows with SUPER + LMB/RMB
hl.bind("SUPER + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind("SUPER + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- System actions
hl.bind("SUPER + d", exec("rofi -show drun -theme ~/.config/rofi/launcher.rasi"))
hl.bind("SUPER + SHIFT + q", exec("wlogout -p layer-shell"))
hl.bind("SUPER + SHIFT + w", exec(terminal .. " nmtui -e"))

-- Media keys
local vol = "~/.config/hypr/scripts/volume"
hl.bind("XF86AudioRaiseVolume",  exec(vol .. " --up"),   { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume",  exec(vol .. " --down"), { locked = true, repeating = true })
hl.bind("XF86AudioMute",         exec(vol .. " --mute"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp",   exec("brightnessctl set 5%+"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", exec("brightnessctl set 5%-"), { locked = true, repeating = true })
hl.bind("XF86Calculator", exec("QT_QPA_PLATFORMTHEME=qt5ct kcalc"))
hl.bind("XF86AudioMedia", exec("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay",  exec("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioStop",  exec("playerctl stop"),       { locked = true })
hl.bind("XF86AudioPrev",  exec("playerctl previous"),   { locked = true })
hl.bind("XF86AudioNext",  exec("playerctl next"),       { locked = true })

-- Change workspace / move window (silently) to workspace
for i = 1, 10 do
    local key = i % 10 -- workspace 10 is on key 0
    hl.bind("SUPER + " .. key,           hl.dsp.focus({ workspace = i }))
    hl.bind("SUPER + SHIFT + " .. key,   hl.dsp.window.move({ workspace = i, follow = false }))
end

-- Screenshot
hl.bind("SHIFT + Print", exec("maimpick"))
hl.bind("Print", exec([[grim -o "$(hyprctl monitors | awk '/^Monitor/{name=$2} /focused: yes/{print name}')" pic-full-"$(date '+%y%m%d-%H%M-%S').png"]]))

-- Toggle fullscreen
hl.bind("SUPER + f", hl.dsp.window.fullscreen({ mode = "fullscreen" }))

-- Volume (keycodes 20 / 21 = minus / equal)
hl.bind("SUPER + SHIFT + code:20", exec("bash " .. vol .. " --down"))
hl.bind("SUPER + SHIFT + code:21", exec("bash " .. vol .. " --up"))
