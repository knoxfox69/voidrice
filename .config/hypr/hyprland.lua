-- Hyprland Lua config (migrated from hyprland.conf and friends).
-- Docs: https://wiki.hypr.land/configuring/

-- Split-out modules (all live next to this file)
require("env")
require("monitor")
require("keybinds")
-- require("colors") -- colors.conf was never sourced by the old config either

-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function()
    -- Core components (authentication, lock screen, notification daemon)
    hl.exec_cmd("dbus-update-activation-environment --systemd")
    hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP") -- for XDPH
    hl.exec_cmd("/usr/bin/gnome-keyring-daemon --start --components=secrets")
    hl.exec_cmd("/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1")
    hl.exec_cmd("dunst")
    hl.exec_cmd("hypridle")
    hl.exec_cmd("wlsunset -l 21.8 -L -102.2 -t 5000 -T 6500")
    hl.exec_cmd("/usr/bin/pypr")
    hl.exec_cmd("/usr/bin/easyeffects --gapplication-service")

    -- Status bar
    hl.exec_cmd("waybar")

    -- Fix cursor
    hl.exec_cmd("hyprctl setcursor Bibata-Modern-Classic 24")

    hl.exec_cmd("sleep 1 && dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")

    -- Wallpaper script
    hl.exec_cmd("sleep 3 & bash $HOME/.config/hypr/scripts/wallpaper-daemon")
end)

---------------------
---- SYSTEM CONFIG ----
---------------------

hl.config({
    input = {
        kb_layout    = "us,latam",
        kb_variant   = "",
        kb_model     = "",
        kb_options   = "grp:alt_shift_toggle",
        kb_rules     = "",

        follow_mouse       = 1,
        numlock_by_default = true,

        touchpad = {
            natural_scroll = true,
        },
    },

    gestures = {
        workspace_swipe_invert             = true,
        workspace_swipe_min_speed_to_force = 10,
        workspace_swipe_cancel_ratio       = 0.85,
    },

    general = {
        gaps_in     = 2,
        gaps_out    = 10,
        border_size = 3,
        col = {
            active_border   = "rgba(7c94bfff)", -- was 0xff7c94bf
            inactive_border = "rgba(ffffff00)", -- was 0x00ffffff
        },
        layout = "master",
    },

    misc = {
        disable_hyprland_logo      = true,
        disable_splash_rendering   = true,
        mouse_move_enables_dpms    = true,
        vrr                        = 0,
        animate_manual_resizes     = true,
        mouse_move_focuses_monitor = true,
    },

    -- Window decorations and styling
    decoration = {
        rounding         = 6,
        active_opacity   = 1,
        inactive_opacity = 1,
    },

    animations = {
        enabled = true,
    },
})

-- Three-finger horizontal swipe to change workspace (the workspace_swipe_* options above tune it)
hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })

hl.animation({ leaf = "windows",    enabled = true,  speed = 2,  bezier = "default" })
hl.animation({ leaf = "border",     enabled = true,  speed = 10, bezier = "default" })
hl.animation({ leaf = "fade",       enabled = false, speed = 5,  bezier = "default" })
hl.animation({ leaf = "workspaces", enabled = true,  speed = 4,  bezier = "default" })

------------------------
---- WINDOW RULES ----
------------------------

-- Popping windows
hl.window_rule({ name = "qalc-popterm", match = { class = "qalc-popterm" }, float = true, pin = true })
hl.window_rule({ name = "foot-popterm", match = { class = "foot-popterm" }, float = true, pin = true })

-- Dialogs
for _, title in ipairs({
    "^(Open File)(.*)$",
    "^(Select a File)(.*)$",
    "^(Choose wallpaper)(.*)$",
    "^(Open Folder)(.*)$",
    "^(Save As)(.*)$",
    "^(Library)(.*)$",
}) do
    hl.window_rule({ name = "float-dialog-" .. title, match = { title = title }, float = true })
end

-- Fixes: Workspaces client window
hl.window_rule({ name = "workspacesclient", match = { class = "Workspacesclient" }, pin = false, float = false })

-----------------------
---- LAYER RULES ----
-----------------------

hl.layer_rule({ match = { namespace = "supercontext" },     blur = true, ignore_alpha = 0.84 })
hl.layer_rule({ match = { namespace = "gtk-layer-shell" },  blur = true })
hl.layer_rule({ match = { namespace = "launcher" },         blur = true })
hl.layer_rule({ match = { namespace = "notifications" },    blur = true, ignore_alpha = 0.69 })
