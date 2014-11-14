tell application "Safari"  
    tell window 1  
        do JavaScript "window.location.reload(true)" in current tab  
    end tell  
end tell