# diskoedit

FRP Map editor for the game Diskophoros:  
Web: http://diskophoros.nikolikesco.de/ | Twitter: https://twitter.com/nmaehlmann

### Technologies used:
* Threepenny-GUI for UI and Functional Reactive Programming: https://wiki.haskell.org/Threepenny-gui
* JuicyPixels for generating textures: https://hackage.haskell.org/package/JuicyPixels
* parsec for parsing TMX files: https://hackage.haskell.org/package/parsec
* Electron for faster UI: https://electronjs.org/

### Building and running with electron
    npm install
    stack install --local-bin-path build
    ./node_modules/.bin/electron electron.js
    
### Building and running without electron (slower UI but does not require npm)
    stack build
    stack exec diskophoros-map-editor-exe
    
