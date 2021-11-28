# diskoedit
Functional Reactive Programming experiment resulting in a map editor for the game Diskophoros:  
Web: https://nmaehlmann.itch.io/diskophoros | Twitter: https://twitter.com/nmaehlmann

### Technologies used:
* Threepenny-GUI for UI and Functional Reactive Programming: https://wiki.haskell.org/Threepenny-gui
* JuicyPixels for generating textures: https://hackage.haskell.org/package/JuicyPixels
* parsec for parsing TMX files: https://hackage.haskell.org/package/parsec
* Electron for faster UI: https://electronjs.org/

### GIF preview:
![Map Editor GIF](http://u.cubeupload.com/nikolas/6HteTN.gif)

### Building and running with electron:
    npm install
    stack install --local-bin-path build
    ./node_modules/.bin/electron electron.js
    
### Building and running without electron (slower UI but does not require npm):
    stack build
    stack exec diskophoros-map-editor-exe
    
