// this file ("electron.js") was taken and modified from https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/doc/electron/electron.js
// under the following license:

// <BEGIN LICENSE electron.js>
// Copyright (c) 2012-2016, Threepenny-GUI authors (see CONTRIBUTORS)
// Copyright (c) 2012, Jeremy Bowers     <https://github.com/thejerf>
// Copyright (c) 2011-2012, Chris Done   <https://github.com/chrisdone>
// All rights reserved.

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:

//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.

//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.

//     * Neither the name of Heinrich Apfelmus nor the names of other
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.

// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// <END LICENSE electron.js>

const { app, BrowserWindow } = require('electron');
const freeport = require('freeport');
const spawn = require('child_process').spawn;
const path = require('path');
const waitOn = require('wait-on');

// Time to wait for Threepenny server, milliseconds
const timeout = 10000;
// Relative path to the Threepenny binary.
const relBin = './build/diskophoros-map-editor-exe.exe';
// Additional arguments to pass to the Threepenny binary.
const binArgs = ['otherArg1', 'otherArg2'];

// Assign a random port to run on.
freeport((err, port) => {
  if (err) throw err;

  const url = `http://localhost:${port}`;
  let child = null; // Server process we spawn and kill

  // Keep a global reference of the window object, if we don't, the window will
  // be closed automatically when the JavaScript object is garbage collected.
  let win;

  function createWindow() {
    // Create the browser window.
    win = new BrowserWindow({
      width: 1000,
      height: 570,
      webPreferences: { nodeIntegration: true },
    });

    console.log(`Loading URL: ${url}`);
    win.loadURL(url);

    // Emitted when the window is closed.
    win.on('closed', () => {
      // Dereference the window object for garbage collection.
      win = null;
    });
  }

  // Called when Electron has finished initialization and is ready to create
  // browser windows. Some APIs can only be used after this event occurs. We
  // start the child process and wait before loading the web page.
  app.on('ready', () => {
    child = spawn(path.join(__dirname, relBin), [port]);
    child.stdout.setEncoding('utf8');
    child.stderr.setEncoding('utf8');
    child.stdout.on('data', console.log);
    child.stderr.on('data', console.log);
    child.on('close', code =>
      console.log(`Threepenny app exited with code ${code}`));

    // Wait until the Threepenny server is ready for connections.
    waitOn({ resources: [url], timeout }, (err_) => {
      if (err_) throw err_;
      createWindow();
    });
  });

  // Quit when all windows are closed, unless on macOS. On macOS it is common
  // for applications and their menu bar to stay active until the user quits
  // explicitly with Cmd + Q
  app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
      app.quit();
    }
  });

  // Kill the child process when quitting Electron.
  app.on('will-quit', () => child.kill());

  app.on('activate', () => {
    // On macOS it's common to re-create a window in the app when the dock icon
    // is clicked and there are no other windows open.
    if (win === null) {
      createWindow();
    }
  });
});