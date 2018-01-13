'use strict';

require('./index.html');
var Elm = require('./Main');
var Dropbox = require('dropbox')

var app = Elm.Main.embed(document.getElementById('main'));
app.ports.dropboxClientID.send(process.env.DROPBOX_APP_KEY);

app.ports.listFiles.subscribe((accessToken) => {
    var dbx = new Dropbox({ accessToken });
    dbx.filesListFolder({ path: '' })
        .then(function (response) {
            var files = response.entries.map((entry) => {
                return { tag: entry['.tag'], path: entry.path_display, size: entry.size || 0 };
            })
            app.ports.fileList.send(files);
        })
        .catch(function (error) {
            console.log(error);
        });
});
