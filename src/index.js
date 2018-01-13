'use strict';

require('./index.html');
var Elm = require('./Main');
var Dropbox = require('dropbox')

var app = Elm.Main.embed(document.getElementById('main'));
app.ports.dropboxClientID.send(process.env.DROPBOX_APP_KEY);

app.ports.listFiles.subscribe((accessToken) => {
    console.info('accessToken', accessToken)
    var dbx = new Dropbox({ accessToken });
    dbx.filesListFolder({ path: '' })
        .then(function (response) {
            console.log(response);
            app.ports.fileList.send("file list");
        })
        .catch(function (error) {
            console.log(error);
        });
})
