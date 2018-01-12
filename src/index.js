'use strict';

require('./index.html');
var Elm = require('./Main');
var Dropbox = require('dropbox')

var app = Elm.Main.embed(document.getElementById('main'));
app.ports.dropboxClientID.send('client id (init)');

app.ports.requestConfig.subscribe(() => {
    console.info('requested client id')
    app.ports.dropboxClientID.send('client id');
    console.info('sent client id')
});

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
