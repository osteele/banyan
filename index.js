'use strict';

require('./src/Main.css');
var Elm = require('./src/Main.elm');
var Dropbox = require('dropbox')

var app = Elm.Main.embed(document.getElementById('app'));
app.ports.dropboxClientID.send(process.env.DROPBOX_APP_KEY);

app.ports.listFiles.subscribe((accessToken, pages) => {
    pages = pages || null;
    var dbx = new Dropbox({ accessToken });
    var cache = localStorage['fileTree'] && JSON.parse(localStorage['fileTree']);
    var listFiles = (fn, pages) =>
        fn
            .then((response) => {
                var files = response.entries.map((entry) => {
                    return {
                        tag: entry['.tag']
                        , key: entry.path_lower
                        , path: entry.path_display
                        , size: entry.size || null
                    };
                });

                cache.cursor = response.cursor;
                files.forEach((entry) => {
                    cache.entries[entry.key] = entry;
                });
                localStorage['fileTree'] = JSON.stringify(cache);

                var more = (pages == null || --pages > 0) && response.has_more;
                app.ports.fileList.send([files, Boolean(more)]);

                if (more) {
                    listFiles(dbx.filesListFolderContinue({ cursor: response.cursor }), pages);
                }
            })
            .catch((error, response) => {
                console.error(error);
                app.ports.fileListError.send(); // error.name, error.message
            });
    if (cache) { // && cache.accessToken === accessToken) {
        app.ports.fileList.send([Object.values(cache.entries), true]);
        listFiles(dbx.filesListFolderContinue({ cursor: cache.cursor }), pages);
    } else {
        cache = { accessToken, entries: {} }
        listFiles(dbx.filesListFolder({ path: '', recursive: true }), pages);
    }
});

app.ports.getAccountInfo.subscribe((accessToken) => {
    var dbx = new Dropbox({ accessToken });
    dbx.usersGetCurrentAccount()
        .then((response) => {
            var teamName = response.team ? response.team.name : "Personal";
            app.ports.setAccountInfo.send({
                teamName,
                name: response.name
            });
        })
        .catch((error) => {
            console.log(error);
        })
});

app.ports.setLocalStore.subscribe(([key, value]) => {
    console.info('store', key, value);
    if (value) {
        localStorage[key] = value;
    } else {
        localStorage.removeItem(key)
    }
});

app.ports.getLocalStore.subscribe((key) => {
    console.info('fetch', key, localStorage[key]);
    app.ports.receiveLocalStore.send([key, localStorage[key]])
});
