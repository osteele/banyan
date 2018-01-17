import './src/Main.css';

import Dropbox from 'dropbox';
import Elm from './src/Main.elm';
import treeMap from './src/treeMap.js';

const accessTokenKey = "accessToken";
const app = Elm.Main.embed(document.getElementById('app'), {
    accessToken: localStorage[accessTokenKey] || null,
    clientId: process.env.DROPBOX_APP_KEY
});

app.ports.listFiles.subscribe(async (accessToken) => {
    const path = '';
    const dbx = new Dropbox({ accessToken });
    let cache = localStorage['fileTree'] && JSON.parse(localStorage['fileTree']);
    if (cache && cache.accessToken !== accessToken) {
        console.info('reset cache')
        cache = null;
    }
    cache = cache || { accessToken, entries: {} };
    let entries = Object.values(cache.entries);
    if (entries.length) {
        console.info('initial send', entries.length, 'entries');
        app.ports.fileList.send([entries, true]);
    }
    let query = cache.cursor
        ? dbx.filesListFolderContinue({ cursor: cache.cursor })
        : dbx.filesListFolder({ path, recursive: true });
    while (query) {
        var response
        try {
            response = await query;
        } catch (error) {
            console.error(error);
            app.ports.fileListError.send(); // error.name, error.message
        }

        let entries = response.entries.map((entry) => {
            return {
                tag: entry['.tag']
                , key: entry.path_lower
                , path: entry.path_display
                , size: entry.size || null
            };
        });

        // update the cache
        cache.cursor = response.cursor;
        entries.forEach((entry) => {
            cache.entries[entry.key] = entry;
        });
        localStorage['fileTree'] = JSON.stringify(cache);

        const more = Boolean(response.has_more);
        app.ports.fileList.send([entries, more]);

        query = more && dbx.filesListFolderContinue({ cursor: response.cursor });
    }
});

app.ports.getAccountInfo.subscribe(accessToken => {
    const dbx = new Dropbox({ accessToken });
    dbx.usersGetCurrentAccount()
        .then((response) => {
            const teamName = response.team ? response.team.name : "Personal";
            app.ports.setAccountInfo.send({
                teamName,
                name: response.name
            });
        })
        .catch((error) => {
            console.log(error);
        })
});

app.ports.storeAccessToken.subscribe((value) => {
    const key = accessTokenKey;
    if (value) {
        localStorage[key] = value;
    } else {
        localStorage.clear();
        // localStorage.removeItem(key)
    }
});

app.ports.drawTreeMap.subscribe(([title, data]) =>
    requestAnimationFrame(() => treeMap(title, data))
);
