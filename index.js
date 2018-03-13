import Dropbox from 'dropbox';
import 'semantic-ui-css/semantic.min.css';
import './src/Main.scss';

import Elm from './src/Main.elm';
// eslint-disable-next-line import/no-named-as-default, import/no-named-as-default-member
import chart from './src/chart';

const accessTokenKey = 'accessToken';
const fileCacheKey = 'fileTree';
// const filesCacheKey = 'files';

const app = Elm.Main.embed(document.getElementById('app'), {
  accessToken: localStorage[accessTokenKey] || null,
  clientId: process.env.DROPBOX_APP_KEY,
});

// See https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder
app.ports.listFolder.subscribe(async ([accessToken, params]) => {
  const useCache = false;
  if (!accessToken) {
    app.ports.receiveFileListError.send('internal error: access token not set');
    return;
  }
  const dbx = new Dropbox({ accessToken });

  // If there's a cached entry, return it first.
  // TODO Make this a separate function.
  let cache = null;
  if (useCache && localStorage[fileCacheKey]) {
    cache = JSON.parse(localStorage[fileCacheKey]);
    console.info('read cache', (localStorage[fileCacheKey] || '').length);
  }
  if (cache && cache.accessToken !== accessToken) {
    console.info('reset cache');
    cache = null;
  }
  cache = cache || { accessToken, entries: {} };
  {
    const entries = Object.values(cache.entries);
    if (entries.length) {
      console.info('initial send', entries.length, 'entries');
      app.ports.receiveFileList.send([entries, false]);
    }
  }

  let query = cache.cursor
    ? dbx.filesListFolderContinue({ cursor: cache.cursor })
    : dbx.filesListFolder({
      path: params.path,
      recursive: params.recursive,
      include_deleted: params.includeDeleted,
    });

  // eslint-disable-next-line no-await-in-loop
  while (query) {
    let response;
    try {
      response = await query;
    } catch (error) {
      console.error('listFiles:', error);
      app.ports.receiveFileListError.send(error.message || error.error);
      return;
    }

    const entries = response.entries.map(entry => ({
      tag: entry['.tag'],
      name: entry.name,
      path_lower: entry.path_lower,
      path_display: entry.path_display,
      size: entry.size || null,
    }));
    const deleted = response.entries.filter(({ tag }) => tag === 'deleted');
    if (deleted.length) {
      console.info('deleted', deleted);
    }

    cache.cursor = response.cursor;
    if (useCache) {
      entries.forEach((entry) => {
        cache.entries[entry.key] = entry;
      });
      localStorage[fileCacheKey] = JSON.stringify(cache);
      console.info('wrote cache', localStorage[fileCacheKey].length);
    }

    const hasMore = Boolean(response.has_more);
    // initiate the query before processing these entries, in case this allows
    // for greater parallelism
    query = hasMore && dbx.filesListFolderContinue({ cursor: response.cursor });
    app.ports.receiveFileList.send([entries, hasMore]);
  }
});

app.ports.saveFilesCache.subscribe((cacheValue) => {
  console.info('write cache', JSON.stringify(cacheValue).length);
  // localStorage[filesCacheKey] = cacheValue;
});

app.ports.getAccountInfo.subscribe(async (accessToken) => {
  const dbx = new Dropbox({ accessToken });
  const { name, team } = await dbx.usersGetCurrentAccount();
  const teamName = team ? team.name : 'Personal';
  app.ports.receiveAccountInfo.send({ teamName, name });
});

app.ports.storeAccessToken.subscribe((token) => {
  localStorage[accessTokenKey] = token;
});

app.ports.removeAccountInfo.subscribe(() => {
  localStorage.clear();
});

app.ports.chart.subscribe(([title, data]) => {
  const onClick = ({ key }) => key && app.ports.setPath.send(key);
  requestAnimationFrame(() => chart(title, data, onClick));
});
