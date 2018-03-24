import Dropbox from 'dropbox';
import 'semantic-ui-css/semantic.min.css';
import './src/Main.scss';

import Elm from './src/Main.elm';
// eslint-disable-next-line import/no-named-as-default, import/no-named-as-default-member
import treemap from './src/treemap';

const accessTokenKey = 'accessToken';
const oldFileCacheKey = 'fileTree';
const filesCacheKey = 'files';

const app = Elm.Main.embed(document.getElementById('app'), {
  accessToken: localStorage[accessTokenKey] || null,
  clientId: process.env.DROPBOX_APP_KEY,
  files: localStorage[filesCacheKey] || null,
});

app.ports.saveFilesCache.subscribe((cacheValue) => {
  const json = JSON.stringify(cacheValue);
  delete localStorage[oldFileCacheKey];
  console.info('write cache', json.length, 'bytes');
  localStorage[filesCacheKey] = json;
});

app.ports.getAccountInfo.subscribe(async (accessToken) => {
  const dbx = new Dropbox({ accessToken });
  const info = await dbx.usersGetCurrentAccount();
  // pre-condition for Elm, since we don't use a custom decoder
  info.account_type = info.account_type['.tag'];
  info.team = info.team || null;
  app.ports.receiveAccountInfo.send(camelizePropertyNames(info));
});

app.ports.storeAccessToken.subscribe((token) => {
  localStorage[accessTokenKey] = token;
});

app.ports.removeAccountInfo.subscribe(() => {
  localStorage.clear();
});

app.ports.renderTreemap.subscribe(([title, data]) => {
  const onClick = ({ key }) => key && app.ports.setPath.send(key);
  requestAnimationFrame(() => treemap(title, data, onClick));
});

function camelizePropertyNames(obj) {
  const result = {};
  Object.entries(obj).forEach(([k, v]) => {
    const kc = k.replace(/_([a-z])/gi, s => s.slice(1).toUpperCase());
    result[kc] = isObject(v) ? camelizePropertyNames(v) : v;
  });
  return result;
}

const isObject = v => v !== null && typeof v === 'object';
