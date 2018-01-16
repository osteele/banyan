module.exports = {
    presets: [require('poi-preset-elm')({ loaderOptions: { debug: false } })],
    env: {
        DROPBOX_APP_KEY: process.env.DROPBOX_APP_KEY
    },
    html: {
        title: 'Banyan',
    },
};
