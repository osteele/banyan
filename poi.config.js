module.exports = {
    presets: [require('poi-preset-elm')({
        loaderOptions: {
            debug: false, forceWatch: true
        }
    }
    )],
    env: {
        DROPBOX_APP_KEY: process.env.DROPBOX_APP_KEY
    }
    ,
    html: {
        title: 'Banyan',
    }
}

    ;
