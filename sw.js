self.addEventListener('message', event => {
  console.log('sw message event ' + event);
});

self.addEventListener('push', event => {
  console.log('sw push event ' + event);
});

self.addEventListener('pushsubscriptionchange', event => {
  console.log('sw push subscription change event ' + event);
});

self.addEventListener('sync', event => {
  console.log('sw sync ' + event);
});

self.addEventListener('online', event => {
  // not firing in FF
  console.log('sw online ' + event);
});

self.addEventListener('offline', event => {
  // not firing in Chrome
  console.log('sw offline ' + event);
});

self.addEventListener('pageshow', event =>  console.log('sw pageshow ' + event));
self.addEventListener('pagehide', event =>  console.log('sw pagehide ' + event));

self.addEventListener('freeze', event =>  console.log('sw freeze ' + event));
self.addEventListener('resume', event =>  console.log('sw resume ' + event));

self.addEventListener('unload', event => {
  console.log(`sw unload ${event}`);
});

self.addEventListener('activate', event => {
  console.log(`sw activate event ${event}`);
});

self.addEventListener('fetch', event => {
  console.log(`sw fetch event ${event}`);
});

self.addEventListener('install', (event) => {
  let ts = new Date();
  console.log(`Start install worker 101. Version 1. ${ts}`);

  // init cache
});
