function resolved(v) {
  return Promise.resolve(v);
}

function bufferToBase64(buffer) {
  return btoa(String.fromCharCode(...new Uint8Array(buffer)));
    // return [...new Uint8Array (buffer)]
    //     .map (b => b.toString (16).padStart (2, "0"))
    //     .join ("");
}

class PushSr {
  constructor(webPushPublicKey, cb) {
    this.webPushPublicKey = webPushPublicKey;
    this.cb = cb;
  }

  closeWebPushSubscription() {
    navigator.serviceWorker.ready.then(
          swr => swr.pushManager.getSubscription().then(
            subscription => {
              if (!!subscription) {
                subscription.unsubscribe().then(bo => {
                  console.log(`Unsubscribed ${bo}`);
                  this.cb("WpExpired");
                });
              }
            }
          ));
  }

  checkWebPushSubscription() {
    if (!this.isSupported()) {
      this.cb("WpNotSupported");
    } else {
      if (Notification.permission != 'granted' && Notification.permission != 'default') {
        console.log(`wp permission is ${Notification.permission}`);
        this.cb("WpNotGranted");
      } else {
        navigator.serviceWorker.ready.then(
          swr => swr.pushManager.getSubscription().then(
            subscription => {
              if (!!subscription) {
                let key = subscription.getKey ? bufferToBase64(subscription.getKey('p256dh')) : '';
                let auth = subscription.getKey ? bufferToBase64(subscription.getKey('auth')) : '';
                console.log(`wp key = ${key} and auth = ${auth}`);
                this.cb([key, auth, subscription.endpoint]);
              } else {
                this.cb("WpExpired");
              }
            },
            e => {
              console.log("Failed get subscription " + e);
              this.cb("WpExpired");
            }
          )).catch(e => {
            console.log("Failed2 get subscription " + e);
            this.cb("WpExpired");
          });
      }
    }
  }

  tryWebPushSubscription() {
    if (!this.isSupported()) {
      this.cb("WpNotSupported");
    } else {
      if (Notification.permission != 'granted' && Notification.permission != 'default') {
        console.log(`wp permission is ${Notification.permission}`);
        this.cb("WpNotGranted");
      } else {
        navigator.serviceWorker.ready.then(
          swr => swr.pushManager.getSubscription().then(
            subscription => {
              if (!!subscription) {
                let key = subscription.getKey ? bufferToBase64(subscription.getKey('p256dh')) : '';
                let auth = subscription.getKey ? bufferToBase64(subscription.getKey('auth')) : '';
                console.log(`wp key = ${key} and auth = ${auth}`);
                this.cb([key, auth, subscription.endpoint]);
              } else {
                this.subscribe();
              }
            },
            e => {
              console.log("Failed get subscription " + e);
              this.cb("WpExpired");
            }
          ));
      }
    }
  }

  subscribe() {
    console.log('lets subscribe');
    navigator.serviceWorker.ready.then(serviceWorkerRegistration => {
      console.log('sw ready for making subscribe');
      serviceWorkerRegistration.pushManager
        .subscribe({
          userVisibleOnly: true,
          applicationServerKey: this.webPushPublicKey
        })
        .then(
          subscription => {
            console.log(`have subscription ${subscription}`);
            let key = subscription.getKey ? bufferToBase64(subscription.getKey('p256dh')) : '';
            let auth = subscription.getKey ? bufferToBase64(subscription.getKey('auth')) : '';
            this.cb([key, auth, subscription.endpoint]);
          })
        .catch(e => {
          if (Notification.permission == 'denied') {
            console.warn('Permission for notification was denied');
            this.cb("WpNotGranted");
          } else {
            console.log(`Unable to subscribe to push ${e}`);
            this.cb("WpExpired");
          }
        });
    }).catch(e => {
      console.info(`sw is not ready for subscribe due ${e}`);
      this.cb("WpExpired");
    });
  }

  isSupported() {
    return typeof Notification != 'undefined'
      && typeof ServiceWorkerRegistration != 'undefined'
      && 'showNotification' in ServiceWorkerRegistration.prototype
      && typeof PushManager != "undefined";
  }
}
