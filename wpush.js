function resolved(v) {
  return Promise.resolve(v);
}

function bufferToHex(buffer) {
    return [...new Uint8Array (buffer)]
        .map (b => b.toString (16).padStart (2, "0"))
        .join ("");
}

function boom(e) {
  return Promise.reject(e);
}

function sendSubscriptionToServer(subscription) {
  let key = subscription.getKey ? subscription.getKey('p256dh') : '';
  let auth = subscription.getKey ? subscription.getKey('auth') : '';

  console.info(`Send key ${key} and auth ${auth} to server`);
}

class PushSr {
  constructor(cb) {
    this.cb = cb;
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
                let key = subscription.getKey ? bufferToHex(subscription.getKey('p256dh')) : '';
                let auth = subscription.getKey ? bufferToHex(subscription.getKey('auth')) : '';
                console.log(`wp key = ${key} and auth = ${auth}`);
                this.cb([key, auth]);
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
                let key = subscription.getKey ? bufferToHex(subscription.getKey('p256dh')) : '';
                let auth = subscription.getKey ? bufferToHex(subscription.getKey('auth')) : '';
                console.log(`wp key = ${key} and auth = ${auth}`);
                this.cb([key, auth]);
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
          applicationServerKey: "BA5wU2icgNXzmjTG5f_ba0tliY9OezaKETqpl"
            + "E7uoe16sF258jTIDJFkHeLLoCpwNsKiNMoFNEK2Z5Yc-zKyTFY"
        })
        .then(
          subscription => {
            console.log(`have subscription ${subscription}`);
            let key = subscription.getKey ? subscription.getKey('p256dh') : '';
            let auth = subscription.getKey ? subscription.getKey('auth') : '';
            this.cb([key, auth]);
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
