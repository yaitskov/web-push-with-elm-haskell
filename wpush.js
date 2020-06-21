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

function urlBase64ToUint8Array(base64String) {
    var padding = '='.repeat((4 - base64String.length % 4) % 4);
    var base64 = (base64String + padding)
        .replace(/\-/g, '+')
        .replace(/_/g, '/');

    var rawData = window.atob(base64);
    var outputArray = new Uint8Array(rawData.length);

    for (var i = 0; i < rawData.length; ++i) {
        outputArray[i] = rawData.charCodeAt(i);
    }
    return outputArray;
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
    let te = new TextEncoder();
    navigator.serviceWorker.ready.then(serviceWorkerRegistration => {
      console.log('sw ready for making subscribe');
      serviceWorkerRegistration.pushManager
        .subscribe({
          userVisibleOnly: true,
          applicationServerKey: urlBase64ToUint8Array("BA5wU2icgNXzmjTG5f_ba0tliY9OezaKETqplE7uoe16sF258jTIDJFkHeLLoCpwNsKiNMoFNEK2Z5Yc-zKyTFY")
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
