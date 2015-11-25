"use strict";

function isTouchDevice() {
  return 'onmsgesturechange' in window;
}

var initialValues = {
  isTouchDevice: isTouchDevice()
};

var elmApp = Elm.fullscreen(Elm.Main, initialValues);

elmApp.ports.isTouchDevice.send(isTouchDevice());
