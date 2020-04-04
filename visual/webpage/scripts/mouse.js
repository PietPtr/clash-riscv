var MOUSEEVENTS = [ "click"
                  , "dblclick"
                  , "mousedown"
                  , "mouseup"
                  , "mouseenter"
                  , "mouseleave"
                  , "mousemove"
                  ];

var TOUCHEVENTS = [ "touchstart"
                  , "touchend"
                  , "touchmove"];

var MOUSEWS = new Websocket( "Mouse"
                           , IPADDRESS
                           , MOUSEPORT
                           , setConnected
                           , doNothing
                           , reportOnError
                           , mouseClose
                           );


function CapturableMouseElements() {
    this.elements = [];


    this.addCaptureElement = function(name, type, domElement) {
        var mouseElement = new MouseElement(name, type, domElement);
        this.elements.push(mouseElement);
    };

    this.removeCaptureElementByid = function(id) {
        this.elements = this.elements.filter( function(el) {
            return el.id == id;
        });
    };

    this.removeCaptureElementByDomElement = function(domElement) {
        this.elements = this.elements.filter( function(el) {
            return el.domElement == domElement;
        });
    };

    this.getRelativeMousePositions = function(x, y) {
        return this.elements.map( function(el) {
            return el.getPositionRelativeToWindow(x, y);
        });
    }
}

var MOUSECAPTUREELEMENTS = new CapturableMouseElements();

function MouseElement(id, type, domElement) {
    this.id = id;
    this.type = type;
    this.domElement = domElement;

    this.getOrigin = function() {
        var clientRect = this.domElement.getBoundingClientRect();

        return [clientRect.left, clientRect.top];
    }

    this.getPositionRelativeToWindow = function(x, y) {
        var elementOrigin = this.getOrigin();

        return new RelativeMousePosition(this, x - elementOrigin[0], y - elementOrigin[1]);
    }
}

function RelativeMousePosition(mouseElement, x, y) {
    this.mouseElement = mouseElement;
    this.x = x;
    this.y = y;

    this.toJSONObject = function (mouseEventType, button) {
        return { "elementType": this.mouseElement.type
               , "mouseEventType": mouseEventType
               , "button" : button
               , "id": this.mouseElement.id
               , "x": x
               , "y": y
               };
    }
}



// Mouse Module functions
function mouseInit() {
    MOUSEEVENTS.forEach(function (mouseEvent, index) {
        document.addEventListener(mouseEvent, mouseEventListener);
    });
    TOUCHEVENTS.forEach(function (touchEvent, index) {
        document.writeln("adding event listener for: " + touchEvent);
        document.addEventListener(touchEvent, touchEventListener);
    });
    SOCKETSTOCONNECT.addToConnect(MOUSEWS);
}

function mouseClose(event) {
    MOUSECAPTUREELEMENTS = new CapturableMouseElements();
    this.parent.connected = false;
    retryOnUncleanCloseH(this, event);
}

function mouseEventListener(mouseEvent) {
    var mouseEventType = mouseEvent.type;
    var button = toMouseButton(mouseEvent.button);
    var x = mouseEvent.clientX;
    var y = mouseEvent.clientY;

    var relativeMousePositions = MOUSECAPTUREELEMENTS.getRelativeMousePositions(x, y);
    var jsonObjects = relativeMousePositions.map(function (el) {
                                                    return el.toJSONObject(mouseEventType, button);
                                                });

    if (MOUSEWS.connected) {
        for(var i = 0; i < jsonObjects.length; i++) {
            MOUSEWS.sendJSONObject(jsonObjects[i]);
        }
    }
}

function touchEventListener(ev) {
    var mouseEventType;
    switch (ev.type) {
      case "touchstart":
        mouseEventType = "mousedown";
        break;
      case "touchmove":
        mouseEventType = "mousemove";
        break;
      case "touchend":
        mouseEventType = "mouseup";
        break;
    }
    var button = "left"; // toMouseButton(mouseEvent.button);
    var touches = ev.changedTouches;
    var x = 0;
    var y = 0;
    if(touches.length > 0) {
      var t = touches[0];
      x = t.clientX;
      y = t.clientY;

      var relativeMousePositions = MOUSECAPTUREELEMENTS.getRelativeMousePositions(x, y);
      var jsonObjects = relativeMousePositions.map(function (el) {
                                                      return el.toJSONObject(mouseEventType, button);
                                                  });

      if (MOUSEWS.connected) {
          for(var i = 0; i < jsonObjects.length; i++) {
              MOUSEWS.sendJSONObject(jsonObjects[i]);
          }
      }
    }
}


function toMouseButton(which) {
    var button = "";

    switch (which) {
        case 0 : button = "left"; break;
        case 1 : button = "middle"; break;
        case 2 : button = "right"; break;
    }

    return button;
}

mouseInit();
