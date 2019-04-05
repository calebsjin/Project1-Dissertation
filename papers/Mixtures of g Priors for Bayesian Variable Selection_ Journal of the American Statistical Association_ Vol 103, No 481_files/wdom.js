"use strict";

var _this = void 0;

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; var ownKeys = Object.keys(source); if (typeof Object.getOwnPropertySymbols === 'function') { ownKeys = ownKeys.concat(Object.getOwnPropertySymbols(source).filter(function (sym) { return Object.getOwnPropertyDescriptor(source, sym).enumerable; })); } ownKeys.forEach(function (key) { _defineProperty(target, key, source[key]); }); } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _typeof(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

var wdom = {
  loadScript: function loadScript(src) {
    var ver = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : Date.now();
    var onload = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : wdom.noop;
    return document.head.appendChild(wdom.createScript(src, ver, onload));
  },
  createScript: function createScript(src) {
    var ver = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : Date.now();
    var onload = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : wdom.noop;
    var async = arguments.length > 3 ? arguments[3] : undefined;
    var defer = arguments.length > 4 ? arguments[4] : undefined;
    var id = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : '';
    if (id && document.getElementById(id) != null) return;
    var script = document.createElement('script');
    script.type = "text/javascript";
    script.innerHTML = '';
    script.src = src + (ver != null ? '?v=' + ver : '');
    script.onload = onload;
    script.async = async;
    script.defer = defer;
    if (id) script.id = id;
    return script;
  },
  addClass: function addClass(el) {
    for (var _len = arguments.length, className = new Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
      className[_key - 1] = arguments[_key];
    }

    return wdom.nodeToArray(el).forEach(function (e) {
      return className.forEach(function (name) {
        return e.classList.add(name);
      });
    });
  },
  removeClass: function removeClass(el) {
    for (var _len2 = arguments.length, className = new Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
      className[_key2 - 1] = arguments[_key2];
    }

    return wdom.nodeToArray(el).forEach(function (e) {
      return className.forEach(function (name) {
        return e.classList.remove(name);
      });
    });
  },
  getString: function getString() {
    var text = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    return text;
  },
  hasClass: function hasClass(el, className) {
    var arrayList = wdom.nodeToArray(el);
    return arrayList.length > 0 && arrayList[0].classList.contains(className);
  },
  offset: function offset(el) {
    var arrayList = wdom.nodeToArray(el);

    if (arrayList.length > 0) {
      var element = arrayList[0];
      var box = element.getBoundingClientRect();
      return {
        top: box.top + window.pageYOffset - document.documentElement.clientTop,
        left: box.left + window.pageXOffset - document.documentElement.clientLeft
      };
    }
  },
  parents: function parents(el, parentSelector) {
    // If no parentSelector defined will bubble up all the way to *document*
    if (parentSelector === undefined) {
      parentSelector = [document];
    }

    var parents = [];
    var p = el.parentNode;

    while (parentSelector.filter(function (ps) {
      return ps === p;
    }).length > 0) {
      var o = p;
      parents.push(o);
      p = o.parentNode;
    }

    return parents;
  },
  el: function el(selector) {
    var host = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : document;
    var first = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;
    var list = null;

    if (selector instanceof HTMLElement) {
      list = selector;
    } else if (_typeof(selector) === 'object' && selector.self === window) {
      list = selector;
    } else {
      if (typeof host === 'string') host = document.querySelector(host);
      list = host.querySelectorAll(selector);
    }

    var list2 = wdom.nodeToArray(list);
    return first ? list2[0] : list2;
  },
  prev: function prev(el) {
    var elList = wdom.nodeToArray(el);

    if (elList.length > 0) {
      return elList[0].previousElementSibling;
    }

    return null;
  },
  next: function next(el) {
    var elList = wdom.nodeToArray(el);

    if (elList.length > 0) {
      return elList[0].nextElementSibling;
    }

    return null;
  },
  toggleClass: function toggleClass(el, className) {
    var elList = wdom.nodeToArray(el);

    if (elList.length > 0) {
      elList[0].classList.toggle(className);
    }
  },
  removeElement: function removeElement(el) {
    var elList = wdom.nodeToArray(el);

    if (elList.length > 0 && elList[0].parentElement) {
      elList[0].parentElement.removeChild(el);
    }
  },
  isNodeList: function isNodeList(el) {
    return el !== null && el instanceof NodeList;
  },
  hide: function hide(el) {
    var displayProp = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'none';
    return wdom.nodeToArray(el).forEach(function (e) {
      e.style.display = displayProp;
      addClass(e, 'hidden');
    });
  },
  toggle: function toggle(el) {
    return wdom.nodeToArray(el).forEach(function (e) {
      var visiblity = !(e.style.display === 'none' || e.classList.contains('hidden'));
      if (visiblity) wdom.hide(e);else wdom.show(e);
    });
  },
  show: function show(el) {
    var displayProp = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'block';
    return wdom.nodeToArray(el).forEach(function (e) {
      e.style.display = displayProp;
      removeClass(e, 'hidden');
    });
  },
  fadeOut: function fadeOut(elem) {
    var callback = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : wdom.noop;
    wdom.nodeToArray(elem).forEach(function (el) {
      if (el.style.opacity === '' || el.style.opacity > 0) {
        var opacity = 1;
        var last = +new Date();

        var tick = function tick() {
          if (opacity < 0.1) {
            el.style.opacity = 0;
            el.style.display = 'none';
            callback();
            return;
          } else {
            window.requestAnimationFrame && requestAnimationFrame(tick) || setTimeout(tick, 16);
          }

          el.style.opacity = opacity;
          opacity -= (new Date() - last) / 250;
          last = +new Date();
        };

        tick();
      }
    });
  },
  fadeIn: function fadeIn(elem) {
    var callback = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : wdom.noop;
    wdom.nodeToArray(elem).forEach(function (el) {
      if (el.style.opacity === '' || el.style.opacity < 1) {
        var op = 0;
        el.style.opacity = op;
        el.style.display = 'inline-block';
        var last = +new Date();

        var tick = function tick() {
          el.style.opacity = +el.style.opacity + (new Date() - last) / 250;
          last = +new Date();

          if (+el.style.opacity < 1) {
            window.requestAnimationFrame && requestAnimationFrame(tick) || setTimeout(tick, 16);
          } else {
            callback();
          }
        };

        tick();
      }
    });
  },
  visible: function visible(el) {
    var elist = wdom.nodeToArray(el);

    if (elist.length > 0) {
      var elem = elist[0];
      return elem.offsetWidth > 0 && elem.offsetHeight > 0;
    }

    return false;
  },
  isHidden: function isHidden(el) {
    var elist = wdom.nodeToArray(el);

    if (elist.length > 0) {
      var elem = elist[0];
      return window.getComputedStyle(el).display === 'none';
    }

    return false;
  },
  height: function height(elem) {
    var elist = wdom.nodeToArray(elem);

    if (elist.length > 0) {
      var el = elist[0];
      var styles = window.getComputedStyle(el);
      var height = el.offsetHeight;
      var borderTopWidth = parseFloat(styles.borderTopWidth);
      var borderBottomWidth = parseFloat(styles.borderBottomWidth);
      var paddingTop = parseFloat(styles.paddingTop);
      var paddingBottom = parseFloat(styles.paddingBottom);
      return height - borderBottomWidth - borderTopWidth - paddingTop - paddingBottom;
    }

    return 0;
  },
  scrollTop: function scrollTop(elem, _scrollTop) {
    var elist = wdom.nodeToArray(elem);
    var setScrollTop = typeof _scrollTop !== 'undefined';

    if (elist.length > 0) {
      var _elem = elist[0];

      if (_elem.self === window) {
        if (setScrollTop) {
          document.documentElement.scrollTop = _scrollTop;
          return;
        } else {
          return document.documentElement.scrollTop;
        }
      }

      if (setScrollTop) {
        _elem.scrollTop = _scrollTop;
        return;
      } else {
        return _elem.scrollTop;
      }
    }

    return 0;
  },
  val: function val(el, value) {
    var elList = wdom.nodeToArray(el);
    var setValue = typeof value !== 'undefined';
    var emap = elList.map(function (el, idx) {
      if (setValue) {
        el.value = value;
        return;
      } else {
        return el.value;
      }
    });
    return (emap.length === 1 ? emap[0] : emap) || '';
  },
  text: function text(el, value) {
    var elList = wdom.nodeToArray(el);
    var setText = typeof value !== 'undefined';
    var emap = elList.map(function (el, idx) {
      if (setText) {
        el.innerText = value;
        return;
      } else {
        return el.innerText;
      }
    });
    return (emap.length === 1 ? emap[0] : emap) || '';
  },
  html: function html(el, value) {
    var elList = wdom.nodeToArray(el);
    var setText = typeof value !== 'undefined';
    var emap = elList.map(function (el, idx) {
      if (setText) {
        el.innerHtml = value;
        return;
      } else {
        return el.innerHtml;
      }
    });
    return (emap.length === 1 ? emap[0] : emap) || '';
  },
  clientHeight: function clientHeight(elem) {
    var elist = wdom.nodeToArray(elem);

    if (elist.length > 0) {
      if (elist[0].self === window) return elist[0].innerHeight;
      return elist[0].clientHeight;
    }

    return 0;
  },
  clientWidth: function clientWidth(elem) {
    var elist = wdom.nodeToArray(elem);

    if (elist.length > 0) {
      if (elist[0].self === window) return elist[0].innerWidth;
      return elist[0].clientWidth;
    }

    return 0;
  },
  width: function width(el) {
    var elist = wdom.nodeToArray(el);

    if (elist.length > 0) {
      var _el = elist[0];
      return _el.offsetWidth;
    }

    return 0;
  },
  scrollEl: function scrollEl(elem, to, duration) {
    var elist = wdom.nodeToArray(elem);

    if (elist.length > 0) {
      var element = elist[0];
      var start = element.scrollTop,
          change = to - start,
          currentTime = 0,
          increment = 20;

      var animateScroll = function animateScroll() {
        currentTime += increment;

        var easeInOutQuad = function easeInOutQuad(t, b, c, d) {
          t /= d / 2;
          if (t < 1) return c / 2 * t * t + b;
          t--;
          return -c / 2 * (t * (t - 2) - 1) + b;
        };

        var val = easeInOutQuad(currentTime, start, change, duration);
        element.scrollTop = val;

        if (currentTime < duration) {
          setTimeout(animateScroll, increment);
        }
      };

      animateScroll();
    }
  },
  animate: function animate(elem, params) {
    var animation = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 'linear';
    var duration = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 300;
    var cb = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : wdom.noop;
    wdom.nodeToArray(elem).forEach(function (el) {
      var transendfunc = function transendfunc() {
        el.removeEventListener('transitionend', transendfunc, false);
        cb();
      };

      el.addEventListener('transitionend', transendfunc, false);
      el.style.transition = animation + ' ' + duration;
      Object.keys(params).forEach(function (key) {
        return el.style[key] = params[key];
      });
    });
  },
  nodeToArray: function nodeToArray(el) {
    if (el != null) {
      if (typeof el === 'string') {
        return wdom.el(el);
      } else if (wdom.isNodeList(el)) {
        return Array.prototype.slice.call(el);
      } else if (el instanceof Array) {
        return el;
      } else {
        return [el];
      }
    } else {
      return [];
    }
  },
  triggerEvent: function triggerEvent(elem, ev) {
    var payload = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : undefined;
    wdom.el(elem).forEach(function (el) {
      var event = null;

      if (window.CustomEvent) {
        event = new CustomEvent(ev, payload);
      } else {
        event = document.createEvent('CustomEvent');
        event.initCustomEvent(ev, true, true, payload);
      }

      el.dispatchEvent(event);
    });
  },
  onReady: function onReady(callback) {
    // window.onload = callback;
    if (document.readyState !== 'loading') callback();else if (document.addEventListener) document.addEventListener('DOMContentLoaded', callback);else document.attachEvent('onreadystatechange', function () {
      if (document.readyState === 'complete') callback();
    });
  },
  closest: function closest(elem, selector) {
    var elist = wdom.nodeToArray(elem);

    if (elist.length > 0) {
      var el = elist[0];
      var matchesFn;
      ['matches', 'webkitMatchesSelector', 'mozMatchesSelector', 'msMatchesSelector', 'oMatchesSelector'].some(function (fn) {
        if (typeof document.body[fn] === 'function') {
          matchesFn = fn;
          return true;
        }

        return false;
      });
      var parent;

      while (el) {
        parent = el.parentElement;

        if (parent && parent[matchesFn](selector)) {
          return parent;
        }

        el = parent;
      }

      return null;
    }
  },
  appendHTML: function appendHTML(elem, html) {
    return wdom.nodeToArray(elem).forEach(function (el) {
      return el.insertAdjacentHTML('beforeend', html);
    });
  },
  appendEl: function appendEl(elem, newEl) {
    return wdom.nodeToArray(elem).forEach(function (el) {
      return el.appendChild(newEl);
    });
  },
  getAttribute: function getAttribute(elem, attr) {
    var elements = wdom.nodeToArray(elem);

    if (elements.length > 0) {
      var el = elements[0];
      return el.getAttribute(attr) || '';
    }
  },
  removeAttribute: function removeAttribute(elem, attr) {
    return wdom.nodeToArray(elem).forEach(function (el) {
      return el.removeAttribute(attr);
    });
  },
  setAttribute: function setAttribute(elem, attrName, attrValue) {
    return wdom.nodeToArray(elem).forEach(function (el) {
      return el.setAttribute(attrName, attrValue);
    });
  },
  inViewportAny: function inViewportAny(elem) {
    var elist = wdom.nodeToArray(elem);

    if (elist.length > 0) {
      var el = elist[0];
      var frame = el.getBoundingClientRect();
      var windowHeight = window.innerHeight || document.documentElement.clientHeight;
      var windowWidth = window.innerWidth || document.documentElement.clientWidth;
      var vertically = frame.top <= windowHeight && frame.top + frame.height - wdom.clientHeight('header') - wdom.clientHeight('.services-nav') >= 0;
      var horizontally = frame.left <= windowWidth && frame.left + frame.width >= 0;
      return vertically && horizontally;
    }

    return false;
  },
  inViewPort: function inViewPort(elem) {
    var elist = wdom.nodeToArray(elem);

    if (elist.length > 0) {
      var el = elist[0];
      var elSize = el.offsetWidth * el.offsetHeight;
      var rec = el.getBoundingClientRect();
      var tViz = rec.top >= 0 && rec.top < window.innerHeight - wdom.clientHeight('header') - wdom.clientHeight('.services-nav');
      var bViz = rec.bottom > 0 && rec.bottom <= window.innerHeight;
      var lViz = rec.left >= 0 && rec.left < window.innerWidth;
      var rViz = rec.right > 0 && rec.right <= window.innerWidth;
      var vVisible = tViz || bViz;
      var hVisible = lViz || rViz;
      return !!(elSize && vVisible && hVisible);
    } else {
      return false;
    }
  },
  parseNum: function parseNum(numb) {
    var number = NaN;

    switch (_typeof(numb)) {
      case 'string':
        number = parseFloat(numb.replace(/,/g, ''));
        break;

      case 'number':
        number = numb;
        break;
    }

    return number;
  },
  contains: function contains(str, substr) {
    return str.indexOf(substr) !== -1;
  },
  hasMinLength: function hasMinLength() {
    var text = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    var length = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
    return !(text && text.length < length);
  },
  hasMaxLength: function hasMaxLength() {
    var text = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    var length = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
    return !(text && text.length > length);
  },
  isEmail: function isEmail(str) {
    return /^([a-zA-Z0-9_\-.+]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(]?)$/.test(str);
  },
  isEmailSeries: function isEmailSeries(str) {
    return /^(([a-zA-Z0-9_\-.+]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(]?)(,\s*|;\s*)?)+$/.test(str);
  },
  removeNullAttributes: function removeNullAttributes(obj) {
    for (var key in obj) {
      if (obj[key] === null) {
        delete obj[key];
      }
    }

    return obj;
  },
  uniqueStamp: function uniqueStamp() {
    var length = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 8;
    var str = Date.now().toString(36);

    for (var i = 1; i < length + 1; i = i + 8) {
      str += Math.random().toString(36).substr(2, 10);
    }

    return str.substr(0, length);
  },
  beautify: function beautify() {
    var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '0';
    var decimalPoint = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 2;
    var num = parseFloat(value);
    var result = '';

    if (!isNaN(num)) {
      var decimal = num % 1 !== 0;
      var bigNumber = num > 1000;
      result = decimal && !bigNumber ? num.toFixed(decimalPoint) : wdom.convertNumberToString(num, 0);
    }

    return result;
  },
  capitalize: function capitalize(str) {
    return str ? str[0].toUpperCase() + str.slice(1) : '';
  },
  convertNumberToString: function convertNumberToString(number) {
    var decimalPoints = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : undefined;

    try {
      var formattedNumber;

      var _wdom$computeCurrency = wdom.computeCurrencyRange(number),
          computedNumber = _wdom$computeCurrency.computedNumber,
          postfix = _wdom$computeCurrency.postfix;

      var retVal;

      switch (_typeof(computedNumber)) {
        case 'string':
          retVal = computedNumber;
          break;

        case 'number':
          var fixedPoint;

          if (typeof decimalPoints === 'undefined') {
            fixedPoint = computedNumber > 100 ? 0 : computedNumber > 10 ? 1 : 2;
          } else {
            fixedPoint = decimalPoints;
          }

          formattedNumber = wdom.format(computedNumber.toFixed(fixedPoint));
          retVal = formattedNumber + postfix;
          break;
      }

      return retVal;
    } catch (err) {
      return null;
    }
  },
  computeCurrencyRange: function computeCurrencyRange(numb) {
    var rangeIdx = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
    var postfixes = ['T', 'B', 'M', 'K'];
    var maxRanges = [1000000000000, 1000000000, 1000000, 1000];

    if (rangeIdx < postfixes.length) {
      if (typeof numb === 'string') {
        var newnumb = wdom.parseNum(numb);

        if (isNaN(newnumb)) {
          return {
            computedNumber: numb,
            postfix: ''
          };
        } else {
          numb = newnumb;
        }
      }

      var range = maxRanges[rangeIdx];

      if (rangeIdx > maxRanges.length - 1) {
        return {
          computedNumber: numb,
          postfix: ''
        };
      }

      if (numb % range !== numb) {
        return {
          computedNumber: numb / range,
          postfix: postfixes[rangeIdx]
        };
      } else {
        return wdom.computeCurrencyRange(numb, ++rangeIdx);
      }
    } else {
      return {
        computedNumber: numb,
        postfix: ''
      };
    }
  },
  isEmpty: function isEmpty() {
    var str = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    return str.length === 0;
  },
  on: function on(el, event, handler) {
    var data = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : {};
    wdom.nodeToArray(el).forEach(function (elem) {
      var evHandler = function evHandler(e) {
        if (!e.data) e.data = {};
        Object.keys(data).forEach(function (k) {
          return e.data[k] = data[k];
        });
        handler.call(_this, e);
      };

      elem.removeEventListener(event, evHandler);
      elem.addEventListener(event, evHandler);
    });
  },
  off: function off(el, event, handler) {
    return wdom.nodeToArray(el).forEach(function (elem) {
      return elem.removeEventListener(event, handler);
    });
  },
  ajax: function ajax(url) {
    var type = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "GET";
    var async = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : true;
    var onSuccess = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : wdom.noop;
    var onError = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : wdom.noop;
    var data = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : undefined;
    var headers = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : undefined;
    var request = new XMLHttpRequest();
    request.open(type, url, async);

    request.onreadystatechange = function () {
      if (request.readyState === XMLHttpRequest.DONE) {
        if (request.status === 200) {
          onSuccess(request);
        } else if (request.status >= 400 && request.status <= 600) {
          onError(request);
        }
      }
    };

    if (data && type === 'POST') {
      request.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
      data = JSON.stringify(data);
    }

    if (headers) Object.keys(headers).forEach(function (header) {
      return request.setRequestHeader(header, headers[header]);
    });
    request.send(data);
    return request;
  },
  emptyJSON: function emptyJSON() {
    var tagName = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 'Report';
    var obj = {};
    obj[tagName] = [];
    return obj;
  },
  //format: num => typeof num === 'number' ? parseFloat(num.toString()) : parseFloat(num).toLocaleString('en-US'),
  format: function format(num) {
    var retVal = typeof num === 'number' ? parseFloat(num.toString()) : parseFloat(num);
    return retVal.toLocaleString('en-US');
  },
  noop: function noop() {},
  decodeHTMLEntities: function decodeHTMLEntities(encodedString) {
    var tmpElement = document.createElement('textarea');
    tmpElement.innerHTML = encodedString;
    return tmpElement.value;
  },
  isJSON: function isJSON(data) {
    var parsedObj = undefined;

    try {
      parsedObj = JSON.parse(data);
    } catch (ex) {
      parsedObj = '';
    }

    return !!parsedObj;
  },
  isHTML: function isHTML(data) {
    if (typeof data === 'string') {
      try {
        JSON.parse(data);
      } catch (ex) {
        var a = document.createElement('div');
        a.innerHTML = data;

        for (var c = a.childNodes, i = c.length; i--;) {
          if (c[i].nodeType === 1) {
            return true;
          }
        }
      }
    }

    return false;
  },
  parseParams: function parseParams() {
    var str = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : location.search;
    var paramsBlock = str.split("?")[1];
    var paramMap = new Map();

    if (paramsBlock) {
      paramsBlock.split('&').forEach(function (p) {
        var components = p.split('=');
        paramMap.set(components[0], components[1]);
      });
    }

    return paramMap;
  },
  unique: function unique(arr) {
    return Array.from(new Set(arr)).sort();
  },
  trim: function trim() {
    var str = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    var newStr = '';

    try {
      if (str && str != null && str.length > 0) newStr = str;
      newStr = newStr.replace(/^[\xA0\s]+/, '').replace(/[\xA0\s]+$/, '');
    } catch (ex) {}

    return newStr;
  },
  encodeTitle: function encodeTitle() {
    var title = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    var result = title.replace(/&(nbsp|amp|quot|lt|gt|apos);/g, '').replace(/<[^>]*>/g, '');
    var matcher = result.toLowerCase().substring(0, 500).trim().match(/([^\u0000-\u007F]|\w)+/g);

    if (matcher && matcher.length) {
      result = encodeURIComponent(matcher.join('_').replace(/\_+/, '_').replace(/_$/, ''));
    }

    return result;
  },
  decodeTitle: function decodeTitle() {
    var title = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    return decodeURIComponent(title).split('_').map(function (c) {
      return c.slice(0, 1).toUpperCase() + c.slice(1);
    }).join(' ');
  },
  encodeQuery: function encodeQuery() {
    var str = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    return encodeURIComponent(str.toLowerCase());
  },
  dictionary: function dictionary(code, type) {
    var retVal = '';

    switch (type) {
      case 'entity':
        switch (code) {
          case '100':
          case '101':
          case 'PROFILE':
            retVal = '/profile';
            break;

          case '109':
            retVal = '/publication';
            break;

          case '110':
          case 'PUBLISHER':
            retVal = '/publisher';
            break;

          case '111':
          case 'GRID':
            retVal = '/institution';
            break;

          case '161':
            retVal = '/group';
            break;

          case '163':
          case 'JOUR':
            retVal = '/journal';
            break;

          case '180':
            retVal = '/organisation';
            break;

          case '194':
            retVal = '/category';
            break;

          case '195':
          case 'FUNDING':
            retVal = '/funder';
            break;

          case '196':
          case 'COUNTRY':
            retVal = '/country';
            break;

          case '197':
            retVal = '/patent';
            break;

          case '198':
          case 'TERM':
            retVal = '/topic';
            break;

          case '199':
            retVal = '/author';
            break;

          case 'grant':
          case '200':
            retVal = '/grant';
            break;
        }

        break;

      case 'entTitle':
        switch (code) {
          case '100':
          case '101':
            retVal = 'profile';
            break;

          case '109':
            retVal = 'publication';
            break;

          case '110':
          case 'PUBLISHER':
            retVal = 'publisher';
            break;

          case '111':
          case 'GRID':
            retVal = 'institution';
            break;

          case '161':
            retVal = 'group';
            break;

          case '163':
          case 'JOUR':
            retVal = 'journal';
            break;

          case '194':
            retVal = 'category';
            break;

          case '195':
          case 'FUNDING':
            retVal = 'funders';
            break;

          case '196':
          case 'COUNTRY':
            retVal = 'country';
            break;

          case '197':
            retVal = 'patent';
            break;

          case '198':
          case 'TERM':
            retVal = 'topic';
            break;

          case '199':
            retVal = 'author';
            break;
        }

        break;

      case 'search':
        switch (code) {
          case 'pu':
            retVal = '/publications';
            break;

          case 'au':
            retVal = '/authors';
            break;

          case 'rg':
            retVal = '/groups';
            break;

          case 'pr':
            retVal = '/people';
            break;

          case 'jn':
            retVal = '/journals';
            break;

          default:
            retVal = '/all';
            break;
        }

        break;

      case 'searchCode':
        var searchCodes = wdom.searchCode();
        retVal = searchCodes[code];
        break;

      case 'icon':
        switch (code) {
          case '109':
            retVal = 'publication';
            break;

          case '110':
            retVal = 'publisher';
            break;

          case '111':
            retVal = 'institute';
            break;

          case '163':
            retVal = 'journal';
            break;

          case '194':
            retVal = 'category';
            break;

          case '195':
            retVal = 'dollar';
            break;

          case '196':
            retVal = 'globe';
            break;

          case '197':
            retVal = 'patent';
            break;

          case '198':
            retVal = 'topics';
            break;

          case '199':
            retVal = 'author';
            break;

          case 'grant':
            retVal = 'dollar';
        }

        break;
    }

    return retVal;
  },
  searchCode: function searchCode() {
    var invertCodes = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : false;
    var codes = {
      'journal': 'JOUR',
      'institute': 'GRID',
      'institution': 'GRID',
      'topic': 'TERM',
      'publisher': 'PUBLISHER',
      'funding': 'FUNDING',
      'funder': 'FUNDING',
      'country': 'COUNTRY',
      'publication': 'PUBLICATION'
    };
    return invertCodes ? Object.keys(codes).reduce(function (obj, key) {
      return obj[codes[key]] = key, obj;
    }, {}) : codes;
  },
  formatNumber: function formatNumber(num) {
    if (num != null || num !== 0) {
      var number = typeof num === 'string' ? parseFloat(num) : num;

      if (!isNaN(number)) {
        return wdom.format(number);
      }
    }

    return num;
  },
  leftPad: function leftPad(str, len, ch) {
    var cache = ['', ' ', '  ', '   ', '    ', '     ', '      ', '       ', '        ', '         ']; // convert `str` to a `string`

    str = str + ''; // `len` is the `pad`'s length now

    len = len - str.length; // doesn't need to pad

    if (len <= 0) return str; // `ch` defaults to `' '`

    if (!ch && ch !== 0) ch = ' '; // convert `ch` to a `string` cuz it could be a number

    ch = ch + ''; // cache common use cases

    if (ch === ' ' && len < 10) return cache[len] + str; // `pad` starts with an empty string

    var pad = ''; // loop

    while (true) {
      // add `ch` to `pad` if `len` is odd
      if (len & 1) pad += ch; // divide `len` by 2, ditch the remainder

      len >>= 1; // "double" the `ch` so this operation count grows logarithmically on `len`
      // each time `ch` is "doubled", the `len` would need to be "doubled" too
      // similar to finding a value in binary search tree, hence O(log(n))

      if (len) ch += ch; // `len` is 0, exit the loop
      else break;
    } // pad `str`!


    return pad + str;
  },
  generateLinkItem: function generateLinkItem(name, title, icon, url) {
    var target = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : '_self';
    var hide = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : false;
    var className = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : '';
    var onClick = arguments.length > 7 && arguments[7] !== undefined ? arguments[7] : wdom.noop;
    var host = arguments.length > 8 && arguments[8] !== undefined ? arguments[8] : undefined;
    return {
      name: name,
      title: title,
      icon: icon,
      url: url,
      target: target,
      hide: hide,
      className: className,
      onClick: onClick,
      host: host
    };
  },
  decodeEntities: function decodeEntities(str) {
    var element = document.createElement('div');

    if (str && typeof str === 'string') {
      str = str.replace(/<script[^>]*>([\S\s]*?)<\/script>/gmi, '');
      str = str.replace(/<\/?\w(?:[^"'>]|"[^"]*"|'[^']*')*>/gmi, '');
      element.innerHTML = str;
      str = element.textContent;
      element.textContent = '';
    }

    return str;
  },
  encodeBase64: function encodeBase64(content) {
    return window['Base64'].encode(content);
  },
  decodeBase64: function decodeBase64(content) {
    return window['Base64'].decode(content);
  },
  isFloat: function isFloat(num) {
    return num && !isNaN(num) && num / parseInt(num.toString(), 10) !== 1;
  },
  parseHex: function parseHex(str) {
    return parseInt(str, 16);
  },
  getBadgeColor: function getBadgeColor(key) {
    key = key.toUpperCase();
    var color = '';

    switch (key) {
      case 'JOUR':
      case 'JOURNAL':
        color = 'journal';
        break;

      case 'COUNTRY':
        color = 'country';
        break;

      case 'FUNDING':
      case 'FUNDER':
        color = 'funding';
        break;

      case 'GRANT TYPE':
        color = 'grants';
        break;

      case 'GRID':
      case 'INSTITUTE':
        color = 'institute';
        break;

      case 'KEYWORDS':
      case 'TERM':
        color = 'topics';
        break;

      case 'PUBLISHER':
        color = 'publisher';
        break;

      case 'AUTHOR':
      case 'PRINCIPAL INVESTIGATOR':
        color = 'author';
        break;

      case 'YEAR':
        color = 'year';
        break;

      case 'PROGRAM NAME':
      case 'PROJECTS':
        color = 'project';
        break;

      case 'CATEGORY':
        color = 'category';
        break;

      case 'ACCESS TYPE':
        color = 'type';
        break;
    }

    return color;
  },
  isIOS: function isIOS() {
    return /iPhone|iPad|iPod/i.test(navigator.userAgent);
  },
  isIpad: function isIpad() {
    return navigator.userAgent.match(/iPad/i) != null;
  },
  isMobile: function isMobile() {
    var nav = navigator.userAgent || navigator.vendor || window['opera'];
    var set1 = /(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows ce|xda|xiino|android|ipad|playbook|silk/i;
    var set2 = /1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-/i;

    if (wdom.isIpad()) {
      return false;
    }

    return set1.test(nav) || set2.test(nav.substr(0, 4));
  },
  copyToClipboard: function copyToClipboard(str) {
    var el = document.createElement('textarea'); // Create a <textarea> element

    el.value = str; // Set its value to the string that you want copied

    el.setAttribute('readonly', ''); // Make it readonly to be tamper-proof

    el.style.position = 'absolute';
    el.style.left = '-9999px'; // Move outside the screen to make it invisible

    document.body.appendChild(el); // Append the <textarea> element to the HTML document

    var selected = document.getSelection().rangeCount > 0 // Check if there is any content selected previously
    ? document.getSelection().getRangeAt(0) // Store selection if found
    : false; // Mark as false to know no selection existed before

    el.select(); // Select the <textarea> content

    document.execCommand('copy'); // Copy - only works as a result of a user action (e.g. click events)

    document.body.removeChild(el); // Remove the <textarea> element

    if (selected) {
      // If a selection existed before copying
      document.getSelection().removeAllRanges(); // Unselect everything on the HTML document

      document.getSelection().addRange(selected); // Restore the original selection
    }
  },

  /**
   * @author: Bilal Alam
   * @description: triggers wiz-dialog web components by setting name attribute
   * @param name {string} - name of the dialog to trigger
   * @param cb {Function} - callback function
   */
  showDialog: function showDialog(dialogName, params, cb) {
    try {
      var dialogElem = document.getElementsByTagName("wiz-dialog");

      if (dialogElem.length > 0) {
        dialogElem[0].setAttribute('name', dialogName);
        if (UserVars) dialogElem[0].setAttribute('user-vars', JSON.stringify(function (_ref) {
          var token = _ref.token,
              uId = _ref.uId;
          return _objectSpread({
            token: token,
            uId: uId
          }, params);
        }(UserVars)));
      } else {
        throw 'Failed to show dialog: wiz-dialog element not found inside DOM.';
      }

      cb && cb(dialogElem[0]);
    } catch (e) {
      console.error(e);
    }
  },

  /**
   * @author: Bilal Alam
   * @description: Attaches multiple attributes or events to a DOM element
   * @param el {object} - DOM element
   * @param propsDict {object} - key value pairs of multiple attributes or event handlers to be attached
   */
  attach: function attach(el, propsDict) {
    try {
      if (_typeof(propsDict) !== "object") throw "Invalid argument type: Expected 'object' recieved '".concat(_typeof(propsDict), "'");

      for (var key in propsDict) {
        if (typeof propsDict[key] === "string") el.setAttribute(key, propsDict[key]);else if (propsDict[key] instanceof Function) {
          this.eventHandlersCache = this.eventHandlersCache || {};
          if (this.eventHandlersCache[key]) el.removeEventListener(key, this.eventHandlersCache[key]);
          this.eventHandlersCache[key] = propsDict[key];
          el.addEventListener(key, propsDict[key]);
        }
      }
    } catch (e) {
      console.error(e);
    }
  }
};