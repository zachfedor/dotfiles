// Generated by CoffeeScript 1.3.3
(function() {
  var activatedElement, ensureScrollChange, getDimension, isRendered, root, scrollProperties;

  window.Scroller = root = {};

  activatedElement = null;

  root.init = function() {
    return handlerStack.push({
      DOMActivate: function() {
        return activatedElement = event.target;
      }
    });
  };

  scrollProperties = {
    x: {
      axisName: 'scrollLeft',
      max: 'scrollWidth',
      viewSize: 'clientHeight'
    },
    y: {
      axisName: 'scrollTop',
      max: 'scrollHeight',
      viewSize: 'clientWidth'
    }
  };

  getDimension = function(el, direction, name) {
    if (name === 'viewSize' && el === document.body) {
      if (direction === 'x') {
        return window.innerWidth;
      } else {
        return window.innerHeight;
      }
    } else {
      return el[scrollProperties[direction][name]];
    }
  };

  ensureScrollChange = function(direction, changeFn) {
    var axisName, element, lastElement, oldScrollValue, rect;
    axisName = scrollProperties[direction].axisName;
    element = activatedElement;
    while (true) {
      oldScrollValue = element[axisName];
      changeFn(element, axisName);
      if (!(element[axisName] === oldScrollValue && element !== document.body)) {
        break;
      }
      lastElement = element;
      element = element.parentElement || document.body;
    }
    rect = activatedElement.getBoundingClientRect();
    if (rect.bottom < 0 || rect.top > window.innerHeight || rect.right < 0 || rect.left > window.innerWidth) {
      return activatedElement = element;
    }
  };

  root.scrollBy = function(direction, amount, factor) {
    if (factor == null) {
      factor = 1;
    }
    if (!document.body && amount instanceof Number) {
      if (direction === "x") {
        window.scrollBy(amount, 0);
      } else {
        window.scrollBy(0, amount);
      }
      return;
    }
    if (!activatedElement || !isRendered(activatedElement)) {
      activatedElement = document.body;
    }
    return ensureScrollChange(direction, function(element, axisName) {
      var elementAmount;
      if (Utils.isString(amount)) {
        elementAmount = getDimension(element, direction, amount);
      } else {
        elementAmount = amount;
      }
      elementAmount *= factor;
      return element[axisName] += elementAmount;
    });
  };

  root.scrollTo = function(direction, pos) {
    if (!document.body) {
      return;
    }
    if (!activatedElement || !isRendered(activatedElement)) {
      activatedElement = document.body;
    }
    return ensureScrollChange(direction, function(element, axisName) {
      var elementPos;
      if (Utils.isString(pos)) {
        elementPos = getDimension(element, direction, pos);
      } else {
        elementPos = pos;
      }
      return element[axisName] = elementPos;
    });
  };

  isRendered = function(element) {
    var computedStyle;
    computedStyle = window.getComputedStyle(element, null);
    return !(computedStyle.getPropertyValue("visibility") !== "visible" || computedStyle.getPropertyValue("display") === "none");
  };

}).call(this);