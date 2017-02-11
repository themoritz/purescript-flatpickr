"use strict";

exports.runPredicateImpl = function (pred) {
  return function(date) {
    return pred(date)();
  };
};

exports.flatpickrImpl = function (el, config) {
  return function () {
    return new Flatpickr(el, config);
  };
};

exports.changeMonthImpl = function (monthNum, isOffset, self) {
  return function () {
    return self.changeMonth(monthNum, isOffset);
  };
};

exports.clear = function (self) {
  return function () {
    return self.clear();
  };
};

exports.close = function (self) {
  return function () {
    return self.close();
  };
};

exports.destroy = function (self) {
  return function () {
    return self.destroy();
  };
};

exports.formatDateImpl = function (formatStr, dateObj, self) {
  return function () {
    return self.formatDate(formatStr, dateObj);
  };
};

exports.jumpToDateImpl = function (date, self) {
  return function () {
    return self.jumpToDate(date);
  };
};

exports.open = function (self) {
  return function () {
    return self.open();
  };
};

exports.parseDateImpl = function (date, self) {
  return function () {
    return self.parseDate(date);
  };
};

exports.redraw = function (self) {
  return function () {
    return self.redraw();
  };
};

exports.setImpl = function (option, value, self) {
  return function () {
    return self.set(option, value);
  };
};

exports.setDateImpl = function (date, triggerChange, self) {
  return function () {
    return self.setDate(date, triggerChange);
  };
};

exports.toggle = function (self) {
  return function () {
    return self.toggle();
  };
};

exports.runHookImpl = function (cb) {
  return function(selectedDates, dateStr, instance) {
    return cb(selectedDates)(dateStr)(instance)();
  };
};

exports.hookImpl = function (name, self, cb) {
  return function () {
    return self["config"][name] = [
      exports.runHookImpl(cb)
    ];
  };
};
