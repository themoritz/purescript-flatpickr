"use strict";

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

// jumpToDate

exports.open = function (self) {
  return function () {
    return self.open();
  };
};

exports.parseDate = function (date, self) {
  return function () {
    return self.parseDate(date);
  };
};

exports.redraw = function (self) {
  return function () {
    return self.redraw();
  };
};

// set

// setDate

exports.toggle = function (self) {
  return function () {
    return self.toggle();
  };
};

// Hooks

exports.hookImpl = function (name, self, cb) {
  return function () {
    return self["config"][name] = [
      function (selectedDates, dateStr, instance) {
        return cb(selectedDates)(dateStr)(instance)();
      }
    ];
  };
};
