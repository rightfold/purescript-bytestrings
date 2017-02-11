'use strict';

var Data_Maybe = require('../Data.Maybe');

exports.unsafeIndex = function(buffer) {
    return function(offset) {
        return buffer[offset];
    };
};

exports.realGetAtOffset = function(offset) {
    return function(buffer) {
        return function() {
            var octet = buffer[offset];
            return octet === undefined
            ? Data_Maybe.Nothing.value
            : new Data_Maybe.Just(octet);
        };
    };
};