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

exports.foldl = function(f) {
    return function(z) {
        return function(buf) {
            var r = z;
            var l = buf.length;
            for (var i = 0; i < l; ++i) {
                r = f(r)(buf[i]);
            }
            return r;
        };
    };
};

exports.foldr = function(f) {
    return function(z) {
        return function(buf) {
            var r = z;
            var l = buf.length;
            for (var i = l - 1; i >= 0; --i) {
                r = f(buf[i])(r);
            }
            return r;
        };
    };
};
