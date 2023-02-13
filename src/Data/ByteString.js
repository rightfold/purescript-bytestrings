export function unsafeIndex (buffer) {
    return function(offset) {
        return buffer[offset];
    };
};

export function realGetAtOffset (Nothing) {
    return function(Just) {
        return function(offset) {
            return function(buffer) {
                return function() {
                    var octet = buffer[offset];
                    return octet === undefined
                        ? Nothing
                        : Just(octet);
                };
            };
        };
    };
};

export function foldl (f) {
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

export function foldr (f) {
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
