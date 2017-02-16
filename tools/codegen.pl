#!/usr/bin/env perl
use strict;
use warnings;

local $" = ', ';

sub shifts {
    my ($f, $n) = @_;
    map { "(i `$f` $_) .&. 0xFF" }
    grep { $_ % 8 == 0 }
    0 .. $n - 1;
}

sub int {
    my ($s, $e, $n) = @_;
    my $endian = {b => 'big', l => 'little'}->{$e};
    my $shift = {s => 'shr', u => 'zshr'}->{$s};
    $s = '' if $s eq 's';
    my @bs = shifts($shift, $n);
    @bs = reverse @bs if $e eq 'b';
    print "\n";
    print "-- | Encode an integer of $n bits in $endian-endian encoding.\n";
    if ($n < 32) {
        print "-- | Extra bits will be discarded.\n";
    }
    print "${s}int${n}${e}e :: Int -> ByteString\n";
    print "${s}int${n}${e}e i = pack [@bs]\n";
}

sub intbe  { main::int('s', 'b', shift) }
sub intle  { main::int('s', 'l', shift) }
sub uintbe { main::int('u', 'b', shift) }
sub uintle { main::int('u', 'l', shift) }

intbe  $_ for 8, 16, 32;
intle  $_ for 8, 16, 32;
uintbe $_ for 8, 16;
uintle $_ for 8, 16;
