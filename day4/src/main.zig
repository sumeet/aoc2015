const std = @import("std");
const Md5 = std.crypto.hash.Md5;
const print = std.debug.print;
const bufPrint = std.fmt.bufPrint;
const string = @cImport({
    @cInclude("string.h");
});

pub fn main() !void {
    var sum = md5("hello");
    var hash_buffer: [1000000]u8 = undefined;
    var hexdigest: [32]u8 = undefined;
    var i: usize = 1;
    while (true) : (i += 1) {
        var this_str_to_hash = try bufPrint(&hash_buffer, "{s}{d}", .{ input, i });
        var digest = md5(this_str_to_hash);
        _ = try bufPrint(&hexdigest, "{x}", .{digest});
        if (string.memcmp(&hexdigest, "00000", 5) == 0) {
            print("{}\n", .{i});
            break;
        }
    }
}

fn md5(s: []const u8) [16]u8 {
    var h: [16]u8 = undefined;
    Md5.hash(s, &h, .{});
    return h;
}

const sample = "abcdef";
const input = "ckczppom";
