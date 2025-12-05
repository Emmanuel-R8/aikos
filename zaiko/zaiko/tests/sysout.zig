const std = @import("std");
const testing = std.testing;
const sysout = @import("../src/data/sysout.zig");

test "sysout validation" {
    var ifpage = sysout.IFPAGE{
        .keyval = sysout.SYSOUT_KEYVAL,
        .version = 1,
        .currentfxp = 0,
        .endofstack = 0,
    };
    
    try testing.expect(sysout.validateSysout(&ifpage) == true);
    
    // Test invalid keyval
    ifpage.keyval = 0;
    try testing.expect(sysout.validateSysout(&ifpage) == false);
}

test "sysout loading" {
    // TODO: Implement test cases for loading sysout files
    // This requires a test sysout file
    try testing.expect(true);
}