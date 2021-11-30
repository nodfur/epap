const freetype = @import("./freetype.zig");

pub fn main() !void {
  try freetype.demo();
}