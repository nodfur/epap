const std = @import("std");
const _ = @cImport(@cInclude("ft2build.h"));
const ft = @cImport(@cInclude("freetype/freetype.h"));

var library: ft.FT_Library = null;

pub fn init() !void {
  var err = ft.FT_Init_FreeType(&library);

  if (err != 0) {
    return error.freetype_error;
  }

  std.log.debug("freetype: initialized library", .{});
}

pub fn done() !void {
  var err = ft.FT_Done_FreeType(library);

  if (err != 0) {
    return error.freetype_error;
  }

  std.log.debug("freetype: closed library", .{});
}

pub fn loadFace(path: [*:0]const u8) !ft.FT_Face {
  var face: ft.FT_Face = null;
  var err = ft.FT_New_Face(library, path, 0, &face);

  if (err != 0) {
    std.log.err("freetype: {s}", .{ft.FT_Error_String(err)});
    return error.freetype_error;
  }

  std.log.debug("freetype: loaded {s}", .{path});

  return face;
}

pub fn setPixelSizes(face: ft.FT_Face, height: u32) !void {
  var err = ft.FT_Set_Pixel_Sizes(face, 0, height);

  if (err != 0) {
    std.log.err("freetype: {s}", .{ft.FT_Error_String(err)});
    return error.freetype_error;
  }

  std.log.debug("freetype: set pixel size to {d}", .{height});
}

pub fn main() !void {
  try init();

  var face = try loadFace("fonts/cozette.otb");
  try setPixelSizes(face, 13);

  try done();
}