const std = @import("std");
const _ = @cImport(@cInclude("ft2build.h"));
const c = @cImport({
  @cInclude("freetype/freetype.h");
  @cInclude("hb-ft.h");
});

var library: c.FT_Library = null;

pub fn init() !void {
  var err = c.FT_Init_FreeType(&library);

  if (err != 0) {
    return error.freetype_error;
  }

  std.log.debug("freetype: initialized library", .{});
}

pub fn done() !void {
  var err = c.FT_Done_FreeType(library);

  if (err != 0) {
    return error.freetype_error;
  }

  std.log.debug("freetype: closed library", .{});
}

pub fn loadFreetypeFace(path: [*:0]const u8) !c.FT_Face {
  var face: c.FT_Face = null;
  var err = c.FT_New_Face(library, path, 0, &face);

  if (err != 0) {
    std.log.err("freetype: {s}", .{c.FT_Error_String(err)});
    return error.freetype_error;
  }

  std.log.debug("freetype: loaded {s}", .{path});

  return face;
}

pub fn setPixelSizes(face: c.FT_Face, height: u32) !void {
  var err = c.FT_Set_Pixel_Sizes(face, 0, height);

  if (err != 0) {
    std.log.err("freetype: {s}", .{c.FT_Error_String(err)});
    return error.freetype_error;
  }

  std.log.debug("freetype: set pixel size to {d}", .{height});
}

const Font = struct {
  freetype: c.FT_Face,
  harfbuzz: *c.hb_font_t,
};

pub fn loadFont(path: [*:0]const u8, height: u32) !Font {
  var freetype = try loadFreetypeFace(path);
  try setPixelSizes(freetype, height);

  var harfbuzz = c.hb_ft_font_create_referenced(freetype);
  if (harfbuzz == null) {
    std.log.err("harfbuzz: failed to create harfbuzz font", .{});
    return error.harfbuzz_error;
  }

  std.log.debug("harfbuzz: created font from freetype", .{});

  return Font{
    .freetype = freetype,
    .harfbuzz = @ptrCast(*c.hb_font_t, harfbuzz),
  };
}

pub fn main() !void {
  try init();

  var font = try loadFont("fonts/cozette.otb", 13);

  try done();
}