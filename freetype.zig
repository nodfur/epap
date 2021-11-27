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

  var harfbuzz = @ptrCast(*c.hb_font_t, c.hb_ft_font_create_referenced(freetype));

  c.hb_ft_font_set_funcs(harfbuzz);

  std.log.debug("harfbuzz: created font from freetype", .{});

  return Font{
    .freetype = freetype,
    .harfbuzz = harfbuzz,
  };
}

pub fn main() !void {
  try init();

  var font = try loadFont("fonts/cozette.otb", 13);

  var buffer: *c.hb_buffer_t =
    @ptrCast(*c.hb_buffer_t, c.hb_buffer_create());

  std.log.debug("harfbuzz: created buffer", .{});

  c.hb_buffer_set_direction(buffer, .HB_DIRECTION_LTR);
  c.hb_buffer_set_script(buffer, .HB_SCRIPT_LATIN);
  c.hb_buffer_set_language(buffer, c.hb_language_from_string("en", -1));
  c.hb_buffer_add_utf8(buffer, "Hello, World!", -1, 0, -1);

  std.log.debug("harfbuzz: added text", .{});

  c.hb_shape(font.harfbuzz, buffer, null, 0);

  std.log.debug("harfbuzz: shaped text", .{});

  var glyph_count: u32 = 0;

  var glyph_info =
    @ptrCast([*]c.hb_glyph_info_t, c.hb_buffer_get_glyph_infos(buffer, &glyph_count));

  std.log.debug("harfbuzz: got glyph info", .{});

  var glyph_pos =
    @ptrCast([*]c.hb_glyph_position_t, c.hb_buffer_get_glyph_positions(buffer, &glyph_count));

  std.log.debug("harfbuzz: got glyph positions", .{});

  var x: c.hb_position_t = 0;
  var y: c.hb_position_t = 0;

  var i: u32 = 0;
  while (i < glyph_count) : (i += 1) {
    var glyph_id = glyph_info[i].codepoint;
    var x_advance = glyph_pos[i].x_advance;
    var y_advance = glyph_pos[i].y_advance;
    var x_offset = glyph_pos[i].x_offset;
    var y_offset = glyph_pos[i].y_offset;

    std.log.debug("glyph {d} at {d}x{d}", .{
      .{glyph_id},
      .{x},
      .{y},
    });

    // render glyph
    var err = c.FT_Load_Glyph(font.freetype, glyph_id, c.FT_LOAD_MONOCHROME);
    if (err != 0) {
      std.log.err("freetype: {s}", .{c.FT_Error_String(err)});
      return error.freetype_error;
    }

    std.log.debug("freetype: loaded glyph format {d}", .{font.freetype.*.glyph.*.format});

    x += x_advance;
    y += y_advance;
  }

  try done();
}