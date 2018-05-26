------------------------------------------------------------------------------
--                                                                          --
--                             tiled-code-gen                               --
--                                                                          --
--                    Copyright (C) 2018 Fabien Chouteau                    --
--                                                                          --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;  use Ada.Text_IO.Text_Streams;

with Interfaces; use Interfaces;

with TCG.Palette;  use TCG.Palette;
with TCG.Tile_Layers;  use TCG.Tile_Layers;
with TCG.Tilesets; use TCG.Tilesets;

package body TCG.Maps.Render is

   type Unsigned_8_Array is array (Integer range <>) of Unsigned_8;

   type Header (As_Array : Boolean := True) is record
      case As_Array is
         when True =>
            Arr : Unsigned_8_Array (1 .. 14);
         when False =>
            Signature : Integer_16;
            Size      : Integer_32; --  File size
            Reserved1 : Integer_16;
            Reserved2 : Integer_16;
            Offset    : Integer_32; --  Data offset
      end case;
   end record with Unchecked_Union, Pack, Size => 14 * 8;

   type Info (As_Array : Boolean := True) is record
      case As_Array is
         when True =>
            Arr : Unsigned_8_Array (1 .. 40);
         when False =>
            Struct_Size   : Integer_32;
            Width         : Integer_32; -- Image width in pixels
            Height        : Integer_32; -- Image hieght in pixels
            Planes        : Integer_16;
            Pixel_Size    : Integer_16; -- Bits per pixel
            Compression   : Integer_32; -- Zero means no compression
            Image_Size    : Integer_32; -- Size of the image data in UInt8s
            PPMX          : Integer_32; -- Pixels per meter in x led
            PPMY          : Integer_32; -- Pixels per meter in y led
            Palette_Size  : Integer_32; -- Number of colors
            Important     : Integer_32;
      end case;
   end record with Unchecked_Union, Pack, Size => 40 * 8;

   ------------
   -- To_BMP --
   ------------

   procedure To_BMP
     (M          : Map;
      Path       : String;
      Background : ARGB_Color)
   is
      Output : File_Type;

      Tile_Width  : constant Natural := Maps.Tile_Width (M);
      Tile_Height : constant Natural := Maps.Tile_Height (M);

      Width  : constant Natural := Maps.Width (M) * Maps.Tile_Width (M);
      Height : constant Natural := Maps.Height (M) * Maps.Tile_Height (M);

      Hdr    : Header;
      Inf    : Info;
      Row_Size    : constant Integer_32 := Integer_32 (Width * 24);
      Row_Padding : constant Integer_32 := (32 - (Row_Size mod 32)) mod 32 / 8;
      Data_Size   : constant Integer_32 :=
        (Row_Size + Row_Padding) * Integer_32 (Height);

      RGB_Pix : ARGB_Color;
      Pix_Out : Unsigned_8_Array (1 .. 3);
      Padding : constant Unsigned_8_Array (1 .. Integer (Row_Padding)) :=
        (others => 0);

      T_Id : Tilesets.Master_Tile_Id;
   begin

      Create (File => Output, Mode => Out_File, Name => Path);

      if not Is_Open (Output) then
         raise Program_Error with "Cannot create file: " & Path;
      end if;

      Hdr.Signature := 16#4D42#;
      Hdr.Size      := (Data_Size + 54) / 4;
      Hdr.Offset    := 54;

      Inf.Struct_Size := 40;
      Inf.Width := Integer_32 (Width);
      Inf.Height := Integer_32 (Height);
      Inf.Planes := 1;
      Inf.Pixel_Size := 24;
      Inf.Compression := 0;
      Inf.Image_Size := Data_Size / 4;
      Inf.PPMX := 2835;
      Inf.PPMY := 2835;
      Inf.Palette_Size := 0;
      Inf.Important := 0;

      Unsigned_8_Array'Write (Stream (Output), Hdr.Arr);
      Unsigned_8_Array'Write (Stream (Output), Inf.Arr);

      for Y in reverse 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            RGB_Pix := Transparent;
            for L in reverse 0 .. Number_Of_Layers (M) - 1 loop
               T_Id := Master_Tile (M,
                                    Tile (Layer (M, L),
                                      1 + X / Tile_Width,
                                      1 + Y / Tile_Height));

               if T_Id /= No_Tile then
                  RGB_Pix := Tilesets.Pix (T_Id,
                                           1 + X mod Tile_Width,
                                           1 + Y mod Tile_Height);
               end if;

               exit when RGB_Pix /= Palette.Transparent;
            end loop;

            if RGB_Pix = Transparent then
               RGB_Pix := Background;
            end if;

            Pix_Out (1) := RGB_Pix.B;
            Pix_Out (2) := RGB_Pix.G;
            Pix_Out (3) := RGB_Pix.R;

            Unsigned_8_Array'Write (Stream (Output), Pix_Out);
         end loop;
      end loop;

      Unsigned_8_Array'Write (Stream (Output), Padding);
      Close (Output);
   end To_BMP;

end TCG.Maps.Render;
