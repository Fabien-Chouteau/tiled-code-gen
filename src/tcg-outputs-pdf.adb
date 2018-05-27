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

with Ada.Directories;
with Ada.Text_IO;

with PDF_Out; use PDF_Out;

with TCG.Palette;
with TCG.Tilesets;
with TCG.Tile_Layers;
with TCG.Utils;

package body TCG.Outputs.PDF is

   use type Palette.Color_Id;
   use type Palette.ARGB_Color;
   use type Tilesets.Master_Tile_Id;

   function Convert (C : Palette.ARGB_Color) return PDF_Out.Color_Type;

   procedure Draw_Square (Outfile : in out PDF_Out_File;
                          Rect    : Rectangle;
                          Color   : Color_Type);
   procedure Put_Palette_Info (Outfile         : in out PDF_Out_File;
                               Rect_Size       : Real := 60.0;
                               Colors_Per_Line : Natural := 3);

   procedure Draw_Tile (Outfile : in out PDF_Out_File;
                        Id      : Tilesets.Master_Tile_Id;
                        Rect    : Rectangle);
   procedure Put_Master_Tileset (Outfile        : in out PDF_Out_File;
                                 Tile_Size      : Real := 130.0;
                                 Tiles_Per_Line : Natural := 2);

   procedure Draw_Tile_Collision
     (Outfile : in out PDF_Out_File;
      Id      : Tilesets.Master_Tile_Id;
      Rect    : Rectangle);
   procedure Put_Master_Tileset_Collisions
     (Outfile        : in out PDF_Out_File;
      Tile_Size      : Real := 130.0;
      Tiles_Per_Line : Natural := 2);

   procedure Put_Map (Outfile : in out PDF_Out_File;
                      M       : Maps.Map);

   -------------
   -- Convert --
   -------------

   function Convert (C : Palette.ARGB_Color)
                     return PDF_Out.Color_Type
   is
      R : constant Real := Real (C.R) / Real (Palette.Component'Last);
      G : constant Real := Real (C.G) / Real (Palette.Component'Last);
      B : constant Real := Real (C.B) / Real (Palette.Component'Last);
   begin
      return (R, G, B);
   end Convert;

   -----------------
   -- Draw_Square --
   -----------------

   procedure Draw_Square (Outfile : in out PDF_Out_File;
                          Rect    : Rectangle;
                          Color   : PDF_Out.Color_Type)
   is
   begin
      PDF_Out.Color (Outfile, Color);
      Draw (Outfile, Rect, fill);
   end Draw_Square;

   ----------------------
   -- Put_Palette_Info --
   ----------------------

   procedure Put_Palette_Info (Outfile         : in out PDF_Out_File;
                               Rect_Size       : Real := 60.0;
                               Colors_Per_Line : Natural := 3)
   is
      Layout : constant Rectangle := PDF_Out.Layout (Outfile);

      Top_Margin : constant Real := 100.0;
      Left_Margin : constant Real := Rect_Size;

      Spacing  : constant Real :=
        (Layout.width - Left_Margin) / Real (Colors_Per_Line);

      X : Real := Layout.x_min + Left_Margin;
      Y : Real := Layout.y_min + Layout.height - Rect_Size - Top_Margin;

      Cnt : Natural := 1;
      C : Palette.ARGB_Color;
   begin
      Font_Size (Outfile, 40.0);
      Put_Line (Outfile, "Color Palette");
      Font_Size (Outfile, 11.0);

      for Id in Palette.First_Id .. Palette.Last_Id loop

         C := Palette.Convert (Id);

         Draw_Square (Outfile, (X, Y, Rect_Size, Rect_Size), Convert (C));

         Color (Outfile, PDF_Out.black);

         Put_XY (Outfile,
                 X + Rect_Size * 1.3,
                 Y + Rect_Size * 0.8,
                 "Color #" & Id'Img);
         New_Line (Outfile);
         Put_Line (Outfile, "R:" & C.R'Img);
         Put_Line (Outfile, "G:" & C.G'Img);
         Put_Line (Outfile, "B:" & C.B'Img);

         if Palette.Transparent = Id then
            Put_Line (Outfile, "Transparent");
         end if;

         if Cnt mod Colors_Per_Line = 0 then
            Y := Y - (Rect_Size + 20.0);
            X := Layout.x_min + Left_Margin;

            if Y <= Top_Margin then
               New_Page (Outfile);
               Y := Layout.y_min + Layout.height - Rect_Size - Top_Margin;
            end if;
         else
            X := X + Spacing;
         end if;
         Cnt := Cnt + 1;
      end loop;
   end Put_Palette_Info;

   ---------------
   -- Draw_Tile --
   ---------------

   procedure Draw_Tile (Outfile : in out PDF_Out_File;
                        Id      : Tilesets.Master_Tile_Id;
                        Rect    : Rectangle)
   is
      Pix_W : constant Real := Rect.width / Real (Tilesets.Tile_Width);
      Pix_H : constant Real := Rect.height / Real (Tilesets.Tile_Height);

      C : Palette.ARGB_Color;
   begin

      for PX in 1 .. Tilesets.Tile_Width loop
         for PY in 1 .. Tilesets.Tile_Height loop
            C := Tilesets.Pix (Id, PX, (Tilesets.Tile_Height - PY + 1));
            if C /= Palette.Transparent then
               Draw_Square (Outfile,
                            (Rect.x_min + Real (PX - 1) * Pix_W,
                             Rect.y_min + Real (PY - 1) * Pix_H,
                             Pix_W, Pix_H),
                            Convert (C));
            end if;
         end loop;
      end loop;
   end Draw_Tile;

   ------------------------
   -- Put_Master_Tileset --
   ------------------------

   procedure Put_Master_Tileset (Outfile        : in out PDF_Out_File;
                                 Tile_Size      : Real := 130.0;
                                 Tiles_Per_Line : Natural := 2)
   is
      Layout : constant Rectangle := PDF_Out.Layout (Outfile);

      Top_Margin : constant Real := 100.0;
      Left_Margin : constant Real := 50.0;

      Spacing  : constant Real :=
        (Layout.width - Left_Margin) / Real (Tiles_Per_Line);

      X : Real := Layout.x_min + Left_Margin;
      Y : Real := Layout.y_min + Layout.height - Tile_Size - Top_Margin;

      Cnt : Natural := 1;
   begin
      Font_Size (Outfile, 40.0);
      Put_Line (Outfile, "Tileset");
      Font_Size (Outfile, 12.0);

      for Id in Tilesets.First_Id .. Tilesets.Last_Id loop

         if Id /= Tilesets.No_Tile then
            Draw_Tile (Outfile, Id, (X, Y, Tile_Size, Tile_Size));
         end if;

         Color (Outfile, PDF_Out.black);
         Draw (Outfile, (X, Y, Tile_Size, Tile_Size), stroke);

         Put_XY (Outfile,
                 X + Tile_Size * 1.1,
                 Y + Tile_Size * 0.8,
                 "Tile #" & Id'Img);
         New_Line (Outfile);
         if Id = Tilesets.No_Tile then
            Put_Line (Outfile, "No tile");
         end if;
         New_Line (Outfile);

         if Cnt mod Tiles_Per_Line = 0 then
            Y := Y - (Tile_Size + 20.0);
            X := Layout.x_min + Left_Margin;

            if Y <= Top_Margin then
               New_Page (Outfile);
               Y := Layout.y_min + Layout.height - Tile_Size - Top_Margin;
            end if;
         else
            X := X + Spacing;
         end if;
         Cnt := Cnt + 1;
      end loop;
   end Put_Master_Tileset;

   -------------------------
   -- Draw_Tile_Collision --
   -------------------------

   procedure Draw_Tile_Collision (Outfile : in out PDF_Out_File;
                                  Id      : Tilesets.Master_Tile_Id;
                                  Rect    : Rectangle)
   is
      Pix_W : constant Real := Rect.width / Real (Tilesets.Tile_Width);
      Pix_H : constant Real := Rect.height / Real (Tilesets.Tile_Height);
   begin

      for PX in 1 .. Tilesets.Tile_Width loop
         for PY in 1 .. Tilesets.Tile_Height loop
            if Tilesets.Collision (Id, PX, (Tilesets.Tile_Height - PY + 1))
            then
               Draw_Square (Outfile,
                            (Rect.x_min + Real (PX - 1) * Pix_W,
                             Rect.y_min + Real (PY - 1) * Pix_H,
                             Pix_W, Pix_H),
                            (1.0, 0.0, 0.0));
            end if;
         end loop;
      end loop;
   end Draw_Tile_Collision;

   -----------------------------------
   -- Put_Master_Tileset_Collisions --
   -----------------------------------

   procedure Put_Master_Tileset_Collisions
     (Outfile        : in out PDF_Out_File;
      Tile_Size      : Real := 130.0;
      Tiles_Per_Line : Natural := 2)
   is
      Layout : constant Rectangle := PDF_Out.Layout (Outfile);

      Top_Margin : constant Real := 100.0;
      Left_Margin : constant Real := 50.0;

      Spacing  : constant Real :=
        (Layout.width - Left_Margin) / Real (Tiles_Per_Line);

      X : Real := Layout.x_min + Left_Margin;
      Y : Real := Layout.y_min + Layout.height - Tile_Size - Top_Margin;

      Cnt : Natural := 1;
   begin
      Font_Size (Outfile, 40.0);
      Put_Line (Outfile, "Tileset collisions");
      Font_Size (Outfile, 12.0);

      for Id in Tilesets.First_Id .. Tilesets.Last_Id loop
         if Id /= Tilesets.No_Tile then
            Draw_Tile_Collision (Outfile, Id, (X, Y, Tile_Size, Tile_Size));
         end if;

         Color (Outfile, PDF_Out.black);
         Draw (Outfile, (X, Y, Tile_Size, Tile_Size), stroke);

         Put_XY (Outfile,
                 X + Tile_Size * 1.1,
                 Y + Tile_Size * 0.8,
                 "Tile #" & Id'Img);
         New_Line (Outfile);
         if Id = Tilesets.No_Tile then
            Put_Line (Outfile, "No tile");
         end if;
         New_Line (Outfile);

         if Cnt mod Tiles_Per_Line = 0 then
            Y := Y - (Tile_Size + 20.0);
            X := Layout.x_min + Left_Margin;

            if Y <= Top_Margin then
               New_Page (Outfile);
               Y := Layout.y_min + Layout.height - Tile_Size - Top_Margin;
            end if;
         else
            X := X + Spacing;
         end if;
         Cnt := Cnt + 1;
      end loop;
   end Put_Master_Tileset_Collisions;

   -------------
   -- Put_Map --
   -------------

   procedure Put_Map (Outfile : in out PDF_Out_File;
                      M       : Maps.Map)
   is
      Tile_Width  : constant Natural := Maps.Tile_Width (M);
      Tile_Height : constant Natural := Maps.Tile_Height (M);
      Width       : constant Natural := Maps.Width (M) * Tile_Width;
      Height      : constant Natural := Maps.Height (M) * Tile_Height;

      Layout      : constant Rectangle := PDF_Out.Layout (Outfile);
      Pix_W       : constant Real := Layout.width / Real (Width);
      Pix_H       : constant Real := Layout.height / Real (Height);
      Pix_Size    : constant Real := Real'Min (Pix_W, Pix_H);

      C    : Palette.ARGB_Color;
      T_Id : Tilesets.Master_Tile_Id;
   begin
      New_Page (Outfile);

      for Y in reverse 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            C := Palette.Transparent;
            for L in reverse Maps.First_Layer (M) .. Maps.Last_Layer (M)
            loop
               T_Id := Maps.Master_Tile (M,
                                         Tile_Layers.Tile (Maps.Layer (M, L),
                                           1 + X / Tile_Width,
                                           1 + Y / Tile_Height));

               if T_Id /= Tilesets.No_Tile then
                  C := Tilesets.Pix (T_Id,
                                     1 + X mod Tile_Width,
                                     1 + Y mod Tile_Height);
               end if;

               if C /= Palette.Transparent then
                  Draw_Square (Outfile,
                               (Layout.x_min + Real (X) * Pix_Size,
                                Layout.y_min + Real (Width - 1 - Y) * Pix_Size,
                                Pix_Size, Pix_Size),
                               Convert (C));
                  exit;
               end if;
            end loop;
         end loop;
      end loop;

      Color (Outfile, PDF_Out.black);
      Font_Size (Outfile, 20.0);
      Put_XY (Outfile,
              50.0,
              Layout.y_min + Layout.height - 30.0,
              Maps.Name (M));
      New_Line (Outfile);
      Font_Size (Outfile, 12.0);

      for L in reverse Maps.First_Layer (M) .. Maps.Last_Layer (M) loop
         Put_Line (Outfile, " - " & Tile_Layers.Name (Maps.Layer (M, L)));
      end loop;
   end Put_Map;

   -----------------
   -- Gen_PDF_Doc --
   -----------------

   procedure Gen_PDF_Doc (Directory : String;
                          Filename  : String;
                          Map_List  : TCG.Maps.List.List)
   is
      Outfile : PDF_Out_File;
   begin
      if not TCG.Utils.Make_Dir (Directory) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Cannot create directory for documentation: '" & Directory & "'");
         return;
      end if;

      Create (Outfile, Ada.Directories.Compose (Directory, Filename));
      Title (Outfile, "");
      Subject (Outfile, "");
      Keywords (Outfile, "Tiled, maps, sprites, palette");
      Creator_Application (Outfile, "Tiled_Code_Gen");

      Page_Setup (Outfile, A4_portrait);

      Put_Palette_Info (Outfile);

      New_Page (Outfile);
      Put_Master_Tileset (Outfile,
                          Tile_Size      => 50.0,
                          Tiles_Per_Line => 5);

      New_Page (Outfile);
      Put_Master_Tileset_Collisions (Outfile,
                                     Tile_Size      => 50.0,
                                     Tiles_Per_Line => 5);

      for M of Map_List loop
         Put_Map (Outfile, M);
      end loop;

      Close (Outfile);
   end Gen_PDF_Doc;

end TCG.Outputs.PDF;
