------------------------------------------------------------------------------
--                                                                          --
--                             tiled-code-gen                               --
--                                                                          --
--                    Copyright (C) 2020 Fabien Chouteau                    --
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

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with TCG.Utils; use TCG.Utils;
with TCG.Tilesets;
use TCG;

package body TCG.Outputs.RSTE is

   use type Tilesets.Master_Tile_Id;
   use type Palette.Color_Id;

   procedure Generate_Tileset (Filepath     : String;
                               Package_Name : String);

   procedure Generate_Tileset_Collisions (Filepath : String);

   procedure Generate_Root_Module
     (Filename : String;
      Format   : Palette.Output_Color_Format;
      Map_List : TCG.Maps.List.List);

   ----------------------
   -- Generate_Tileset --
   ----------------------

   procedure Generate_Tileset (Filepath     : String;
                               Package_Name : String)
   is
      pragma Unreferenced (Package_Name);
      Output : Ada.Text_IO.File_Type;

      procedure P (Str : String);
      procedure PL (Str : String);
      procedure NL;

      procedure P (Str : String) is
      begin
         Put (Output, Str);
      end P;

      procedure PL (Str : String) is
      begin
         Put_Line (Output, Str);
      end PL;

      procedure NL is
      begin
         New_Line (Output);
      end NL;
   begin

      Create (Output, Out_File, Filepath);
      PL ("pub type ColorId = usize;");
      PL ("pub type Tile = [ColorId; super::TILE_SIZE * super::TILE_SIZE];");
      PL ("pub static TILE_SET : [Tile; " &
            Natural'Image (Tilesets.Number_Of_Tiles + 1)
          & "] = ");
      PL ("     [");

      --  The RSTE tile set has a special 0 tile that use for empty tile
      P ("            [");
      for X in 1 .. Tilesets.Tile_Width loop
         for Y in 1 .. Tilesets.Tile_Height loop
            P (" 0,");
         end loop;
         NL;
         P ("             ");
      end loop;
      PL ("],");

      for Id in Tilesets.First_Id .. Tilesets.Last_Id loop
         if Id /= Tilesets.No_Tile then
            P ("            [");
            for Y in 1 .. Tilesets.Tile_Height loop

               if Y /= 1 then
                  P ("             ");
               end if;

               for X in 1 .. Tilesets.Tile_Width loop
                  P (Palette.Color_Id'Image ((Tilesets.Pix (Id, X, Y))));
                  if X /= Tilesets.Tile_Height then
                     P (",");
                  end if;
               end loop;

               if Y /= Tilesets.Tile_Width then
                  PL (",");
               else
                  P ("]");
               end if;
            end loop;

            if Id /= Tilesets.Last_Id then
               PL (",");
            else
               PL ("];");
            end if;
         end if;
      end loop;

      Close (Output);
   end Generate_Tileset;

   ---------------------------------
   -- Generate_Tileset_Collisions --
   ---------------------------------

   procedure Generate_Tileset_Collisions (Filepath : String) is
      Output : Ada.Text_IO.File_Type;

      procedure P (Str : String);
      procedure PL (Str : String);
      procedure NL;

      procedure P (Str : String) is
      begin
         Put (Output, Str);
      end P;

      procedure PL (Str : String) is
      begin
         Put_Line (Output, Str);
      end PL;

      procedure NL is
      begin
         New_Line (Output);
      end NL;
   begin
      Create (Output, Out_File, Filepath);
      PL ("pub type Tile = [bool; super::TILE_SIZE * super::TILE_SIZE];");
      PL ("pub static TILE_SET : [Tile; " &
            Natural'Image (Tilesets.Number_Of_Tiles + 1)
          & "] = ");
      PL ("     [");

      --  The RSTE tile set has a special 0 tile that use for empty tile
      P ("            [");
      for X in 1 .. Tilesets.Tile_Width loop
         for Y in 1 .. Tilesets.Tile_Height loop
            P ("false,");
         end loop;
         NL;
         P ("             ");
      end loop;
      PL ("],");

      for Id in Tilesets.First_Id .. Tilesets.Last_Id loop
         if Id /= Tilesets.No_Tile then
            P ("            [");
            for Y in 1 .. Tilesets.Tile_Height loop

               if Y /= 1 then
                  P ("             ");
               end if;

               for X in 1 .. Tilesets.Tile_Width loop
                  P (if Tilesets.Collision (Id, X, Y) then
                        "true"
                     else
                        "false");
                  if X /= Tilesets.Tile_Height then
                     P (",");
                  end if;
               end loop;

               if Y /= Tilesets.Tile_Width then
                  PL (",");
               else
                  P ("]");
               end if;
            end loop;

            if Id /= Tilesets.Last_Id then
               PL (",");
            else
               PL ("];");
            end if;
         end if;
      end loop;

      Close (Output);
   end Generate_Tileset_Collisions;

   --------------------------
   -- Generate_Root_Module --
   --------------------------

   procedure Generate_Root_Module
     (Filename : String;
      Format   : Palette.Output_Color_Format;
      Map_List : TCG.Maps.List.List)
   is
      use TCG.Palette;

      Output : File_Type;
   begin
      Create (Output, Out_File, Filename);

      Put_Line (Output, "pub mod tileset;");
      Put_Line (Output, "pub mod tileset_collisions;");

      for Map of Map_List loop
         Put_Line (Output,
                   "pub mod " & To_Rust_Identifier (Maps.Name (Map)) & ";");
      end loop;

      New_Line (Output);
      case Format is
         when Palette.ARGB =>
            Put_Line (Output, "pub struct OutputColor {");
            Put_Line (Output, "   a : u8,");
            Put_Line (Output, "   r : u8,");
            Put_Line (Output, "   g : u8,");
            Put_Line (Output, "   b : u8,");
            Put_Line (Output, "};");
         when Palette.RGB565 | Palette.RGB565_Swap | Palette.RGB555 =>
            Put_Line (Output, "pub type OutputColor = u16;");
         when Palette.RGB888 =>
            Put_Line (Output, "pub type OutputColor = u32;");
      end case;

      New_Line (Output);
      Put_Line (Output, "pub const TRANSPARENT : OutputColor = " &
                  Palette.Image (Palette.Transparent, Format) & ";");

      New_Line (Output);
      Put_Line (Output, "pub const TILE_SIZE : usize = " &
                  Tilesets.Tile_Width'Img & ";");

      Put_Line (Output, "pub const NO_TILE : usize = 0;");

      New_Line (Output);

      Put_Line (Output, "pub static PALETTE : [u32; " &
                  Color_Id'Image (Palette.Last_Id - Palette.First_Id + 1) &
                  "] = [");

      for Id in Palette.First_Id .. Palette.Last_Id loop
         Put (Output, "    " & Palette.Image (Palette.Convert (Id), Format));

         if Id /= Palette.Last_Id then
            Put_Line (Output, ",");
         end if;
      end loop;
      Put_Line (Output, "];");

      New_Line (Output);
      Put_Line (Output, "pub enum ObjectKind {Rectangle, Point,");
      Put_Line (Output, "     Ellipse, Polygon, Tile, Text}");
      New_Line (Output);
      Put_Line (Output, "pub struct Object {");
      Put_Line (Output, "    kind           : ObjectKind,");
      Put_Line (Output, "    name           : &'static str,");
      Put_Line (Output, "    id             : u32,");
      Put_Line (Output, "    x              : f32,");
      Put_Line (Output, "    y              : f32,");
      Put_Line (Output, "    width          : f32,");
      Put_Line (Output, "    height         : f32,");
      Put_Line (Output, "    str            : &'static str,");
      Put_Line (Output, "    flip_vertical  : bool,");
      Put_Line (Output, "    flip_horizontal: bool,");
      Put_Line (Output, "    tile_id        : usize,");
      Put_Line (Output, "}");
      New_Line (Output);
      Put_Line (Output, "pub type ObjectArray = [Object];");
      New_Line (Output);
      Close (Output);
   end Generate_Root_Module;

   -----------------
   -- Gen_PDF_Doc --
   -----------------

   procedure Gen_RSTE_Source
     (Directory        : String;
      Root_Module_Name : String;
      Format           : Palette.Output_Color_Format;
      Map_List         : TCG.Maps.List.List)
   is
      Module_Dir : constant String := Compose (Directory, Root_Module_Name);
   begin
      if not TCG.Utils.Make_Dir (Directory) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Cannot create directory for RSTE code: '" & Directory & "'");
         return;
      end if;

      if not TCG.Utils.Make_Dir (Module_Dir) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Cannot create directory for RSTE code: '" & Module_Dir & "'");
         return;
      end if;

      if Tilesets.Tile_Width /= Tilesets.Tile_Height then
         raise Program_Error with "Tiles are not square";
      end if;

      declare
         Module_Name : constant String := "mod";
         Filename     : constant String :=
           Compose (Module_Dir, To_Rust_Filename (Module_Name));
      begin
         Generate_Root_Module (Filename, Format, Map_List);
      end;

      declare
         Module_Name : constant String := "tileset";
         Filename     : constant String :=
           Compose (Module_Dir, To_Rust_Filename (Module_Name));
      begin
         Generate_Tileset (Filename, Module_Name);
      end;

      declare
         Module_Name : constant String := "tileset_collisions";
         Filename     : constant String :=
           Compose (Module_Dir, To_Rust_Filename (Module_Name));
      begin
         Generate_Tileset_Collisions (Filename);
      end;

      for Map of Map_List loop
         declare
            Module_Name : constant String :=
              To_Rust_Identifier (Maps.Name (Map));
            Filename     : constant String :=
              Compose (Module_Dir, To_Rust_Filename (Module_Name));
         begin
            TCG.Maps.Generate_RSTE_Source (Map, Filename);
         end;
      end loop;

   end Gen_RSTE_Source;

end TCG.Outputs.RSTE;
