------------------------------------------------------------------------------
--                                                                          --
--                             tiled-code-gen                               --
--                                                                          --
--                    Copyright (C) 2021 Fabien Chouteau                    --
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
with TCG.Palette;
use TCG;

package body TCG.Outputs.LibGBA is

   use type Tilesets.Master_Tile_Id;
   use type Palette.Color_Id;

   procedure Generate_Charblock (Filepath     : String;
                                 Package_Name : String);

   procedure Generate_Tileset_Collisions (Filepath     : String;
                                          Package_Name : String);

   procedure Generate_Root_Package
     (Filename     : String;
      Package_Name : String;
      Format       : Palette.Output_Color_Format);

   ------------------------
   -- Generate_Charblock --
   ------------------------

   procedure Generate_Charblock (Filepath     : String;
                               Package_Name : String)
   is
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
      pragma Style_Checks ("M200");

      Create (Output, Out_File, Filepath);
      PL ("with GBA.Graphics.Charblocks;");
      PL ("pragma Style_Checks (Off);");
      PL ("package " & Package_Name & " is");
      NL;
      PL ("   Block : aliased constant GBA.Graphics.Charblocks.Charblock_Raw :=");
      PL ("     (");

      for Id in Tilesets.First_Id .. Tilesets.Last_Id loop
         for Y in 1 .. Tilesets.Tile_Height loop

            P ("             ");

            for X in 1 .. Tilesets.Tile_Width loop
               if Id /= Tilesets.No_Tile and then Id <= Tilesets.Last_Id then
                  P (Palette.Color_Id'Image ((Tilesets.Pix (Id, X, Y))));
               else
                  P (" 0");
               end if;

               if X /= Tilesets.Tile_Width then
                  P (",");
               end if;
            end loop;

            if Y /= Tilesets.Tile_Height then
               PL (",");
            else
               P ("");
            end if;
         end loop;

         if Id /= Tilesets.Last_Id then
            PL (",");
         else
               PL (");");
         end if;
      end loop;

      PL ("end " & Package_Name & ";");

      Close (Output);
   end Generate_Charblock;

   ---------------------------------
   -- Generate_Tileset_Collisions --
   ---------------------------------

   procedure Generate_Tileset_Collisions (Filepath     : String;
                                          Package_Name : String)
   is
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
      PL ("with GBA.Graphics.Charblocks;");
      PL ("pragma Style_Checks (Off);");
      PL ("package " & Package_Name & " is");
      NL;
      PL ("   Block : aliased constant GBA.Graphics.Charblocks.Charblock_Raw :=");
      PL ("     (");

      for Id in Tilesets.First_Id .. Tilesets.Last_Id loop
         for Y in 1 .. Tilesets.Tile_Height loop

            P ("             ");

            for X in 1 .. Tilesets.Tile_Width loop
               P (if Id /= Tilesets.No_Tile
                    and then
                     Id <= Tilesets.Last_Id
                    and then
                     Tilesets.Collision (Id, X, Y)
                  then
                     "1"
                  else
                     "0");
               if X /= Tilesets.Tile_Width then
                  P (",");
               end if;
            end loop;

            if Y /= Tilesets.Tile_Height then
               PL (",");
            else
               P ("");
            end if;
         end loop;

         if Id /= Tilesets.Last_Id then
            PL (",");
         else
               PL (");");
         end if;
      end loop;

      PL ("end " & Package_Name & ";");

      Close (Output);
   end Generate_Tileset_Collisions;

   ---------------------------
   -- Generate_Root_Package --
   ---------------------------

   procedure Generate_Root_Package
     (Filename     : String;
      Package_Name : String;
      Format       : Palette.Output_Color_Format)
   is
      Output : File_Type;
   begin
      Create (Output, Out_File, Filename);

      Put_Line (Output, "with GBA.Graphics.Palettes;");
      Put_Line (Output, "with GBA.Graphics.Charblocks;");
      New_Line (Output);
      Put_Line (Output, "pragma Style_Checks (Off);");
      Put_Line (Output, "package " & Package_Name & " is");
      New_Line (Output);

      New_Line (Output);
      Put_Line (Output,
                "   Palette : aliased GBA.Graphics.Palettes.Palette := (");

      declare
         use TCG.Palette;

         First : constant Color_Id := Palette.First_Id;
         Last  : constant Color_Id := Palette.Last_Id;

         --  The GBA palettes have fixed size 256 colors
         Max   : constant Color_Id := 255;
      begin
         if First /= 0 then
            raise Program_Error with "expected Palette.First_Id = 0";
         end if;

         if Last > 255 then
            raise Program_Error with "Palette size (" &
              Palette.Number_Of_Colors'Img &
              ")  above maximum allowed for LibGBA (256)";
         end if;

         for Id in First .. Max  loop
            if Id <= Last then
               Put (Output, "     " & Id'Img & " => " &
                      Palette.Image (Palette.Convert (Id), Format));
            else
               Put (Output, "     " & Id'Img & " => 0");
            end if;

            if Id /= Max then
               Put_Line (Output, ",");
            end if;
         end loop;
      end;
      Put_Line (Output, ");");

      New_Line (Output);
      Put_Line (Output, "   type Object_Kind is (Rectangle_Obj, Point_Obj,");
      Put_Line (Output, "     Ellipse_Obj, Polygon_Obj, Tile_Obj, Text_Obj);");
      New_Line (Output);
      Put_Line (Output, "   type String_Access is access all String;");
      New_Line (Output);
      Put_Line (Output, "   type Object");
      Put_Line (Output, "     (Kind : Object_Kind := Rectangle_Obj)");
      Put_Line (Output, "   is record");
      Put_Line (Output, "      Name           : String_Access;");
      Put_Line (Output, "      Id             : Natural;");
      Put_Line (Output, "      X              : Float;");
      Put_Line (Output, "      Y              : Float;");
      Put_Line (Output, "      Width          : Float;");
      Put_Line (Output, "      Height         : Float;");
      --  Put_Linr (Output, "      Points  : Polygon_Access;");
      Put_Line (Output, "      Str            : String_Access;");
      Put_Line (Output, "      Flip_Vertical  : Boolean;");
      Put_Line (Output, "      Flip_Horizontal: Boolean;");
      Put_Line
        (Output, "      Tile_Id        : GBA.Graphics.Charblocks.Tile_Id;");
      Put_Line (Output, "   end record;");
      New_Line (Output);
      Put_Line (Output, "   type Object_Array is array (Natural range <>)");
      Put_Line (Output, "      of Object;");
      New_Line (Output);
      Put_Line (Output, "end " & Package_Name & ";");
      Close (Output);
   end Generate_Root_Package;

   -----------------------
   -- Gen_LibGBA_Source --
   -----------------------

   procedure Gen_LibGBA_Source
     (Directory          : String;
      Root_Package_Name  : String;
      Map_List           : TCG.Maps.List.List)
   is
      Format : constant Palette.Output_Color_Format := Palette.RGB555;
   begin
      if not TCG.Utils.Make_Dir (Directory) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Cannot create directory for GESTE code: '" & Directory & "'");
         return;
      end if;

      if Tilesets.Tile_Width /= Tilesets.Tile_Height then
         raise Program_Error with "Tiles are not square";
      end if;

      if Tilesets.Tile_Width /= 8 then
         raise Program_Error with "Only 8x8 tiles allowed for LibGBA";
      end if;

      if Tilesets.Number_Of_Tiles > 512 then
         raise Program_Error
           with "Number of tiles (" & Tilesets.Number_Of_Tiles'Img &
           ") above maximum allowed for LibGBA (512)";
      end if;

      declare
         Package_Name : constant String := Root_Package_Name;
         Filename     : constant String :=
           Compose (Directory, To_Ada_Filename (Package_Name));
      begin
         Generate_Root_Package (Filename, Package_Name, Format);
      end;

      declare
         Package_Name : constant String := Root_Package_Name & ".Charblock";
         Filename     : constant String :=
           Compose (Directory, To_Ada_Filename (Package_Name));
      begin
         Generate_Charblock (Filename, Package_Name);
      end;

      declare
         Package_Name : constant String :=
           Root_Package_Name & ".Collisions";
         Filename     : constant String :=
           Compose (Directory, To_Ada_Filename (Package_Name));
      begin
         Generate_Tileset_Collisions (Filename, Package_Name);
      end;

      for Map of Map_List loop
         declare
            Package_Name : constant String :=
              Root_Package_Name & "." & To_Ada_Identifier (Maps.Name (Map));
            Filename     : constant String :=
              Compose (Directory, To_Ada_Filename (Package_Name));
         begin
            TCG.Maps.Generate_LibGBA_Source (Map, Package_Name, Filename);
         end;
      end loop;

   end Gen_LibGBA_Source;

end TCG.Outputs.LibGBA;
