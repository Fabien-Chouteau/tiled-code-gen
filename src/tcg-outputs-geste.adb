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

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with TCG.Utils; use TCG.Utils;
with TCG.Tilesets;
use TCG;

package body TCG.Outputs.GESTE is

   use type Tilesets.Master_Tile_Id;
   use type Palette.Color_Id;

   procedure Generate_Tileset (Filepath     : String;
                               Package_Name : String);

   procedure Generate_Tileset_Collisions (Filepath     : String;
                                          Package_Name : String);

   procedure Generate_GESET_Config
     (Filename     : String;
      Package_Name : String;
      Format       : Palette.Output_Color_Format);

   procedure Generate_Root_Package
     (Filename     : String;
      Package_Name : String;
      Format       : Palette.Output_Color_Format);

   ----------------------
   -- Generate_Tileset --
   ----------------------

   procedure Generate_Tileset (Filepath     : String;
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
      PL ("with GESTE;");
      PL ("pragma Style_Checks (Off);");
      PL ("package " & Package_Name & " is");
      NL;
      PL ("   Tiles : aliased constant GESTE.Tile_Array :=");
      PL ("     (");

      for Id in Tilesets.First_Id .. Tilesets.Last_Id loop
         if Id /= Tilesets.No_Tile then
            P ("     " & Id'Img & " => (");
            for X in 1 .. Tilesets.Tile_Width loop

               if X /= 1 then
                  P ("             ");
               end if;
               P ("(");

               for Y in 1 .. Tilesets.Tile_Height loop
                  P (Palette.Color_Id'Image ((Tilesets.Pix (Id, X, Y))));
                  if Y /= Tilesets.Tile_Height then
                     P (",");
                  end if;
               end loop;
               P (")");
               if X /= Tilesets.Tile_Width then
                  PL (",");
               else
                  P (")");
               end if;
            end loop;

            if Id /= Tilesets.Last_Id then
               PL (",");
            else
               PL (");");
            end if;
         end if;
      end loop;

      PL ("end " & Package_Name & ";");

      Close (Output);
   end Generate_Tileset;

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
      PL ("with GESTE;");
      PL ("pragma Style_Checks (Off);");
      PL ("package " & Package_Name & " is");
      NL;
      PL ("   Tiles : aliased constant GESTE.Tile_Collisions_Array :=");
      PL ("     (");

      for Id in Tilesets.First_Id .. Tilesets.Last_Id loop
         if Id /= Tilesets.No_Tile then
            P ("     " & Id'Img & " => (");
            for X in 1 .. Tilesets.Tile_Width loop

               if X /= 1 then
                  P ("             ");
               end if;
               P ("(");

               for Y in 1 .. Tilesets.Tile_Height loop

                  P (if Tilesets.Collision (Id, X, Y) then
                        "True"
                     else
                        "False");

                  if Y /= Tilesets.Tile_Height then
                     P (",");
                  end if;
               end loop;
               P (")");
               if X /= Tilesets.Tile_Width then
                  PL (",");
               else
                  P (")");
               end if;
            end loop;

            if Id /= Tilesets.Last_Id then
               PL (",");
            else
               PL (");");
            end if;
         end if;
      end loop;

      PL ("end " & Package_Name & ";");

      Close (Output);
   end Generate_Tileset_Collisions;

   ---------------------------
   -- Generate_GESET_Config --
   ---------------------------

   procedure Generate_GESET_Config
     (Filename     : String;
      Package_Name : String;
      Format       : Palette.Output_Color_Format)
   is
      Output : File_Type;
   begin
      Create (Output, Out_File, Filename);

      Put_Line (Output, "with Interfaces;");
      New_Line (Output);
      Put_Line (Output, "package " & Package_Name & " is");
      New_Line (Output);

      Put_Line (Output, "   type Color_Index is range " &
                  Palette.First_Id'Img & " .. " &
                  Palette.Last_Id'Img & ";");
      New_Line (Output);
      case Format is
         when Palette.ARGB =>
            Put_Line (Output, "   type Component is 0 .. 255 with Size => 8;");
            New_Line (Output);
            Put_Line (Output, "   type Output_Color is record");
            Put_Line (Output, "      A, R, G, B : Component;");
            Put_Line (Output, "   end record;");
         when Palette.RGB565 =>
            Put_Line (Output,
                      "   subtype Output_Color is Interfaces.Unsigned_16;");
      end case;

      New_Line (Output);
      Put_Line (Output, "   Transparent : constant Output_Color := " &
                  Palette.Image (Palette.Transparent, Format) & ";");

      New_Line (Output);
      Put_Line (Output, "   Tile_Size : constant := " &
                  Tilesets.Tile_Width'Img & ";");

      New_Line (Output);
      Put_Line (Output, "   type Tile_Index is range 0 .."  &
                  Tilesets.Number_Of_Tiles'Img & ";");

      Put_Line (Output, "   No_Tile : constant Tile_Index := 0;");
      Put_Line (Output, "end " & Package_Name & ";");
      Close (Output);
   end Generate_GESET_Config;

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

      Put_Line (Output, "with GESTE;");
      Put_Line (Output, "with GESTE.Maths_Types;");
      Put_Line (Output, "with GESTE_Config;");
      New_Line (Output);
      Put_Line (Output, "pragma Style_Checks (Off);");
      Put_Line (Output, "package " & Package_Name & " is");
      New_Line (Output);

      New_Line (Output);
      Put_Line (Output, "   Palette : aliased GESTE.Palette_Type := (");
      for Id in Palette.First_Id .. Palette.Last_Id loop
         Put (Output, "     " & Id'Img & " => " &
                Palette.Image (Palette.Convert (Id), Format));

         if Id /= Palette.Last_Id then
            Put_Line (Output, ",");
         end if;
      end loop;
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
      Put_Line (Output, "      X              : GESTE.Maths_Types.Value;");
      Put_Line (Output, "      Y              : GESTE.Maths_Types.Value;");
      Put_Line (Output, "      Width          : GESTE.Maths_Types.Value;");
      Put_Line (Output, "      Height         : GESTE.Maths_Types.Value;");
      --  Put_Linr (Output, "      Points  : Polygon_Access;");
      Put_Line (Output, "      Str            : String_Access;");
      Put_Line (Output, "      Flip_Vertical  : Boolean;");
      Put_Line (Output, "      Flip_Horizontal: Boolean;");
      Put_Line (Output, "      Tile_Id        : GESTE_Config.Tile_Index;");
      Put_Line (Output, "   end record;");
      New_Line (Output);
      Put_Line (Output, "   type Object_Array is array (Natural range <>)");
      Put_Line (Output, "      of Object;");
      New_Line (Output);
      Put_Line (Output, "end " & Package_Name & ";");
      Close (Output);
   end Generate_Root_Package;

   -----------------
   -- Gen_PDF_Doc --
   -----------------

   procedure Gen_GESTE_Source
     (Directory          : String;
      Root_Package_Name  : String;
      Format             : Palette.Output_Color_Format;
      Map_List           : TCG.Maps.List.List)
   is
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

      declare
         Package_Name : constant String := "GESTE_Config";
         Filename     : constant String :=
           Compose (Directory, To_Ada_Filename (Package_Name));
      begin
         Generate_GESET_Config (Filename, Package_Name, Format);
      end;

      declare
         Package_Name : constant String := Root_Package_Name;
         Filename     : constant String :=
           Compose (Directory, To_Ada_Filename (Package_Name));
      begin
         Generate_Root_Package (Filename, Package_Name, Format);
      end;

      declare
         Package_Name : constant String := Root_Package_Name & ".Tileset";
         Filename     : constant String :=
           Compose (Directory, To_Ada_Filename (Package_Name));
      begin
         Generate_Tileset (Filename, Package_Name);
      end;

      declare
         Package_Name : constant String :=
           Root_Package_Name & ".Tileset_Collisions";
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
            TCG.Maps.Generate_Ada_Source (Map, Package_Name, Filename);
         end;
      end loop;

   end Gen_GESTE_Source;

end TCG.Outputs.GESTE;
