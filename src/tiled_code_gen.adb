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

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Directories;  use Ada.Directories;

with TCG.Palette;
with TCG.Maps;
with TCG.Maps.Render;
with TCG.Maps.List;
with TCG.Outputs.PDF;
with TCG.Outputs.GESTE;
with TCG.Outputs.LibGBA;
with TCG.Outputs.RSTE;
with TCG.Tilesets;
use TCG;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

procedure Tiled_Code_Gen is

   List : Maps.List.List;

   Config           : Command_Line_Configuration;
   PDF_Enabled      : aliased Boolean;
   GESTE_Enabled    : aliased Boolean;
   RSTE_Enabled     : aliased Boolean;
   GBA_Enabled      : aliased Boolean;
   BMP_Enabled      : aliased Boolean;
   Source_Out_Dir   : aliased String_Access := new String'("src");
   Doc_Out_Dir      : aliased String_Access := new String'("doc");
   Color_Format_Str : aliased String_Access := new String'("RGB565");
   Color_Format     : Palette.Output_Color_Format;
   Root_Package     : aliased String_Access := new String'("Game_Assets");

   Tileset          : Tilesets.Tileset_Id := Tilesets.Invalid_Tileset;
   use type Tilesets.Tileset_Id;
begin

   declare
   begin
      Define_Switch
        (Config, PDF_Enabled'Access, "-p",
         Long_Switch => "--pdf",
         Help => "Generate PDF documentation");

      Define_Switch
        (Config, GESTE_Enabled'Access, "-g",
         Long_Switch => "--geste",
         Help => "Generate code for GEneric Sprite and Tile Engine");

      Define_Switch
        (Config, RSTE_Enabled'Access, "",
         Long_Switch => "--rste",
         Help => "Generate code for Rust Sprite and Tile Engine");

      Define_Switch
        (Config, GBA_Enabled'Access, "",
         Long_Switch => "--libgba",
         Help => "Generate code for Rust Sprite and Tile Engine");

      Define_Switch
        (Config, BMP_Enabled'Access, "-b",
         Long_Switch => "--bmp",
         Help => "Generate bitmap images of the maps");

      Define_Switch
        (Config, Root_Package'Access, "-r:",
         Long_Switch => "--root-package-name=",
         Help => "Name of the root package of generated sources (default: " &
           Root_Package.all & ")");

      Define_Switch
        (Config, Source_Out_Dir'Access, "-o:",
         Long_Switch => "--source-out-dir=",
         Help => "Output directory for the generated sources (default: " &
           Source_Out_Dir .all & ")");

      Define_Switch
        (Config, Doc_Out_Dir'Access, "-d:",
         Long_Switch => "--doc-out-dir=",
         Help => "Output directory for the generated documentation " &
           "(default: " & Doc_Out_Dir.all & ")");

      Define_Switch
        (Config, Color_Format_Str'Access, "-f:",
         Long_Switch => "--color-format=",
         Help => "Color format used in generated sources (" &
           Palette.Supported_Formats & ") (default: " &
           Color_Format_Str.all & ")");

      Set_Usage
        (Config,
         "[switches] maps (.tmx) or tilesets (.tsx)",
         "Tiled-Code-Gen, a code generator for Tiled the map editor");

      Getopt (Config);
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      when GNAT.Command_Line.Exit_From_Command_Line =>
         return;
   end;

   --  Checking output color format
   if Palette.Format_Supported (Color_Format_Str.all) then
      Color_Format := Palette.Convert (Color_Format_Str.all);
   else
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Unsuported color format: " & Color_Format_Str.all);
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   --  Processing each Tiled maps
   loop
      declare
         Arg    : constant String := Get_Argument (Do_Expansion => True);
      begin
         exit when Arg'Length = 0;
         if Ends_With (Arg, ".tmx") then
            List.Append (TCG.Maps.Load (Arg, Base_Name (Arg)));
         elsif Ends_With (Arg, ".tsx") then
            Tileset := Tilesets.Load (Arg);
            Tilesets.Fill_Master_Tileset (Tileset);
         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Unknown file format: '" & Arg & "'");
         end if;
      end;
   end loop;

   if List.Is_Empty and then Tileset = Tilesets.Invalid_Tileset then
      Display_Help (Config);
      return;
   end if;

   for Map of List loop
      TCG.Maps.Fill_Master_Tileset (Map);
   end loop;

   if GESTE_Enabled then
      Outputs.GESTE.Gen_GESTE_Source (Directory         => Source_Out_Dir.all,
                                      Root_Package_Name => Root_Package.all,
                                      Format            => Color_Format,
                                      Map_List          => List);
   end if;

   if GBA_Enabled then
      Outputs.LibGBA.Gen_LibGBA_Source
        (Directory         => Source_Out_Dir.all,
         Root_Package_Name => Root_Package.all,
         Map_List          => List);
   end if;

   if RSTE_Enabled then
      Outputs.RSTE.Gen_RSTE_Source (Directory        => Source_Out_Dir.all,
                                    Root_Module_Name => Root_Package.all,
                                    Format           => Color_Format,
                                    Map_List         => List);
   end if;

   if PDF_Enabled then
      Outputs.PDF.Gen_PDF_Doc (Doc_Out_Dir.all, "doc.pdf", List);
   end if;

   if BMP_Enabled then
      for M of List loop
         TCG.Maps.Render.To_BMP
           (M,
            Path       => Compose (Doc_Out_Dir.all, Maps.Name (M) & ".bmp"),
            Background => (255, 255, 255, 255));
      end loop;
   end if;
   if not GESTE_Enabled
     and then
      not GBA_Enabled
     and then
      not PDF_Enabled
     and then
      not BMP_Enabled
   then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "Warning: no generator enabled.");
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "Use --help to see the list of options.");
   end if;

end Tiled_Code_Gen;
