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

with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

with GNAT.OS_Lib;

with DOM.Core; use DOM.Core;

with TCG.Utils;         use TCG.Utils;
with TCG.Tile_Layers;   use TCG.Tile_Layers;
with TCG.Object_Groups; use TCG.Object_Groups;

with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;

package body TCG.Maps is

   function Create (N : Node; Name : String) return Map;
   function Load_Tileset (N : Node; Base_Dir : String) return Map_Tileset;

   ------------
   -- Create --
   ------------

   function Create (N : Node; Name : String) return Map is
      Tile_Width  : constant Natural := Item_As_Natural (N, "tilewidth");
      Tile_Height : constant Natural := Item_As_Natural (N, "tileheight");
      Width       : constant Natural := Item_As_Natural (N, "width");
      Height      : constant Natural := Item_As_Natural (N, "height");

      M : constant Map := new Map_Data;
   begin
      M.Width := Width;
      M.Height := Height;
      M.Tile_Width := Tile_Width;
      M.Tile_Height := Tile_Height;
      M.Name := new String'(Name);
      return M;
   end Create;

   ------------------
   -- Load_Tileset --
   ------------------

   function Load_Tileset (N : Node;
                          Base_Dir : String)
                          return Map_Tileset
   is
      Source     : constant String :=
        GNAT.OS_Lib.Normalize_Pathname (Item_As_String (N, "source"),
                                        Base_Dir);
      First_Tile : constant Natural := Item_As_Natural (N, "firstgid");
   begin
      return (Tilesets.Load (Source), Tilesets.Map_Tile_Id (First_Tile));
   end Load_Tileset;

   ----------
   -- Load --
   ----------

   function Load (Path : String; Name : String) return Map is
      Dir    : constant String :=  Containing_Directory (Path);

      Input  : File_Input;
      Reader : Tree_Reader;
      Doc    : Document;
      List   : Node_List;
      N      : Node;

      M : Map;
   begin
      Set_Public_Id (Input, "Map file");
      Open (Path, Input);

      Set_Feature (Reader, Validation_Feature, False);
      Set_Feature (Reader, Namespace_Feature, False);

      Parse (Reader, Input);
      Close (Input);

      Doc := Get_Tree (Reader);

      List := Get_Elements_By_Tag_Name (Doc, "map");

      if Length (List) > 1 then
         raise Program_Error with "Too many map elements";
      end if;

      M := Create (Item (List, 0), Name);

      Free (List);

      List := Get_Elements_By_Tag_Name (Doc, "objectgroup");
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         M.Obj_Group_List.Append (Object_Groups.Load (N));
      end loop;
      Free (List);

      List := Get_Elements_By_Tag_Name (Doc, "tileset");
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         M.Tileset_List.Append (Load_Tileset (N, Dir));
      end loop;
      Free (List);

      List := Get_Elements_By_Tag_Name (Doc, "layer");
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         M.Layer_List.Append (Tile_Layers.Load (N));
      end loop;
      Free (List);

      Free (Reader);

      return M;
   end Load;

   ----------
   -- Name --
   ----------

   function Name (This : Map) return String
   is (This.Name.all);

   -----------
   -- Width --
   -----------

   function Width (This : Map) return Natural
   is (This.Width);

   ------------
   -- Height --
   ------------

   function Height (This : Map) return Natural
   is (This.Height);

   ----------------
   -- Tile_Width --
   ----------------

   function Tile_Width (This : Map) return Natural
   is (This.Tile_Width);

   -----------------
   -- Tile_Height --
   -----------------

   function Tile_Height (This : Map) return Natural
   is (This.Tile_Height);

   ---------
   -- Put --
   ---------

   procedure Put
     (This : Map)
   is
   begin
      Put_Line ("Map " & This.Name.all & ":");
      for TS of This.Tileset_List loop
         Put_Line ("Tileset " & Tilesets.Name (TS.Id));
      end loop;
      for L of This.Layer_List loop
         Tile_Layers.Put (L);
      end loop;
   end Put;

   ----------------------
   -- Number_Of_Layers --
   ----------------------

   function Number_Of_Layers (This : Map) return Natural
     is (Natural (Layer_Vect.Length (This.Layer_List)));

   -----------------
   -- First_Index --
   -----------------

   function First_Layer (This : Map) return Natural
   is (Natural (Layer_Vect.First_Index (This.Layer_List)));

   ----------------
   -- Last_Index --
   ----------------

   function Last_Layer (This : Map) return Natural
   is (Natural (Layer_Vect.Last_Index (This.Layer_List)));

   -----------
   -- Layer --
   -----------

   function Layer (This : Map; Index : Natural) return Tile_Layers.Tile_Layer
   is (This.Layer_List.Element (Index));

   -----------------
   -- Master_Tile --
   -----------------

   function Master_Tile (M  : Map;
                         Id : Tilesets.Map_Tile_Id)
                         return Tilesets.Master_Tile_Id
   is
      use Tilesets;

      First_Id : Map_Tile_Id := 0;
      TS_Id    : Tileset_Id := Invalid_Tileset;
   begin
      if Id = 0 then
         return No_Tile;
      end if;
      for TS of M.Tileset_List loop
         if TS.First_Tile > Id then
            exit;
         else
            TS_Id := TS.Id;
            First_Id := TS.First_Tile;
         end if;
      end loop;
      return Convert (TS_Id, Local_Tile_Id (Id - First_Id));
   end Master_Tile;

   ------------------------
   -- First_Object_Group --
   ------------------------

   function First_Object_Group (This : Map) return Natural
   is (This.Obj_Group_List.First_Index);

   -----------------------
   -- Last_Object_Group --
   -----------------------

   function Last_Object_Group (This : Map) return Natural
   is (This.Obj_Group_List.Last_Index);

   ------------------
   -- Object_Group --
   ------------------

   function Object_Group (This : Map; Index : Natural)
                          return Object_Groups.Object_Group
   is (This.Obj_Group_List.Element (Index));

   ---------------------------
   -- Generate_GESTE_Source --
   ---------------------------

   procedure Generate_GESTE_Source (M            : Map;
                                  Package_Name : String;
                                  Filepath     : String)
   is
      Output : File_Type;
      Indent : Natural := 0;

      procedure P (Str : String);
      procedure PL (Str : String);
      procedure NL;
      procedure Put_Object (M   : Map;
                            Obj : Object_Groups.Object);

      -------
      -- P --
      -------

      procedure P (Str : String) is
      begin
         Put (Output, Str);
      end P;

      --------
      -- PL --
      --------

      procedure PL (Str : String) is
      begin
         for X in 1 .. Indent loop
            Put (Output, " ");
         end loop;
         Put_Line (Output, Str);
      end PL;

      --------
      -- NL --
      --------

      procedure NL is
      begin
         New_Line (Output);
      end NL;

      ----------------
      -- Put_Object --
      ----------------

      procedure Put_Object (M   : Map;
                            Obj : Object_Groups.Object)
      is
      begin
            PL ("Kind => " & Obj.Kind'Img & ",");
            PL ("Id   => " & Obj.Id'Img & ",");

            if Obj.Name /= null then
               PL ("Name => new String'(""" & Obj.Name.all & """),");
            else
               PL ("Name => null,");
            end if;

            PL ("X    => " & Obj.Pt.X'Img & ",");
            PL ("Y    => " & Obj.Pt.Y'Img & ",");

            PL ("Width => " & Obj.Width'Img & ",");
            PL ("Height => " & Obj.Height'Img & ",");
            PL ("Flip_Vertical => " & Obj.Flip_Vertical'Img & ",");
            PL ("Flip_Horizontal => " & Obj.Flip_Horizontal'Img & ",");
            PL ("Tile_Id => " & Master_Tile (M, Obj.Tile_Id)'Img & ",");

            if Obj.Str /= null then
               PL ("Str => new String'(""" & Obj.Str.all & """)");
            else
               PL ("Str => null");
            end if;
      end Put_Object;
   begin
      Create (Output, Out_File, Filepath);

      PL ("with GESTE;");
      PL ("with GESTE.Grid;");
      PL ("pragma Style_Checks (Off);");
      PL ("package " & Package_Name & " is");

      NL;
      Indent := Indent + 3;
      PL ("--  " & M.Name.all);
      PL ("Width       : constant :=" & M.Width'Img & ";");
      PL ("Height      : constant :=" & M.Height'Img & ";");
      PL ("Tile_Width  : constant :=" & M.Tile_Width'Img & ";");
      PL ("Tile_Height : constant :=" & M.Tile_Height'Img & ";");
      NL;

      for L of M.Layer_List loop
         declare
            Layer_Ada_Id : constant String := To_Ada_Identifier (Name (L));
         begin
            PL ("--  " & Name (L));
            PL ("package " & Layer_Ada_Id & " is");
            Indent := Indent + 3;
            PL ("Width  : constant := " & Width (L)'Img & ";");
            PL ("Height : constant := " & Width (L)'Img & ";");
            PL ("Data   : aliased GESTE.Grid.Grid_Data :=");
            P  ("  (");

            for X in 1 .. Width (L) loop

               if X /= 1 then
                  P ("         ");
               end if;
               P ("(");

               for Y in 1 .. Height (L) loop
                  P (Master_Tile (M, Tile (L, X, Y))'Img);
                  if Y /= Height (L) then
                     P (",");
                  end if;
               end loop;
               P (")");
               if X /= Width (L) then
                  P (",");
                  NL;
               else
                  P (")");
               end if;
            end loop;
            PL (";");

            Indent := Indent - 3;

            PL ("end " & Layer_Ada_Id & ";");
            NL;
         end;
      end loop;

      for G of M.Obj_Group_List loop
         declare
            Group_Ada_Id : constant String := To_Ada_Identifier (Name (G));
         begin
            PL ("package " & Group_Ada_Id & " is");

            Indent := Indent + 3;

            if Length (G) /= 0 then
               --  Objects as array
               PL ("Objects : Object_Array :=");
               Indent := Indent + 2;
               PL ("(");
               Indent := Indent + 2;
               for Index in First_Index (G) .. Last_Index (G) loop
                  declare
                     Obj : constant Object_Groups.Object :=
                       Get_Object (G, Index);
                  begin
                     PL (Index'Img & " => (");

                     Indent := Indent + 2;
                     Put_Object (M, Obj);
                     Indent := Indent - 2;
                     if Index = Last_Index (G) then
                        PL (")");
                     else
                        PL ("),");
                     end if;
                  end;
               end loop;
               Indent := Indent - 2;
               PL (");");
               Indent := Indent - 2;
            end if;

            if Length (G) /= 0 then
               --  Object as indivial declaration
               for Index in First_Index (G) .. Last_Index (G) loop
                  declare
                     Obj : constant Object_Groups.Object :=
                       Get_Object (G, Index);
                  begin
                     if Obj.Name /= null then
                        PL (TCG.Utils.To_Ada_Identifier (Obj.Name.all) &
                              " : aliased constant Object := (");
                        Indent := Indent + 2;
                        Put_Object (M, Obj);
                        PL (");");
                        Indent := Indent - 2;
                     end if;
                  end;
               end loop;
            end if;
            Indent := Indent - 3;
            PL ("end " & Group_Ada_Id & ";");
         end;
      end loop;
      Indent := Indent - 3;

      PL ("end " & Package_Name & ";");

      Close (Output);
   end Generate_GESTE_Source;

   ----------------------------
   -- Generate_LibGBA_Source --
   ----------------------------

   procedure Generate_LibGBA_Source (M            : Map;
                                     Package_Name : String;
                                     Filepath     : String)
   is
      Output : File_Type;
      Indent : Natural := 0;

      procedure P (Str : String);
      procedure PL (Str : String);
      procedure NL;
      procedure Put_Object (M   : Map;
                            Obj : Object_Groups.Object);

      -------
      -- P --
      -------

      procedure P (Str : String) is
      begin
         Put (Output, Str);
      end P;

      --------
      -- PL --
      --------

      procedure PL (Str : String) is
      begin
         for X in 1 .. Indent loop
            Put (Output, " ");
         end loop;
         Put_Line (Output, Str);
      end PL;

      --------
      -- NL --
      --------

      procedure NL is
      begin
         New_Line (Output);
      end NL;

      ----------------
      -- Put_Object --
      ----------------

      procedure Put_Object (M   : Map;
                            Obj : Object_Groups.Object)
      is
      begin
            PL ("Kind => " & Obj.Kind'Img & ",");
            PL ("Id   => " & Obj.Id'Img & ",");

            if Obj.Name /= null then
               --  PL ("Name => new String'(""" & Obj.Name.all & """),");
               PL ("Name => null,");
            else
               PL ("Name => null,");
            end if;

            PL ("X    => " & Obj.Pt.X'Img & ",");
            PL ("Y    => " & Obj.Pt.Y'Img & ",");

            PL ("Width => " & Obj.Width'Img & ",");
            PL ("Height => " & Obj.Height'Img & ",");
            PL ("Flip_Vertical => " & Obj.Flip_Vertical'Img & ",");
            PL ("Flip_Horizontal => " & Obj.Flip_Horizontal'Img & ",");
            PL ("Tile_Id => " & Master_Tile (M, Obj.Tile_Id)'Img & ",");

            if Obj.Str /= null then
               --  PL ("Str => new String'(""" & Obj.Str.all & """)");
               PL ("Str => null");
            else
               PL ("Str => null");
            end if;
      end Put_Object;
   begin
      pragma Style_Checks ("M200");

      Create (Output, Out_File, Filepath);

      PL ("with GBA.Graphics.Background.Viewport;");
      PL ("pragma Style_Checks (Off);");
      PL ("package " & Package_Name & " is");

      NL;
      Indent := Indent + 3;
      PL ("--  " & M.Name.all);
      PL ("Width       : constant :=" & M.Width'Img & ";");
      PL ("Height      : constant :=" & M.Height'Img & ";");
      PL ("Tile_Width  : constant :=" & M.Tile_Width'Img & ";");
      PL ("Tile_Height : constant :=" & M.Tile_Height'Img & ";");
      NL;

      for L of M.Layer_List loop
         declare
            Layer_Ada_Id : constant String := To_Ada_Identifier (Name (L));
         begin
            PL ("--  " & Name (L));
            PL ("package " & Layer_Ada_Id & " is");
            Indent := Indent + 3;
            PL ("Width  : constant := " & Width (L)'Img & ";");
            PL ("Height : constant := " & Height (L)'Img & ";");
            PL ("Data   : aliased GBA.Graphics.Background.Viewport.Raw_Screenblock :=");
            P  ("  (");

            for Y in 1 .. Height (L) loop

               if Y /= 1 then
                  P ("         ");
               end if;

               for X in 1 .. Width (L) loop
                  P (Master_Tile (M, Tile (L, X, Y))'Img);
                  if X /= Width (L) then
                     P (",");
                  end if;
               end loop;

               if Y /= Height (L) then
                  P (",");
                  NL;
               else
                  P (")");
               end if;
            end loop;
            PL (";");

            PL ("Info : constant GBA.Graphics.Background.Viewport.Map_Info := (Data'Access, Width, Height);");
            Indent := Indent - 3;
            PL ("end " & Layer_Ada_Id & ";");
            NL;
         end;
      end loop;

      for G of M.Obj_Group_List loop
         declare
            Group_Ada_Id : constant String := To_Ada_Identifier (Name (G));
         begin
            PL ("package " & Group_Ada_Id & " is");

            Indent := Indent + 3;

            if Length (G) /= 0 then
               --  Objects as array
               PL ("Objects : Object_Array :=");
               Indent := Indent + 2;
               PL ("(");
               Indent := Indent + 2;
               for Index in First_Index (G) .. Last_Index (G) loop
                  declare
                     Obj : constant Object_Groups.Object :=
                       Get_Object (G, Index);
                  begin
                     PL (Index'Img & " => (");

                     Indent := Indent + 2;
                     Put_Object (M, Obj);
                     Indent := Indent - 2;
                     if Index = Last_Index (G) then
                        PL (")");
                     else
                        PL ("),");
                     end if;
                  end;
               end loop;
               Indent := Indent - 2;
               PL (");");
               Indent := Indent - 2;
            end if;

            if Length (G) /= 0 then
               --  Object as indivial declaration
               for Index in First_Index (G) .. Last_Index (G) loop
                  declare
                     Obj : constant Object_Groups.Object :=
                       Get_Object (G, Index);
                  begin
                     if Obj.Name /= null then
                        PL (TCG.Utils.To_Ada_Identifier (Obj.Name.all) &
                              " : aliased constant Object := (");
                        Indent := Indent + 2;
                        Put_Object (M, Obj);
                        PL (");");
                        Indent := Indent - 2;
                     end if;
                  end;
               end loop;
            end if;
            Indent := Indent - 3;
            PL ("end " & Group_Ada_Id & ";");
         end;
      end loop;
      Indent := Indent - 3;

      PL ("end " & Package_Name & ";");

      Close (Output);
   end Generate_LibGBA_Source;

   --------------------------
   -- Generate_RSTE_Source --
   --------------------------

   procedure Generate_RSTE_Source (M        : Map;
                                   Filepath : String)
   is
      Output : File_Type;
      Indent : Natural := 0;

      procedure P (Str : String);
      procedure PL (Str : String);
      procedure NL;
      procedure Put_Object (M   : Map;
                            Obj : Object_Groups.Object);
      function Rust_Boolean (B : Boolean) return String;
      function Rust_Object_Kind (O : Object_Kind) return String;

      ------------------
      -- Rust_Boolean --
      ------------------

      function Rust_Boolean (B : Boolean) return String
      is (if B then "true" else "false");

      ----------------------
      -- Rust_Object_Kind --
      ----------------------

      function Rust_Object_Kind (O : Object_Kind) return String
      is (case O is
             when Point_Obj     => "super::super::ObjectKind::Point",
             when Rectangle_Obj => "super::super::ObjectKind::Rectangle",
             when Ellipse_Obj   => "super::super::ObjectKind::Ellipse",
             when Polygon_Obj   => "super::super::ObjectKind::Polygon",
             when Tile_Obj      => "super::super::ObjectKind::Tile",
             when Text_Obj      => "super::super::ObjectKind::Text");

      -------
      -- P --
      -------

      procedure P (Str : String) is
      begin
         Put (Output, Str);
      end P;

      --------
      -- PL --
      --------

      procedure PL (Str : String) is
      begin
         for X in 1 .. Indent loop
            Put (Output, " ");
         end loop;
         Put_Line (Output, Str);
      end PL;

      --------
      -- NL --
      --------

      procedure NL is
      begin
         New_Line (Output);
      end NL;

      ----------------
      -- Put_Object --
      ----------------

      procedure Put_Object (M   : Map;
                            Obj : Object_Groups.Object)
      is
      begin
         PL ("kind : " & Rust_Object_Kind (Obj.Kind) & ",");
         PL ("id   : " & Obj.Id'Img & ",");

         if Obj.Name /= null then
            PL ("name :""" & Obj.Name.all & """,");
         else
            PL ("name : """",");
         end if;

         PL ("x    : " & Obj.Pt.X'Img & ",");
         PL ("y    : " & Obj.Pt.Y'Img & ",");

         PL ("width : " & Obj.Width'Img & ",");
         PL ("height : " & Obj.Height'Img & ",");
         PL ("flip_vertical : " & Rust_Boolean (Obj.Flip_Vertical) & ",");
         PL ("flip_horizontal : " & Rust_Boolean (Obj.Flip_Horizontal) & ",");
         PL ("tile_id : " & Master_Tile (M, Obj.Tile_Id)'Img & ",");

         if Obj.Str /= null then
            PL ("str : """ & Obj.Str.all & """,");
         else
            PL ("str : """",");
         end if;
      end Put_Object;
   begin
      Create (Output, Out_File, Filepath);

      PL ("//  " & M.Name.all);
      PL ("#[allow(unused_imports)]");
      NL;
      PL ("const WIDTH       : usize =" & M.Width'Img & ";");
      PL ("const HEIGHT      : usize =" & M.Height'Img & ";");
      PL ("const TILE_WIDTH  : usize =" & M.Tile_Width'Img & ";");
      PL ("const TILE_HEIGHT : usize =" & M.Tile_Height'Img & ";");
      NL;

      for L of M.Layer_List loop
         PL ("pub mod " & To_Rust_Identifier (Name (L)) & " {");
         Indent := Indent + 4;
         PL ("use sprite_and_tile::*;");
         PL ("pub const WIDTH  : usize = " & Width (L)'Img & ";");
         PL ("pub const HEIGHT : usize = " & Height (L)'Img & ";");
         PL ("pub static TILE_MAP_DATA : [usize;" &
               Integer'Image (Width (L) * Height (L)) & "] =");
         P  ("        [");

         for Y in 1 .. Height (L) loop

            if Y /= 1 then
               P ("         ");
            end if;

            for X in 1 .. Width (L) loop
               P (Master_Tile (M, Tile (L, X, Y))'Img);
               P (",");
            end loop;
            if Y /= Height (L) then
               NL;
            end if;
         end loop;
         PL ("];");

         PL ("static TILE_MAP : TileMap =");
         PL ("   TileMap {width  : WIDTH,");
         PL ("            height : HEIGHT,");
         PL ("            map    : & TILE_MAP_DATA}; ");
         Indent := Indent - 4;

         PL ("}");
         NL;
      end loop;

      for G of M.Obj_Group_List loop
         PL ("pub mod " & To_Rust_Identifier (Name (G)) & " {");

         Indent := Indent + 3;

         if Length (G) /= 0 then
            --  Objects as array
            PL ("pub static OBJECTS : [super::super::Object;" &
                  Length (G)'Img & "] =");
            Indent := Indent + 2;
            PL ("[");
            Indent := Indent + 2;
            for Index in First_Index (G) .. Last_Index (G) loop
               declare
                  Obj : constant Object_Groups.Object :=
                    Get_Object (G, Index);
               begin
                  PL ("super::super::Object {");

                  Indent := Indent + 2;
                  Put_Object (M, Obj);
                  Indent := Indent - 2;
                  PL ("},");
               end;
            end loop;
            Indent := Indent - 2;
            PL ("];");
            Indent := Indent - 2;
         end if;

         if Length (G) /= 0 then
            --  Object as indivial declaration
            for Index in First_Index (G) .. Last_Index (G) loop
               declare
                  Obj : constant Object_Groups.Object :=
                    Get_Object (G, Index);
               begin
                  if Obj.Name /= null then
                     PL ("pub static " &
                           TCG.Utils.To_Rust_Static_Identifier (Obj.Name.all) &
                           " : super::super::Object = " &
                           "super::super::Object {");
                     Indent := Indent + 2;
                     Put_Object (M, Obj);
                     PL ("};");
                     Indent := Indent - 2;
                  end if;
               end;
            end loop;
         end if;
         Indent := Indent - 3;
         PL ("}");
      end loop;
      Close (Output);
   end Generate_RSTE_Source;

   -------------------------
   -- Fill_Master_Tileset --
   -------------------------

   procedure Fill_Master_Tileset (M : Map) is
      Unused : Tilesets.Master_Tile_Id;
   begin
      --  For all layers...
      for L of M.Layer_List loop
         --  For all tiles...
         for X in 1 .. Width (L) loop
            for Y in 1 .. Height (L) loop

               --  Convert to a Master_Tile_ID to make sure the tile is
               --  added to the master tile set.
               Unused := Master_Tile (M, Tile (L, X, Y));
            end loop;
         end loop;

         --  For all groups...
         for G of M.Obj_Group_List loop
            if Length (G) /= 0 then
               --  For all objects...
               for Index in First_Index (G) .. Last_Index (G) loop
                  declare
                     Obj : constant Object_Groups.Object :=
                       Get_Object (G, Index);
                  begin

                     --  Convert the tile to a Master_Tile_ID to make sure the
                     --  tile is added to the master tile set.
                     Unused := Master_Tile (M, Obj.Tile_Id);
                  end;
               end loop;
            end if;
         end loop;
      end loop;
   end Fill_Master_Tileset;

end TCG.Maps;
