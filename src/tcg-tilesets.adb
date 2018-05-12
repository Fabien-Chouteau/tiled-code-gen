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
with Ada.Calendar;

with TCG.Palette;        use TCG.Palette;
with TCG.Utils;          use TCG.Utils;

with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with GID;

package body TCG.Tilesets is

   function Create (N : Node) return Local_Tileset;

   procedure Load_Data (This     : in out Local_Tileset;
                        Base_Dir : String;
                        N        : Node);

   ------------
   -- Create --
   ------------

   function Create (N : Node) return Local_Tileset is
      Number_Of_Tiles : constant Natural := Item_As_Natural (N, "tilecount");
      Tile_Width      : constant Natural := Item_As_Natural (N, "tilewidth");
      Tile_Height     : constant Natural := Item_As_Natural (N, "tileheight");
      Columns         : constant Natural := Item_As_Natural (N, "columns");

      TS : Local_Tileset;
   begin
      if M_Tile_Width = 0 then
         M_Tile_Width := Tile_Width;
      elsif Tile_Width /= M_Tile_Width then
         raise Program_Error with "invalid tile witdth";
      end if;

      if M_Tile_Height = 0 then
         M_Tile_Height := Tile_Height;
      elsif Tile_Height /= M_Tile_Height then
         raise Program_Error with "invalid tile height";
      end if;

      TS.Columns := Columns;
      TS.Number_Of_Tiles := Number_Of_Tiles;
      TS.Name := new String'(Item_As_String (N, "name"));
      return TS;
   end Create;

   ---------------
   -- Load_Data --
   ---------------

   procedure Load_Data (This     : in out Local_Tileset;
                        Base_Dir : String;
                        N        : Node)
   is

      Current_X, Current_Y : Natural := 0;

      Width, Height : Natural := 0;

      procedure Set_X_Y (X, Y : Natural);
      procedure Put_Pixel (R, G, B : Palette.Component;
                           A       : Palette.Component);
      procedure Feedback (Percent : Natural);

      procedure Set_X_Y (X, Y : Natural) is
      begin
         Current_X := X;
         Current_Y := Y;
      end Set_X_Y;

      ---------------
      -- Put_Pixel --
      ---------------

      procedure Put_Pixel (R, G, B : Palette.Component;
                           A       : Palette.Component)
      is
         use type Palette.Component;

         Id : Palette.Color_Id;
         X  : constant Natural := Current_X;
         Y  : constant Natural := (Height - 1) - Current_Y;

         Loc_Id  : Local_Tile_Id;
         Mast_Id : Master_Tile_Id;
      begin

         if A = 0 then
            Id := Transparent;
         else
            Id := Palette.Add_Color ((A, R, G, B));
         end if;

         Loc_Id := Local_Tile_Id
           ((X / Tile_Width) + (Y / Tile_Height) * This.Columns);

         Mast_Id := This.First_Master_Tile + Master_Tile_Id (Loc_Id);

         Master_Tileset (Mast_Id).Pixels (1 + X mod Tile_Width,
                                          1 + Y mod Tile_Height) := Id;

         if Current_X < Width - 1 then
            Current_X := Current_X + 1;
         else
            Current_X := 0;
            Current_Y := Current_Y + 1;
         end if;
      end Put_Pixel;

      --------------
      -- Feedback --
      --------------

      procedure Feedback (Percent : Natural) is
      begin
         null;
      end Feedback;

      procedure GID_Load is
        new GID.Load_image_contents (Palette.Component,
                                     Set_X_Y,
                                     Put_Pixel,
                                     Feedback,
                                     GID.fast);

      Source : constant String :=
        Compose (Base_Dir, Item_As_String (N, "source"));
      Trans  : constant String := Item_As_String (N, "trans");

      F   : Ada.Streams.Stream_IO.File_Type;
      Des : GID.Image_descriptor;

      Unused : Ada.Calendar.Day_Duration;
   begin

      Palette.Set_Transparent (Palette.To_ARGB (Trans));

      --  Allocate the new tiles
      This.First_Master_Tile := Master_Tileset.Last_Index + 1;
      for Cnt in 1 .. This.Number_Of_Tiles loop
         Master_Tileset.Append (new Tile_Data (Tile_Width, Tile_Height));
      end loop;

      This.Source := new String'(Source);

      Open (F, In_File, Source);

      Put_Line ("Processing " & Source & "...");

      GID.Load_image_header (Des, Stream (F).all);

      Width := GID.Pixel_width (Des);
      Height := GID.Pixel_height (Des);

      GID_Load (Des, Unused);

   end Load_Data;

   ----------
   -- Load --
   ----------

   function Load (Path : String) return Tileset_Id is
      Dir    : constant String := Containing_Directory (Path);

      Input  : File_Input;
      Reader : Tree_Reader;
      Doc    : Document;
      List   : Node_List;
      N      : Node;
      TS     : Local_Tileset;
      Id     : Tileset_Id;
   begin

      Id := Already_Loaded (Path);
      if Id /= Invalid_Tileset then
         return Id;
      end if;

      Set_Public_Id (Input, "Tileset file");
      Open (Path, Input);

      Set_Feature (Reader, Validation_Feature, False);
      Set_Feature (Reader, Namespace_Feature, False);

      Parse (Reader, Input);
      Close (Input);

      Doc := Get_Tree (Reader);

      List := Get_Elements_By_Tag_Name (Doc, "tileset");

      if Length (List) > 1 then
         raise Program_Error with "Too many tileset elements";
      end if;

      N := Item (List, 0);

      TS := Create (N);

      Free (List);

      List := Get_Elements_By_Tag_Name (Doc, "image");

      if Length (List) > 1 then
         raise Program_Error with "Too many image elements";
      end if;

      N := Item (List, 0);

      Load_Data (TS, Dir, N);

      Free (List);

      Free (Reader);

      TS.Path := new String'(Path);

      Local_Tilesets.Append (TS);
      return Tileset_Id (Local_Tilesets.Last_Index);
   end Load;

   ----------
   -- Name --
   ----------

   function Name (Id : Tileset_Id) return String
   is (Local_Tilesets.Element (Natural (Id)).Name.all);

   -------------
   -- Convert --
   -------------

   function Convert (Id  : Tileset_Id;
                     Loc : Local_Tile_Id)
                     return Master_Tile_Id
   is
      TS : constant Local_Tileset := Local_Tilesets.Element (Natural (Id));
   begin
      return Master_Tile_Id (TS.First_Master_Tile + Master_Tile_Id (Loc));
   end Convert;

   ---------------------
   -- Number_Of_Tiles --
   ---------------------

   function Number_Of_Tiles return Natural
   is (Natural (Master_Tileset.Length) - 1);

   --------------
   -- First_Id --
   --------------

   function First_Id return Master_Tile_Id
   is (Master_Tileset.First_Index);

   -------------
   -- Last_Id --
   -------------

   function Last_Id return Master_Tile_Id
   is (Master_Tileset.Last_Index);

   ----------------
   -- Tile_Width --
   ----------------

   function Tile_Width return Positive
   is (M_Tile_Width);

   -----------------
   -- Tile_Height --
   -----------------

   function Tile_Height return Positive
   is (M_Tile_Height);

   ---------
   -- Pix --
   ---------

   function Pix (T : Master_Tile_Id;
                 X, Y : Positive)
                 return Palette.ARGB_Color
   is (Palette.Convert (Pix (T, X, Y)));

   ---------
   -- Pix --
   ---------

   function Pix (T    : Master_Tile_Id;
                 X, Y : Positive)
                 return Palette.Color_Id
   is (Master_Tileset.Element (T).Pixels (X, Y));

   ---------
   -- Put --
   ---------

   procedure Put is
      use Tile_Data_Vect;
   begin
      Put_Line ("Number_Of_Tiles: " & Number_Of_Tiles'Img);
      Put_Line ("Tile_Width: " & Tile_Width'Img);
      Put_Line ("Tile_Height: " & Tile_Height'Img);

      for This of Local_Tilesets loop
         Put_Line ("Tileset -> Name: " & This.Name.all);
         Put_Line ("Source: " &
                   (if This.Source /= null then This.Source.all else ""));
         Put_Line ("Number_Of_Tiles: " & This.Number_Of_Tiles'Img);
      end loop;

      for Cur in Master_Tileset.Iterate loop
         Put_Line ("Tile" & To_Index (Cur)'Img & ":");
         for Y in 0 .. Tile_Height - 1 loop
            for X in 1 .. Tile_Width - 1 loop
               Put (Palette.Image
                    (Tile_Data_Vect.Element (Cur).Pixels (X, Y)));
            end loop;
            New_Line;
         end loop;
      end loop;
   end Put;

   --------------------
   -- Already_Loaded --
   --------------------

   function Already_Loaded (Path : String) return Tileset_Id
   is
   begin
      for Cur in Local_Tilesets.Iterate loop
         if Tileset_Vect.Element (Cur).Path.all = Path then
            return Tileset_Id (Tileset_Vect.To_Index (Cur));
         end if;
      end loop;
      return Invalid_Tileset;
   end Already_Loaded;

begin
   Master_Tileset.Append (null);
   if Master_Tileset.Last_Index /= No_Tile then
      raise Program_Error;
   end if;
end TCG.Tilesets;
