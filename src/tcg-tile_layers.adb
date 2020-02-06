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

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with DOM.Core;           use DOM.Core;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Elements;  use DOM.Core.Elements;

with TCG.Utils;    use TCG.Utils;
with TCG.Tilesets; use TCG.Tilesets;

package body TCG.Tile_Layers is

   function Create (N : Node) return Tile_Layer;
   procedure Load_Data (L : Tile_Layer; N : Node)
     with Pre => L /= No_Layer;

   Whitespace : constant Ada.Strings.Maps.Character_Set :=
     not Ada.Strings.Maps.Constants.Decimal_Digit_Set;

   ------------
   -- Create --
   ------------

   function Create (N : Node) return Tile_Layer is
      Id     : Natural;
      Width  : constant Natural := Item_As_Natural (N, "width");
      Height : constant Natural := Item_As_Natural (N, "height");
      Name   : constant String := Item_As_String (N, "name");
      L      : constant Tile_Layer := new Layer_Data (Width, Height);
   begin
      if Item_Exists (N, "id") then
         Id := Item_As_Natural (N, "id");
      else
         --  When there is not ID it means that there is only one layer
         Id := 0;
      end if;

      L.Id := Tile_Layer_Id (Id);
      L.Name := new String'(Name);
      return L;
   end Create;

   ---------------
   -- Load_Data --
   ---------------

   procedure Load_Data (L : Tile_Layer; N : Node) is
      Data : constant String := Node_Value (First_Child (N));
      Cursor : Integer := Data'First;

      function Next_Tile return Map_Tile_Id;

      ---------------
      -- Next_Tile --
      ---------------

      function Next_Tile return Map_Tile_Id is
         From : constant Integer := Cursor;
         To   : Integer := Cursor;
      begin
         while To < Data'Last and then Data (To) /= ',' loop
            To := To + 1;
         end loop;

         Cursor := To + 1;

         return  Map_Tile_Id'Value
           (Trim (Data (From .. To - 1), Whitespace, Whitespace));
      end Next_Tile;

      Encoding : constant String := Item_As_String (N, "encoding");
   begin

      if Encoding /= "csv" then
         raise Program_Error with "Unsupported layer encoding: " & Encoding;
      end if;

      for Y in L.Map'Range (2) loop
         for X in L.Map'Range (1) loop
            L.Map (X, Y) := Next_Tile;
         end loop;
      end loop;
   end Load_Data;

   ----------
   -- Load --
   ----------

   function Load (Root : Node) return Tile_Layer is
      L : constant Tile_Layer := Create (Root);

      List : Node_List;
   begin
      List := Get_Elements_By_Tag_Name (Root, "data");

      if Length (List) > 1 then
         raise Program_Error with "Too many data elements";
      end if;
      Load_Data (L, Item (List, 0));
      Free (List);

      return L;
   end Load;

   ----------
   -- Name --
   ----------

   function Name (This : Tile_Layer) return String
   is (if This.Name /= null then This.Name.all else "");

   --------
   -- Id --
   --------

   function Id (This : Tile_Layer) return Tile_Layer_Id
   is (This.Id);

   -----------
   -- Width --
   -----------

   function Width (This : Tile_Layer) return Natural
   is (This.Width);

   ------------
   -- Height --
   ------------

   function Height (This : Tile_Layer) return Natural
   is (This.Height);

   ----------
   -- Tile --
   ----------

   function Tile
     (This : Tile_Layer;
      X, Y : Natural)
      return TCG.Tilesets.Map_Tile_Id
   is (This.Map (X, Y));

   ---------
   -- Put --
   ---------

   procedure Put (This : Tile_Layer) is
   begin
      Put_Line ("Layer: " & Name (This) & " Id:" & This.Id'Img);
      for Y in This.Map'Range (2) loop
         for X in This.Map'Range (1) loop
            Put (This.Map (X, Y)'Img &
                 (if X = This.Map'Last (1) then "," else ", "));
         end loop;
         New_Line;
      end loop;
   end Put;

end TCG.Tile_Layers;
