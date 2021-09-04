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

with DOM.Core;          use DOM.Core;
with DOM.Core.Nodes;    use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;

with TCG.Utils; use TCG.Utils;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;

package body TCG.Object_Groups is

   function To_Float (Str : String) return Float;
   function To_Point (Str : String) return Point;
   function To_Polygon (Str    : String)
                        return not null Polygon_Access;
   function Load (N : Node) return Object;

   --------------
   -- To_Float --
   --------------

   function To_Float (Str : String) return Float
   is (Float'Value (Str));

   --------------
   -- To_Point --
   --------------

   function To_Point (Str : String) return Point is
      Index : constant Natural := Ada.Strings.Fixed.Index (Str, ",");
   begin
      return (To_Float (Str (Str'First .. Index - 1)),
              To_Float (Str (Index + 1 .. Str'Last)));
   end To_Point;

   ----------------
   -- To_Polygon --
   ----------------

   function To_Polygon (Str    : String)
                        return not null Polygon_Access
   is
      Number_Of_Points : constant Natural :=
        Ada.Strings.Fixed.Count (Str, " ") + 1;

      Ret : constant not null Polygon_Access
        := new Polygon (1 .. Number_Of_Points);

      Index : Natural;
      Last_Index : Natural := Str'First;
   begin

      for Pt of Ret.all loop
         Index := Ada.Strings.Fixed.Index (Str (Last_Index .. Str'Last), " ");

         if Index = 0 then
            --  Last point in the list
            Pt := To_Point (Str (Last_Index .. Str'Last));
         else
            Pt := To_Point (Str (Last_Index .. Index - 1));
         end if;
         Last_Index := Index + 1;
      end loop;

      return Ret;
   end To_Polygon;

   ----------
   -- Load --
   ----------

   function Load (N : Node) return Object is
      Id              : Natural;
      X               : constant Float := Item_As_Float (N, "x");
      Y               : constant Float := Item_As_Float (N, "y");
      Has_Width       : constant Boolean := Item_Exists (N, "width");
      Has_Height      : constant Boolean := Item_Exists (N, "height");
      Has_GID         : constant Boolean := Item_Exists (N, "gid");
      Is_Ellipse      : Boolean;
      Is_Point        : Boolean;
      Str             : String_Access := null;
      Name            : String_Access := null;
      Height, Width   : Float := 0.0;
      GID             : Unsigned_32 := 0;
      List            : Node_List;
      Kind            : Object_Kind;
      Flip_Vertical   : Boolean := False;
      Flip_Horizontal : Boolean := False;
      Poly            : Polygon_Access;
   begin
      if Item_Exists (N, "id") then
         Id := Item_As_Natural (N, "id");
      else
         --  When there is not ID it means that there is only one object
         Id := 0;
      end if;

      if Has_Width then
         Width := Item_As_Float (N, "width");
      end if;

      if Has_Height then
         Height := Item_As_Float (N, "height");
      end if;

      if Has_GID then
         GID := Item_As_UInt32 (N, "gid");

         Flip_Vertical := (GID and 16#4000_0000#) /= 0;
         Flip_Horizontal := (GID and 16#8000_0000#) /= 0;

         GID := GID and 16#3FFF_FFFF#;
      end if;

      if Item_Exists (N, "name") then
         Name := new String'(Item_As_String (N, "name"));
      end if;

      List := Get_Elements_By_Tag_Name (N, "text");
      if Length (List) = 1 then
         Str := new String'(Node_Value (First_Child (Item (List, 0))));
      end if;
      Free (List);

      List := Get_Elements_By_Tag_Name (N, "polygon");
      if Length (List) = 1 then
         Poly := To_Polygon (Item_As_String (Item (List, 0), "points"));
      end if;
      Free (List);

      List := Get_Elements_By_Tag_Name (N, "ellipse");
      if Length (List) > 0 then
         Is_Ellipse := True;
      end if;
      Free (List);

      List := Get_Elements_By_Tag_Name (N, "point");
      if Length (List) > 0 then
         Is_Point := True;
      end if;
      Free (List);

      if Poly /= null then
         Kind := Polygon_Obj;
      elsif Is_Ellipse then
         Kind := Ellipse_Obj;
      elsif Is_Point then
         Kind := Point_Obj;
      elsif Str /= null then
         Kind := Text_Obj;
      elsif Has_GID then
         Kind := Tile_Obj;
      else
         Kind := Rectangle_Obj;
      end if;

      return (Kind            => Kind,
              Name            => Name,
              Id              => Id,
              Pt              => (X, Y),
              Width           => Width,
              Height          => Height,
              Points          => Poly,
              Str             => Str,
              Flip_Vertical   => Flip_Vertical,
              Flip_Horizontal => Flip_Horizontal,
              Tile_Id         => Tilesets.Map_Tile_Id (GID));
   end Load;

   ----------
   -- Load --
   ----------

   function Load (Root : DOM.Core.Node) return Object_Group is
      Id    : Natural;
      Name  : constant String := Item_As_String (Root, "name");
      List  : Node_List;
      N     : Node;
      Group : constant Object_Group := new Group_Data;
   begin
      if Item_Exists (Root, "id") then
         Id := Item_As_Natural (Root, "id");
      else
         --  When there is not ID it means that there is only one group
         Id := 0;
      end if;

      Group.Id := Object_Group_Id (Id);
      Group.Name := new String'(Name);

      List := Get_Elements_By_Tag_Name (Root, "object");
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         Group.Objects.Append (Load (N));
      end loop;
      Free (List);
      return Group;
   end Load;

   ----------
   -- Name --
   ----------

   function Name
     (This : Object_Group)
      return String
   is (This.Name.all);

   --------
   -- Id --
   --------

   function Id
     (This : Object_Group)
      return Object_Group_Id
   is (This.Id);

   ------------
   -- Length --
   ------------

   function Length (This : Object_Group) return Natural
   is (Natural (This.Objects.Length));

   -----------------
   -- First_Index --
   -----------------

   function First_Index (This : Object_Group) return Natural
   is (This.Objects.First_Index);

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (This : Object_Group) return Natural
   is (This.Objects.Last_Index);

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (This  : Object_Group;
                        Index : Natural)
                        return Object
   is (This.Objects.Element (Index));

end TCG.Object_Groups;
