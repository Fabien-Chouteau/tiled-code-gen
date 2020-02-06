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

with DOM.Core;

with TCG.Tilesets;

private with Ada.Containers.Vectors;

package TCG.Object_Groups is

   type Object_Kind is (Rectangle_Obj, Point_Obj, Ellipse_Obj,
                        Polygon_Obj, Tile_Obj, Text_Obj);

   type Point is record
      X, Y : Float;
   end record;

   type Polygon is array (Positive range <>) of Point;
   type Polygon_Access is access all Polygon;

   type String_Access is access all String;

   type Object (Kind : Object_Kind := Rectangle_Obj)
   is record
      Name            : String_Access;
      Id              : Natural;
      Pt              : Point;
      Width, Height   : Float;
      Points          : Polygon_Access;
      Str             : String_Access;
      Flip_Vertical   : Boolean;
      Flip_Horizontal : Boolean;
      Tile_Id         : TCG.Tilesets.Map_Tile_Id;
   end record;

   type Object_Group_Id is new Integer;

   type Object_Group is private;
   No_Layer : constant Object_Group;

   function Load (Root : DOM.Core.Node) return Object_Group;

   function Name (This : Object_Group) return String
     with Pre => This /= No_Layer;

   function Id (This : Object_Group) return Object_Group_Id
     with Pre => This /= No_Layer;

   function Length (This : Object_Group) return Natural;

   function First_Index (This : Object_Group) return Natural
     with Pre => This /= No_Layer and then Length (This) /= 0;

   function Last_Index (This : Object_Group) return Natural
     with Pre => This /= No_Layer and then Length (This) /= 0;

   function Get_Object (This  : Object_Group;
                        Index : Natural)
                        return Object
     with Pre => This /= No_Layer
     and then Length (This) /= 0
     and then Index in First_Index (This) .. Last_Index (This);

private

   package Object_Vector_Package is new Ada.Containers.Vectors
     (Natural, Object);

   type Group_Data is record
      Name    : String_Access := null;
      Id      : Object_Group_Id;
      Objects : Object_Vector_Package.Vector;
   end record;

   type Object_Group is access all Group_Data;
   No_Layer : constant Object_Group := null;

end TCG.Object_Groups;
