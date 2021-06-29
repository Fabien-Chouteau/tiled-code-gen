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

with TCG.Tile_Layers;
with TCG.Object_Groups;
with TCG.Tilesets;

private with Ada.Containers.Vectors;

package TCG.Maps is

   type Map is private;

   No_Map : constant Map;

   function Load (Path : String; Name : String) return Map;

   function Name (This : Map) return String
     with Pre => This /= No_Map;

   function Width (This : Map) return Natural
     with Pre => This /= No_Map;

   function Height (This : Map) return Natural
     with Pre => This /= No_Map;

   function Tile_Width (This : Map) return Natural
     with Pre => This /= No_Map;

   function Tile_Height (This : Map) return Natural
     with Pre => This /= No_Map;

   procedure Put (This : Map)
     with Pre => This /= No_Map;

   function Number_Of_Layers (This : Map) return Natural
     with Pre => This /= No_Map;

   function First_Layer (This : Map) return Natural
     with Pre => This /= No_Map;

   function Last_Layer (This : Map) return Natural
     with Pre => This /= No_Map;

   function Layer (This : Map; Index : Natural) return Tile_Layers.Tile_Layer
     with Pre => This /= No_Map;

   function Master_Tile (M  : Map;
                         Id : Tilesets.Map_Tile_Id)
                         return Tilesets.Master_Tile_Id;
   --  Return the Master_Tile_Id of the local map tile

   function First_Object_Group (This : Map) return Natural
     with Pre => This /= No_Map;

   function Last_Object_Group (This : Map) return Natural
     with Pre => This /= No_Map;

   function Object_Group (This : Map; Index : Natural)
                          return Object_Groups.Object_Group
     with Pre => This /= No_Map;

   procedure Generate_GESTE_Source (M            : Map;
                                    Package_Name : String;
                                    Filepath     : String);

   procedure Generate_LibGBA_Source (M            : Map;
                                     Package_Name : String;
                                     Filepath     : String);

   procedure Generate_RSTE_Source (M        : Map;
                                   Filepath : String);

   procedure Fill_Master_Tileset (M : Map);
   --  Fill the master tileset with all the tiles and only the tiles used in
   --  the maps of the list.

private

   type String_Access is access all String;

   type Map_Tileset is record
      Id          : Tilesets.Tileset_Id;
      First_Tile  : Tilesets.Map_Tile_Id;
   end record;

   package Tileset_Vect is new Ada.Containers.Vectors
     (Natural, Map_Tileset);

   package Layer_Vect is new Ada.Containers.Vectors
     (Natural, TCG.Tile_Layers.Tile_Layer, TCG.Tile_Layers."=");

   package Object_Group_Vect is new Ada.Containers.Vectors
     (Natural, TCG.Object_Groups.Object_Group, TCG.Object_Groups."=");

   type Map_Data is record
      Tileset_List   : Tileset_Vect.Vector;
      Layer_List     : Layer_Vect.Vector;
      Obj_Group_List : Object_Group_Vect.Vector;
      Name           : String_Access;
      Width          : Natural;
      Height         : Natural;
      Tile_Width     : Natural;
      Tile_Height    : Natural;
   end record;

   type Map is access all Map_Data;

   No_Map : constant Map := null;

end TCG.Maps;
