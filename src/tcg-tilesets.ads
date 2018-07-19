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

with TCG.Palette;

private with Ada.Containers;
private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with TCG.Collision_Objects;

package TCG.Tilesets is

   --  We can load multiple maps, each map can load multiple tile sets and
   --  potetially the same tile sets will be loaded by different maps.
   --
   --  To simplify the generated data, we build a unique master tile set that
   --  contains all the tiles from all the tile sets of all the maps. The tiles
   --  from the master tile set are indentifiyed with the Master_Tile_Id type.
   --
   --  During loading of the maps we have to be able to get the Master_Tile_Id
   --  of a tile from a specific tile set. For this the function Convert will
   --  return the Master_Tile_Id from the Tileset_Id and the Local_Tile_Id.

   type Master_Tile_Id is new Natural;
   --  Id of a tile in the unified master tile set

   No_Tile : constant Master_Tile_Id := 0;

   type Local_Tile_Id is new Natural;
   --  Id of a tile within a given tile set

   type Map_Tile_Id is new Natural;
   --  Id of a tile within a the layers of a map

   type Tileset_Id is private;
   --  Id of one of the loaded tile sets

   Invalid_Tileset : constant Tileset_Id;

   function Load (Path : String) return Tileset_Id;
   --  Load a tile set in the master tile set and return its Id. An exception
   --  will be raised of the the tile size is different than the previously
   --  loaded tile set(s).

   function Name (Id : Tileset_Id) return String
     with Pre => Id /= Invalid_Tileset;

   function Convert (Id  : Tileset_Id;
                     Loc : Local_Tile_Id)
                     return Master_Tile_Id
     with Pre => Id /= Invalid_Tileset;
   --  Move a tile into the Master tileset and return its id

   function Number_Of_Tiles return Natural;
   --  Number of tiles in the master set

   function First_Id return Master_Tile_Id;
   --  Return the first valid Master_Tile_Id

   function Last_Id return Master_Tile_Id;
   --  Return the last valid Master_Tile_Id

   function Tile_Width return  Positive;
   --  Width of tiles in the master set. Returns 0 if not tile set are loaded.

   function Tile_Height return Positive;
   --  Height of tiles in the master set. Returns 0 if not tile set are loaded.

   function Pix (T    : Master_Tile_Id;
                 X, Y : Positive)
                 return Palette.ARGB_Color
     with Pre => T /= No_Tile
     and then X <= Tile_Width
     and then Y <= Tile_Height;

   function Pix (T    : Master_Tile_Id;
                 X, Y : Positive)
                 return Palette.Color_Id
     with Pre => T /= No_Tile
     and then X <= Tile_Width
     and then Y <= Tile_Height;

   function Has_Collision (T : Master_Tile_Id)
                           return Boolean
     with Pre => T /= No_Tile;

   function Collision (T    : Master_Tile_Id;
                       X, Y : Positive)
                       return Boolean
     with Pre => T /= No_Tile
     and then X <= Tile_Width
     and then Y <= Tile_Height;

   procedure Fill_Master_Tileset (T : Tileset_Id);
   --  Fill the master tileset with all the tiles of this tileset

private

   type String_Access is access all String;

   type Tile_Pix is array (Positive range <>,
                           Positive range <>) of
     TCG.Palette.ARGB_Color;

   type Tile_Data (Width, Height : Positive) is record
      Pixels     : Tile_Pix (1 .. Width, 1 .. Height);
      Collisions : TCG.Collision_Objects.Collisions;
   end record;

   type Tile_Data_Acc is access all Tile_Data;

   package Tile_Data_Vect is new Ada.Containers.Vectors
     (Index_Type   => Local_Tile_Id,
      Element_Type => Tile_Data_Acc);

   type Local_Tileset is record
      Name              : String_Access := null;
      Source            : String_Access := null;
      Path              : String_Access := null;
      Tiles             : Tile_Data_Vect.Vector;
      Columns           : Natural := 0;
      Number_Of_Tiles   : Natural := 0;
   end record;

   package Tileset_Vect is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Local_Tileset);

   type Tileset_Id is new Tileset_Vect.Extended_Index;
   Invalid_Tileset : constant Tileset_Id := Tileset_Id (Tileset_Vect.No_Index);

   Local_Tilesets : Tileset_Vect.Vector;

   function Already_Loaded (Path : String) return Tileset_Id;

   type Tile_Id_Rec is record
      TS : Tileset_Id;
      Id : Local_Tile_Id;
   end record;

   use type Ada.Containers.Hash_Type;

   function Hash (Id : Tile_Id_Rec)
                  return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Id.TS)
        xor
       Ada.Containers.Hash_Type (Id.Id));

   function Equivalent_Keys (A, B : Tile_Id_Rec)
                             return Boolean
   is (A.TS = B.TS and then A.Id = B.Id);

   package Master_Tile_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Tile_Id_Rec,
      Element_Type    => Master_Tile_Id,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   package Master_Tile_Vect is new Ada.Containers.Vectors
     (Index_Type   => Master_Tile_Id,
      Element_Type => Tile_Data_Acc);

   Master_Tileset : Master_Tile_Map.Map;
   Master_Tilevect : Master_Tile_Vect.Vector;
   Last_Master_Id : Master_Tile_Id := 0;
   M_Tile_Width  : Natural := 0;
   M_Tile_Height : Natural := 0;

end TCG.Tilesets;
