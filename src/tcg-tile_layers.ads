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

with TCG.Tilesets;
with DOM.Core;

package TCG.Tile_Layers is

   type Tile_Layer_Id is new Integer;

   type Tile_Layer is private;
   No_Layer : constant Tile_Layer;

   function Load (Root : DOM.Core.Node) return Tile_Layer;

   function Name (This : Tile_Layer) return String
     with Pre => This /= No_Layer;

   function Id (This : Tile_Layer) return Tile_Layer_Id
     with Pre => This /= No_Layer;

   function Width (This : Tile_Layer) return Natural
     with Pre => This /= No_Layer;

   function Height (This : Tile_Layer) return Natural
     with Pre => This /= No_Layer;

   function Tile (This : Tile_Layer;
                  X, Y : Natural)
                  return TCG.Tilesets.Map_Tile_Id
     with Pre => This /= No_Layer
     and then X in 1 .. Width (This)
     and then Y in 1 .. Height (This);

   procedure Put (This : Tile_Layer)
     with Pre => This /= No_Layer;
private

   type String_Access is access all String;

   type Layer_Tile_Map is array (Natural range <>, Natural range <>)
     of Tilesets.Map_Tile_Id;

   type Layer_Data (Width, Height : Natural) is record
      Name : String_Access := null;
      Id   : Tile_Layer_Id;
      Map  : Layer_Tile_Map (1 .. Width, 1 .. Height);
   end record;

   type Tile_Layer is access all Layer_Data;
   No_Layer : constant Tile_Layer := null;

end TCG.Tile_Layers;
