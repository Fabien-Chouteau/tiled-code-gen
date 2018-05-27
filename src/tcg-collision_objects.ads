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

--  This package handles the collision shapes of tiles
--
--  The shapes are loaded from the tileset file in the Collisions objects and
--  users can then test if a given point is within one of the shapes.

with DOM.Core;

private with GNATCOLL.Geometry;
private with Ada.Containers.Doubly_Linked_Lists;

package TCG.Collision_Objects is

   type Collisions is limited private;

   procedure Load (This : in out Collisions;
                   N    : DOM.Core.Node);

   function Has_Collision (This : Collisions)
                           return Boolean;
   --  Return true if there is at least one collision shape

   function Collide (This : Collisions;
                     X, Y : Float)
                     return Boolean;
   --  Return True if this coordinate is in one of the collision shapes

private

   package Geometry is new GNATCOLL.Geometry (Float);
   use Geometry;

   type Shape_Kind is (Rectangle_Shape, Ellipse_Shape, Polygon_Shape);

   type Collision_Shape (Kind : Shape_Kind := Rectangle_Shape) is record
      case Kind is
         when Rectangle_Shape | Ellipse_Shape =>
            Rect : Polygon (1 .. 4);
         when Polygon_Shape =>
            Poly : not null access Polygon;
      end case;
   end record;

   package Shape_List_Pck is new Ada.Containers.Doubly_Linked_Lists
     (Collision_Shape);

   type Collisions is limited record
      List : Shape_List_Pck.List;
   end record;

end TCG.Collision_Objects;
