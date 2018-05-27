with GESTE;
with GESTE.Grid;
pragma Style_Checks (Off);
package Game_Assets.map_a is

   --  map_a
   Width       : constant := 10;
   Height      : constant := 10;
   Tile_Width  : constant := 16;
   Tile_Height : constant := 16;

   --  Tile Layer 1
   package Tile_Layer_1 is
      Width  : constant :=  10;
      Height : constant :=  10;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 11, 18, 1, 7),
         ( 0, 0, 0, 0, 0, 72, 2, 8, 8, 8),
         ( 0, 0, 0, 0, 0, 0, 2, 8, 8, 8),
         ( 0, 0, 0, 0, 0, 0, 2, 8, 8, 8),
         ( 0, 0, 0, 0, 0, 0, 3, 9, 9, 8),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 31, 8),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 32, 8),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 33, 8),
         ( 0, 0, 0, 0, 0, 37, 1, 7, 7, 8),
         ( 0, 0, 0, 0, 0, 38, 2, 8, 8, 8))      ;
   end Tile_Layer_1;

   --  Tile Layer 2
   package Tile_Layer_2 is
      Width  : constant :=  10;
      Height : constant :=  10;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 40, 0, 0, 0, 0, 78, 78, 0, 0),
         ( 0, 41, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 42, 0, 0, 0, 24, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 47, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 48, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))      ;
   end Tile_Layer_2;

end Game_Assets.map_a;
