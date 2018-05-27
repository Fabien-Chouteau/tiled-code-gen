with GESTE;
with GESTE.Grid;
pragma Style_Checks (Off);
package Game_Assets.map_b is

   --  map_b
   Width       : constant := 20;
   Height      : constant := 10;
   Tile_Width  : constant := 16;
   Tile_Height : constant := 16;

   --  Tile Layer 1
   package Tile_Layer_1 is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 0, 18, 1, 7),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 2, 8),
         ( 0, 0, 0, 0, 0, 0, 2, 8, 2, 8),
         ( 0, 0, 0, 0, 11, 0, 0, 0, 2, 8),
         ( 0, 0, 0, 0, 2, 8, 0, 0, 2, 8),
         ( 0, 0, 0, 0, 2, 8, 2, 8, 8, 8),
         ( 0, 0, 0, 0, 2, 8, 8, 77, 77, 8),
         ( 0, 0, 0, 0, 2, 8, 77, 77, 77, 8),
         ( 0, 0, 0, 0, 2, 77, 77, 77, 77, 8),
         ( 0, 0, 0, 0, 2, 77, 77, 77, 77, 8),
         ( 0, 0, 0, 0, 2, 77, 77, 77, 77, 77),
         ( 0, 0, 0, 0, 77, 77, 77, 77, 77, 77),
         ( 0, 0, 0, 0, 77, 77, 77, 77, 77, 77),
         ( 0, 0, 0, 0, 77, 77, 77, 77, 77, 77),
         ( 0, 0, 0, 0, 77, 77, 77, 77, 77, 77),
         ( 0, 0, 0, 0, 77, 77, 77, 77, 77, 77),
         ( 0, 0, 0, 0, 2, 8, 77, 77, 77, 8),
         ( 0, 0, 0, 0, 2, 8, 8, 77, 77, 8),
         ( 0, 0, 0, 0, 2, 8, 8, 8, 77, 8),
         ( 0, 0, 0, 0, 2, 8, 8, 8, 8, 8))      ;
   end Tile_Layer_1;

   --  Tile Layer 2
   package Tile_Layer_2 is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 11, 0, 0, 0),
         ( 0, 40, 0, 0, 0, 72, 78, 78, 0, 0),
         ( 0, 41, 0, 0, 0, 0, 2, 8, 0, 0),
         ( 0, 42, 0, 72, 78, 78, 2, 8, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 12, 0),
         ( 0, 0, 0, 0, 0, 0, 12, 9, 9, 0),
         ( 0, 0, 0, 0, 0, 0, 6, 0, 36, 0),
         ( 0, 0, 0, 0, 0, 14, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 14, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 14, 0, 0, 82, 9),
         ( 0, 0, 0, 79, 3, 15, 0, 0, 0, 55),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 44, 56),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 44, 57),
         ( 47, 0, 0, 0, 0, 0, 0, 0, 0, 58),
         ( 48, 0, 0, 0, 1, 13, 0, 0, 0, 7),
         ( 0, 0, 0, 0, 0, 0, 5, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 5, 24, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 7, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))      ;
   end Tile_Layer_2;

   package Object_Layer_1 is
      Objects : Object_Array :=
        (
           0 => (
            Kind => RECTANGLE_OBJ,
            Id   =>  1,
            Name => null,
            X    =>  1.29333E+02,
            Y    =>  7.00000E+00,
            Width =>  4.23333E+01,
            Height =>  1.96667E+01,
            Tile_Id =>  0,
            Str => null
          ),
           1 => (
            Kind => POINT_OBJ,
            Id   =>  2,
            Name => null,
            X    =>  1.01667E+02,
            Y    =>  1.53333E+01,
            Width =>  0.00000E+00,
            Height =>  0.00000E+00,
            Tile_Id =>  0,
            Str => null
          ),
           2 => (
            Kind => POLYGON_OBJ,
            Id   =>  3,
            Name => null,
            X    =>  2.71667E+02,
            Y    =>  8.66667E+00,
            Width =>  0.00000E+00,
            Height =>  0.00000E+00,
            Tile_Id =>  0,
            Str => null
          ),
           3 => (
            Kind => TILE_OBJ,
            Id   =>  4,
            Name => null,
            X    =>  1.85000E+02,
            Y    =>  2.33333E+01,
            Width =>  1.60000E+01,
            Height =>  1.60000E+01,
            Tile_Id =>  80,
            Str => null
          ),
           4 => (
            Kind => TEXT_OBJ,
            Id   =>  6,
            Name => null,
            X    =>  8.28125E+01,
            Y    =>  3.18333E+01,
            Width =>  9.23750E+01,
            Height =>  1.90000E+01,
            Tile_Id =>  0,
            Str => new String'("Hello World")
          )
        );
   end Object_Layer_1;
end Game_Assets.map_b;
