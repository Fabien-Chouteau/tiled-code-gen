with GESTE;

pragma Style_Checks (Off);
package Game_Assets is

   type Color_Index is range  0 ..  20;

   type Output_Color is mod 2**16
     with Size => 16;

   Transparent : constant Output_Color :=  391;

   Tile_Size : constant :=  16;

   type Tile_Index is range 0 .. 82;

   package Engine is new GESTE
     (Output_Color => Output_Color,
      Color_Index  => Color_Index,
      Tile_Index   => Tile_Index,
      Tile_Size    => Tile_Size,
      No_Tile      => Tile_Index'First,
      Transparent  => Transparent);


   Palette : aliased Engine.Palette_Type := (
      0 =>  391,
      1 =>  59147,
      2 =>  22089,
      3 =>  41834,
      4 =>  58727,
      5 =>  52303,
      6 =>  4907,
      7 =>  39694,
      8 =>  35372,
      9 =>  16847,
      10 =>  17208,
      11 =>  14856,
      12 =>  21228,
      13 =>  29681,
      14 =>  63423,
      15 =>  42326,
      16 =>  57613,
      17 =>  11414,
      18 =>  65535,
      19 =>  46486,
      20 =>  0);
end Game_Assets;
