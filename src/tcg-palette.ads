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

with Interfaces;

package TCG.Palette is

   subtype Component is Interfaces.Unsigned_8;

   type ARGB_Color is record
      A, R, G, B : Component;
   end record;

   type Color_Id is new Natural;

   type Output_Color_Format is (ARGB, RGB565, RGB565_Swap, RGB555, RGB888);

   function Add_Color (C : ARGB_Color) return Color_Id;

   function Convert (Id : Color_Id) return ARGB_Color;

   function In_Palette (C : ARGB_Color) return Boolean;
   --  Return True is the given color is already definied in the palette

   function Transparent_Defined return Boolean;
   --  Return True is the the transparent collor is alreadty defined

   function Transparent return Color_Id
     with Pre => Transparent_Defined;
   --  Return the color defined as transparent

   function Transparent return ARGB_Color
     with Pre => Transparent_Defined;
   --  Return the color defined as transparent

   procedure Set_Transparent (C : ARGB_Color)
     with Pre  => (not Transparent_Defined or else Transparent = C)
                    and then not In_Palette (C),
          Post => Transparent_Defined and then Transparent = C;
   --  Set the color that will be treated as transparent.

   function Number_Of_Colors return Natural;

   function First_Id return Color_Id;
   --  Return the first valid Color_Id

   function Last_Id return Color_Id;
   --  Return the last valid Color_Id

   function Image (Id : Color_Id) return String;
   --  Return a string representing a color id (not the color itself)

   function Image (C      : ARGB_Color;
                   Format : Output_Color_Format)
                   return String;
   --  Return a string representing the color using the given format

   function Format_Supported (Fmt : String) return Boolean;
   --  Return True if the Fmt string represent a supported color format

   function Supported_Formats return String;
   --  Return a String that contains the names of all supported color formats

   function Convert (Fmt : String) return Output_Color_Format
     with Pre => Format_Supported (Fmt);
   --  Return the Output_Color_Format designated by the Fmt string

   function To_ARGB (Str : String) return ARGB_Color
     with Pre => Str'Length = 6;
   --  Convert and 6 hexadecimal characters string to ARGB_Color

   procedure Put;

private

   function Image (C : ARGB_Color) return String;
   function To_RGB565 (C : ARGB_Color) return Interfaces.Unsigned_16;
   function To_RGB565_Swap (C : ARGB_Color) return Interfaces.Unsigned_16;
   function To_RGB555 (C : ARGB_Color) return Interfaces.Unsigned_16;
   function To_RGB888 (C : ARGB_Color) return Interfaces.Unsigned_32;

end TCG.Palette;
