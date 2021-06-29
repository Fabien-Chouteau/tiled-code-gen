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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
use Ada.Containers;

with Interfaces; use Interfaces;

package body TCG.Palette is

   function ID_Hashed (C : ARGB_Color) return Hash_Type;

   package Color_Hashmap is new Ada.Containers.Hashed_Maps
     (Key_Type     => ARGB_Color,
      Element_Type => Color_Id,
      Hash         => ID_Hashed,
      Equivalent_Keys => "=");
   use Color_Hashmap;

   package Color_Id_Vector is new Ada.Containers.Vectors
     (Index_Type   => Color_Id,
      Element_Type => ARGB_Color);
   use Color_Id_Vector;

   Convert_Vect : Color_Id_Vector.Vector;

   Hashmap : Color_Hashmap.Map;

   Transparent_Color    : ARGB_Color;
   Transparent_Color_Id : Color_Id;
   Transparent_Has_Definition : Boolean := False;

   type String_Access is access all String;
   Format_Strings : constant array (Output_Color_Format) of String_Access
     := (ARGB        => new String'("ARGB"),
         RGB565      => new String'("RGB565"),
         RGB565_Swap => new String'("RGB565_Swap"),
         RGB555      => new String'("RGB555"),
         RGB888      => new String'("RGB888"));

   ---------------
   -- ID_Hashed --
   ---------------

   function ID_Hashed (C : ARGB_Color) return Hash_Type is
   begin
      return Hash_Type'Val (C.A + C.R + C.G + C.B);
   end ID_Hashed;

   ---------------
   -- Add_Color --
   ---------------

   function Add_Color (C : ARGB_Color) return Color_Id is
      Id : Color_Id;
   begin
      if In_Palette (C) then
         Id := Element (Hashmap.Find (C));
      else
         Convert_Vect.Append (C);
         Id := Convert_Vect.Last_Index;
         Hashmap.Insert (C, Id);
      end if;
      return Id;
   end Add_Color;

   -------------
   -- Convert --
   -------------

   function Convert (Id : Color_Id) return ARGB_Color
   is (Convert_Vect.Element (Id));

   ----------------
   -- In_Palette --
   ----------------

   function In_Palette (C : ARGB_Color) return Boolean
   is (Hashmap.Contains (C));

   -------------------------
   -- Transparent_Defined --
   -------------------------

   function Transparent_Defined return Boolean
   is (Transparent_Has_Definition);

   -----------------
   -- Transparent --
   -----------------

   function Transparent return Color_Id
     is (Transparent_Color_Id);

   function Transparent return ARGB_Color
   is (Transparent_Color);

   ---------------------
   -- Set_Transparent --
   ---------------------

   procedure Set_Transparent (C : ARGB_Color) is
   begin
      if Transparent_Has_Definition and then Transparent_Color /= C then
         raise Program_Error with
           "Incompatible new definition of transparent color";
      end if;

      --  Transparent_Color_Id := Add_Color (C);
      Convert_Vect.Replace_Element (Transparent_Color_Id, C);
      Hashmap.Insert (C, Transparent_Color_Id);
      Transparent_Color := C;
      Transparent_Has_Definition := True;
   end Set_Transparent;

   ----------------------
   -- Number_Of_Colors --
   ----------------------

   function Number_Of_Colors return Natural
   is (Natural (Convert_Vect.Length));

   --------------
   -- First_Id --
   --------------

   function First_Id return Color_Id
   is (Convert_Vect.First_Index);

   -------------
   -- Last_Id --
   -------------

   function Last_Id return Color_Id
   is (Convert_Vect.Last_Index);

   -----------
   -- Image --
   -----------

   function Image (Id : Color_Id) return String
   is (Id'Img);

   -------------
   -- To_ARGB --
   -------------

   function To_ARGB (Str : String) return ARGB_Color is
      R : constant String := Str (Str'First .. Str'First + 1);
      G : constant String := Str (Str'First + 2 .. Str'First + 3);
      B : constant String := Str (Str'First + 4 .. Str'First + 5);
   begin
      return (A => 255,
              R => Component'Value ("16#" & R & "#"),
              G => Component'Value ("16#" & G & "#"),
              B => Component'Value ("16#" & B & "#"));
   end To_ARGB;

   ---------------
   -- To_RGB565 --
   ---------------

   function To_RGB565 (C : ARGB_Color) return Unsigned_16 is
      R : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (C.R), 3) and 16#1F#;
      G : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (C.G), 2) and 16#3F#;
      B : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (C.B), 3) and 16#1F#;
   begin
      return (Shift_Left (R, 11) or Shift_Left (G, 5) or B);
   end To_RGB565;

   --------------------
   -- To_RGB565_Swap --
   --------------------

   function To_RGB565_Swap (C : ARGB_Color) return Unsigned_16 is
      RGB : constant Unsigned_16 := To_RGB565 (C);
   begin
      return Shift_Right (RGB and 16#FF00#, 8) or
        (Shift_Left (RGB, 8) and 16#FF00#);
   end To_RGB565_Swap;

   ---------------
   -- To_RGB888 --
   ---------------

   function To_RGB888 (C : ARGB_Color) return Unsigned_32 is
      R : constant Unsigned_32 := Shift_Left (Unsigned_32 (C.R), 16);
      G : constant Unsigned_32 := Shift_Left (Unsigned_32 (C.G), 8);
      B : constant Unsigned_32 := Shift_Left (Unsigned_32 (C.B), 0);
   begin
      return R or G or B;
   end To_RGB888;

   ---------------
   -- To_RGB555 --
   ---------------

   function To_RGB555 (C : ARGB_Color) return Unsigned_16 is
      R : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (C.R), 3) and 16#1F#;
      G : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (C.G), 3) and 16#1F#;
      B : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (C.B), 3) and 16#1F#;
   begin
      return (Shift_Left (B, 10) or Shift_Left (G, 5) or R);
   end To_RGB555;

   -----------
   -- Image --
   -----------

   function Image (C      : ARGB_Color;
                   Format : Output_Color_Format)
                   return String
   is
   begin
      case Format is
         when ARGB =>
            return Image (C);
         when RGB565 =>
            return To_RGB565 (C)'Img;
         when RGB565_Swap =>
            return To_RGB565_Swap (C)'Img;
         when RGB555 =>
            return To_RGB555 (C)'Img;
         when RGB888 =>
            return To_RGB888 (C)'Img;
      end case;
   end Image;

   ----------------------
   -- Format_Supported --
   ----------------------

   function Format_Supported (Fmt : String)
                              return Boolean
   is (for some Str of Format_Strings => Str.all = Fmt);

   -----------------------
   -- Supported_Formats --
   -----------------------

   function Supported_Formats return String
   is (Format_Strings (ARGB).all & ", " &
         Format_Strings (RGB565).all & ", " &
         Format_Strings (RGB565_Swap).all);

   -------------
   -- Convert --
   -------------

   function Convert (Fmt : String) return Output_Color_Format
   is
   begin
      for Format in Output_Color_Format loop
         if Fmt = Format_Strings (Format).all then
            return Format;
         end if;
      end loop;
      raise Program_Error with "Invalid output color format: " & Fmt;
   end Convert;

   ---------
   -- Put --
   ---------

   procedure Put is
   begin
      Put_Line ("Palette ->");
      Put_Line ("Number_Of_Colors: " & Length (Hashmap)'Img);
      Put_Line ("Transparent color: " & Image (Transparent_Color));
      for Cur in Convert_Vect.Iterate loop
         Put_Line (To_Index (Cur)'Img & " => " & Image (Element (Cur)));
      end loop;
   end Put;

   -----------
   -- Image --
   -----------

   function Image (C : ARGB_Color) return String
   is ("(" & C.A'Img & "," & C.R'Img & "," &
         C.G'Img & "," & C.B'Img & ")");

begin

   Transparent_Color_Id := Add_Color ((0, 0, 0, 0));
   if Transparent_Color_Id /= 0 then
      raise Program_Error with "Transparent_Color_Id should be zero";
   end if;
end TCG.Palette;
