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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories;

with GNAT.Case_Util;

with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;

package body TCG.Utils is

   --------------------
   -- Ada_ID_Mapping --
   --------------------

   function Ada_ID_Mapping (From : Character) return Character
   is (case From is
          when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' => From,
          when others => '_');

   --------------------------
   -- Ada_Filename_Mapping --
   --------------------------

   function Ada_Filename_Mapping (From : Character) return Character
   is (case From is
          when 'a' .. 'z' | '0' .. '9' => From,
          when 'A' .. 'Z' => GNAT.Case_Util.To_Lower (From),
          when '.' => '-',
          when others => '_');

   ---------------------
   -- Rust_ID_Mapping --
   ---------------------

   function Rust_ID_Mapping (From : Character) return Character
   is (case From is
          when 'a' .. 'z' | '0' .. '9' => From,
          when 'A' .. 'Z' => GNAT.Case_Util.To_Lower (From),
          when others => '_');

   ----------------------------
   -- Rust_Static_ID_Mapping --
   ----------------------------

   function Rust_Static_ID_Mapping (From : Character) return Character
   is (case From is
          when 'A' .. 'Z' | '0' .. '9' => From,
          when 'a' .. 'z' => GNAT.Case_Util.To_Upper (From),
          when others => '_');

   ---------------------------
   -- Rust_Filename_Mapping --
   ---------------------------

   function Rust_Filename_Mapping (From : Character) return Character
   is (case From is
          when 'a' .. 'z' | '0' .. '9' => From,
          when 'A' .. 'Z' => GNAT.Case_Util.To_Lower (From),
          when '.' => '-',
          when others => '_');

   -----------------
   -- Item_Exists --
   -----------------

   function Item_Exists (N : Node; Item : DOM_String) return Boolean
   is (Get_Named_Item (Attributes (N), Item) /= null);

   ---------------------
   -- Item_As_Natural --
   ---------------------

   function Item_As_Natural (N : Node; Item : DOM_String) return Natural
   is (Natural'Value (Value (Get_Named_Item (Attributes (N), Item))));

   --------------------
   -- Item_As_UInt32 --
   --------------------

   function Item_As_UInt32 (N : Node; Item : DOM_String) return Unsigned_32
   is (Unsigned_32'Value (Value (Get_Named_Item (Attributes (N), Item))));

   --------------------
   -- Item_As_String --
   --------------------

   function Item_As_String (N : Node; Item : DOM_String) return String
   is (Value (Get_Named_Item (Attributes (N), Item)));

   -------------------
   -- Item_As_Float --
   -------------------

   function Item_As_Float (N : Node; Item : DOM_String) return Float
   is (Float'Value (Value (Get_Named_Item (Attributes (N), Item))));

   -----------------------
   -- To_Ada_Identifier --
   -----------------------

   function To_Ada_Identifier (Str : String) return String
   is (Translate (Str, Ada_ID_Mapping'Access));

   ---------------------
   -- To_Ada_Filename --
   ---------------------

   function To_Ada_Filename (Str     : String;
                             Is_Spec : Boolean := True)
                             return String
   is (Translate (Str, Ada_Filename_Mapping'Access) &
       (if Is_Spec then ".ads" else ".adb"));

   ------------------------
   -- To_Rust_Identifier --
   ------------------------

   function To_Rust_Identifier (Str : String) return String
   is (Translate (Str, Rust_ID_Mapping'Access));

   -------------------------------
   -- To_Rust_Static_Identifier --
   -------------------------------

   function To_Rust_Static_Identifier (Str : String) return String
   is (Translate (Str, Rust_Static_ID_Mapping'Access));

   ----------------------
   -- To_Rust_Filename --
   ----------------------

   function To_Rust_Filename (Str : String) return String
   is (Translate (Str, Rust_Filename_Mapping'Access) & ".rs");

   ----------------
   -- Ensure_Dir --
   ----------------

   function Make_Dir (Dirpath : String) return Boolean is
   begin
      if not Ada.Directories.Exists (Dirpath) then
         Ada.Directories.Create_Directory (Dirpath);
      end if;
      return True;
   exception
      when others =>
         return False;
   end Make_Dir;

end TCG.Utils;
