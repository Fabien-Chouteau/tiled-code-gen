with Ada.Strings.Fixed;

with TCG.Utils; use TCG.Utils;

with DOM.Core;          use DOM.Core;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Core.Nodes;    use DOM.Core.Nodes;

package body TCG.Collision_Objects is

   type Polygon_Access is access all Polygon;

   function To_Float (Str : String) return Float;
   function To_Point (Str : String) return Point;
   function To_Polygon (Offset : Point;
                        Str    : String)
                        return not null Polygon_Access;

   function Create (N : Node) return Collision_Shape;
   function Inside_Ellipse (Pt      : Point;
                            Ellipse : Polygon)
                            return Boolean
     with Pre => Ellipse'Length = 4 and then Ellipse'First = 1;

   --------------
   -- To_Float --
   --------------

   function To_Float (Str : String) return Float
   is (Float'Value (Str));

   --------------
   -- To_Point --
   --------------

   function To_Point (Str : String) return Point is
      Index : constant Natural := Ada.Strings.Fixed.Index (Str, ",");
   begin
      return (To_Float (Str (Str'First .. Index - 1)),
              To_Float (Str (Index + 1 .. Str'Last)));
   end To_Point;

   ----------------
   -- To_Polygon --
   ----------------

   function To_Polygon (Offset : Point;
                        Str    : String)
                        return not null Polygon_Access
   is
      Number_Of_Points : constant Natural :=
        Ada.Strings.Fixed.Count (Str, " ") + 1;

      Ret : constant not null Polygon_Access
        := new Polygon (1 .. Number_Of_Points);

      Index : Natural;
      Last_Index : Natural := Str'First;
   begin

      for Pt of Ret.all loop
         Index := Ada.Strings.Fixed.Index (Str (Last_Index .. Str'Last), " ");

         if Index = 0 then
            --  Last point in the list
            Pt := To_Point (Str (Last_Index .. Str'Last));
         else
            Pt := To_Point (Str (Last_Index .. Index - 1));
         end if;
         Pt.X := Pt.X + Offset.X;
         Pt.Y := Pt.Y + Offset.Y;
         Last_Index := Index + 1;
      end loop;

      return Ret;
   end To_Polygon;

   ------------
   -- Create --
   ------------

   function Create (N : Node) return Collision_Shape is
      X : constant Float := To_Float (Item_As_String (N, "x"));
      Y : constant Float := To_Float (Item_As_String (N, "y"));
      Has_Width  : constant Boolean := Item_Exists (N, "width");
      Has_Height : constant Boolean := Item_Exists (N, "height");

      Height, Width : Float;
      Rect : Polygon (1 .. 4);
      Poly : access Polygon;
      List : Node_List;
   begin
      if Has_Width or else Has_Height then
         Width  := To_Float (Item_As_String (N, "width"));
         Height := To_Float (Item_As_String (N, "height"));

         Rect := ((X,         Y),
                  (X + Width, Y),
                  (X + Width, Y + Height),
                  (X,         Y + Height));

         List := Get_Elements_By_Tag_Name (N, "ellipse");
         if Length (List) /= 0 then
            Free (List);
            return (Ellipse_Shape, Rect);
         else
            Free (List);
            return (Rectangle_Shape, Rect);
         end if;

      else
         List := Get_Elements_By_Tag_Name (N, "polygon");

         if Length (List) /= 1 then
            raise Program_Error with "Invalid number of polygon elements";
         end if;
         Poly := To_Polygon ((X, Y),
                             Item_As_String (Item (List, 0), "points"));
         Free (List);

         return (Polygon_Shape, Poly);
      end if;
   end Create;

   -------------------
   -- Has_Collision --
   -------------------

   function Has_Collision (This : Collisions)
                           return Boolean
   is (not This.List.Is_Empty);

   ------------
   -- Create --
   ------------

   procedure Load (This : in out Collisions;
                   N    : DOM.Core.Node)
   is
      List   : Node_List;
   begin
      List := Elements.Get_Elements_By_Tag_Name (N, "object");
      for Index in 1 .. Length (List) loop
         This.List.Append (Create (Item (List, Index - 1)));
      end loop;
      Free (List);
   end Load;

   --------------------
   -- Inside_Ellipse --
   --------------------

   function Inside_Ellipse (Pt      : Point;
                            Ellipse : Polygon)
                            return Boolean
   is
      Diag_1 : constant Geometry.Line := To_Line (Ellipse (1), Ellipse (3));
      Diag_2 : constant Geometry.Line := To_Line (Ellipse (2), Ellipse (4));

      Center : constant Point := Intersection (Diag_1, Diag_2);

      H_Axis : constant Vector := To_Vector ((Ellipse (1), Ellipse (2)));
      V_Axis : constant Vector := To_Vector ((Ellipse (2), Ellipse (3)));

      H_Semi : constant Distance_Type := Length (H_Axis) / 2.0;
      V_Semi : constant Distance_Type := Length (V_Axis) / 2.0;
   begin
      return (
              ((Pt.X - Center.X)**2 / H_Semi**2)
              +
              ((Pt.Y - Center.Y)**2 / V_Semi**2)
             ) <= 1.0;
   end Inside_Ellipse;

   -------------
   -- Collide --
   -------------

   function Collide
     (This : Collisions;
      X, Y : Float)
      return Boolean
   is
      function Fixed_Inside (P : Point; Poly : Polygon) return Boolean;

      function Fixed_Inside (P : Point; Poly : Polygon) return Boolean is
         J : Natural := Poly'Last;
         C : Boolean := False;
         Deltay  : Float;
      begin
         --  See http://www.ecse.rpi.edu/Homepages/wrf/Research
         --     /Short_Notes/pnpoly.html
         for S in Poly'Range loop
            Deltay := P.Y - Poly (S).Y;

            --  The divide below is mandatory: if you transform it into a
            --  multiplication on the other side, the sign of the denominator
            --  will flip the inequality, and thus make the code harder.
            if ((0.0 <= Deltay and then P.Y < Poly (J).Y)
                or else (Poly (J).Y <= P.Y and then Deltay < 0.0))
              and then
                (P.X - Poly (S).X < (Poly (J).X - Poly (S).X) * Deltay
                 / (Poly (J).Y - Poly (S).Y))
            then
               C := not C;
            end if;
            J := S;
         end loop;
         return C;
      end Fixed_Inside;

   begin
      for Shape of This.List loop
         case Shape.Kind is
            when Rectangle_Shape =>
               if Fixed_Inside ((X, Y), Shape.Rect) then
                  return True;
               end if;
            when Ellipse_Shape =>
               if Inside_Ellipse ((X, Y), Shape.Rect) then
                  return True;
               end if;
            when Polygon_Shape =>
               if Fixed_Inside ((X, Y), Shape.Poly.all) then
                  return True;
               end if;
         end case;
      end loop;
      return False;
   end Collide;

end TCG.Collision_Objects;
