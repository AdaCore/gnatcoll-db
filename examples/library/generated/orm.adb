------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;
pragma Warnings (On);
pragma Style_Checks (Off);

package body Orm is
   pragma Warnings (Off);
   use Sessions.Pointers;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Book_DDR, Book_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Customer_DDR, Customer_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Dvd_DDR, Dvd_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Customer'Class, Detached_Customer_Access);

   F_Books_Pages       : constant := 0;
   F_Books_Borrowed_By : constant := 1;
   F_Books_Id          : constant := 2;
   F_Books_Title       : constant := 3;
   F_Books_Author      : constant := 4;
   F_Books_Published   : constant := 5;
   Upto_Books_0 : constant Counts := ((6,6),(6,6),(6,6),(6,6));
   Alias_Books : constant Alias_Array := (-1,2,-1);
   F_Customers_Id    : constant := 0;
   F_Customers_First : constant := 1;
   F_Customers_Last  : constant := 2;
   Counts_Customers : constant Counts := ((3,3),(3,3),(3,3),(3,3));
   Alias_Customers : constant Alias_Array := (0 => -1);
   F_Dvds_Region      : constant := 0;
   F_Dvds_Borrowed_By : constant := 1;
   F_Dvds_Id          : constant := 2;
   F_Dvds_Title       : constant := 3;
   F_Dvds_Author      : constant := 4;
   F_Dvds_Published   : constant := 5;
   Upto_Dvds_0 : constant Counts := ((6,6),(6,6),(6,6),(6,6));
   Alias_Dvds : constant Alias_Array := (-1,2,-1);

   pragma Warnings (On);
   function Detach_No_Lookup
     (Self    : Book'Class;
      Session : Session_Type)
     return Detached_Book'Class;
   function Detach_No_Lookup
     (Self    : Customer'Class;
      Session : Session_Type)
     return Detached_Customer'Class;
   function Detach_No_Lookup
     (Self    : Dvd'Class;
      Session : Session_Type)
     return Detached_Dvd'Class;
   --  Same as Detach, but does not check the session cache Same as Detach,
   --  but does not check the session cache Same as Detach, but does not check
   --  the session cache

   procedure Do_Query_Books
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Customers
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Dvds
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Customer; Op2 : Customer) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Op1 : Detached_Customer;
      Op2 : Detached_Customer)
     return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Book; Op2 : Book) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Book; Op2 : Detached_Book) return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Dvd; Op2 : Dvd) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Dvd; Op2 : Detached_Dvd) return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ------------
   -- Author --
   ------------

   function Author (Self : Book) return String is
   begin
      return String_Value (Self, F_Books_Author);
   end Author;

   ------------
   -- Author --
   ------------

   function Author (Self : Detached_Book) return String is
   begin
      return To_String (Book_Data (Self.Unchecked_Get).ORM_Author);
   end Author;

   ------------
   -- Author --
   ------------

   function Author (Self : Dvd) return String is
   begin
      return String_Value (Self, F_Dvds_Author);
   end Author;

   ------------
   -- Author --
   ------------

   function Author (Self : Detached_Dvd) return String is
   begin
      return To_String (Dvd_Data (Self.Unchecked_Get).ORM_Author);
   end Author;

   -----------------
   -- Borrowed_By --
   -----------------

   function Borrowed_By (Self : Book) return Integer is
   begin
      return Integer_Value (Self, F_Books_Borrowed_By);
   end Borrowed_By;

   -----------------
   -- Borrowed_By --
   -----------------

   function Borrowed_By (Self : Detached_Book) return Integer is
   begin
      return Book_Data (Self.Unchecked_Get).ORM_Borrowed_By;
   end Borrowed_By;

   -----------------
   -- Borrowed_By --
   -----------------

   function Borrowed_By (Self : Book) return Customer'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 and then Self.Data.Follow_LJ then
         return I_Customers.Internal_Element
           (Self,
            Upto_Books_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Borrowed_By";
         end if;

         return Filter (All_Customers, Id => Self.Borrowed_By)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Borrowed_By;

   -----------------
   -- Borrowed_By --
   -----------------

   function Borrowed_By (Self : Detached_Book) return Detached_Customer'Class
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Borrowed_By = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Borrowed_By";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Borrowed_By := new Detached_Customer'Class'
           (Get_Customer (S, Id => D.ORM_Borrowed_By));
      end if;
      return D.ORM_FK_Borrowed_By.all;
   end Borrowed_By;

   -----------------
   -- Borrowed_By --
   -----------------

   function Borrowed_By (Self : Dvd) return Integer is
   begin
      return Integer_Value (Self, F_Dvds_Borrowed_By);
   end Borrowed_By;

   -----------------
   -- Borrowed_By --
   -----------------

   function Borrowed_By (Self : Detached_Dvd) return Integer is
   begin
      return Dvd_Data (Self.Unchecked_Get).ORM_Borrowed_By;
   end Borrowed_By;

   -----------------
   -- Borrowed_By --
   -----------------

   function Borrowed_By (Self : Dvd) return Customer'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 and then Self.Data.Follow_LJ then
         return I_Customers.Internal_Element
           (Self,
            Upto_Dvds_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Borrowed_By";
         end if;

         return Filter (All_Customers, Id => Self.Borrowed_By)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Borrowed_By;

   -----------------
   -- Borrowed_By --
   -----------------

   function Borrowed_By (Self : Detached_Dvd) return Detached_Customer'Class
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Borrowed_By = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Borrowed_By";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Borrowed_By := new Detached_Customer'Class'
           (Get_Customer (S, Id => D.ORM_Borrowed_By));
      end if;
      return D.ORM_FK_Borrowed_By.all;
   end Borrowed_By;

   -----------
   -- First --
   -----------

   function First (Self : Customer) return String is
   begin
      return String_Value (Self, F_Customers_First);
   end First;

   -----------
   -- First --
   -----------

   function First (Self : Detached_Customer) return String is
   begin
      return To_String (Customer_Data (Self.Unchecked_Get).ORM_First);
   end First;

   --------
   -- Id --
   --------

   function Id (Self : Customer) return Integer is
   begin
      return Integer_Value (Self, F_Customers_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Customer) return Integer is
   begin
      return Customer_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Book) return Integer is
   begin
      return Integer_Value (Self, F_Books_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Book) return Integer is
   begin
      return Book_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Dvd) return Integer is
   begin
      return Integer_Value (Self, F_Dvds_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Dvd) return Integer is
   begin
      return Dvd_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   ----------
   -- Last --
   ----------

   function Last (Self : Customer) return String is
   begin
      return String_Value (Self, F_Customers_Last);
   end Last;

   ----------
   -- Last --
   ----------

   function Last (Self : Detached_Customer) return String is
   begin
      return To_String (Customer_Data (Self.Unchecked_Get).ORM_Last);
   end Last;

   -----------
   -- Pages --
   -----------

   function Pages (Self : Book) return Integer is
   begin
      return Integer_Value (Self, F_Books_Pages);
   end Pages;

   -----------
   -- Pages --
   -----------

   function Pages (Self : Detached_Book) return Integer is
   begin
      return Book_Data (Self.Unchecked_Get).ORM_Pages;
   end Pages;

   ---------------
   -- Published --
   ---------------

   function Published (Self : Book) return Ada.Calendar.Time is
   begin
      return Time_Value (Self, F_Books_Published);
   end Published;

   ---------------
   -- Published --
   ---------------

   function Published (Self : Detached_Book) return Ada.Calendar.Time is
   begin
      return Book_Data (Self.Unchecked_Get).ORM_Published;
   end Published;

   ---------------
   -- Published --
   ---------------

   function Published (Self : Dvd) return Ada.Calendar.Time is
   begin
      return Time_Value (Self, F_Dvds_Published);
   end Published;

   ---------------
   -- Published --
   ---------------

   function Published (Self : Detached_Dvd) return Ada.Calendar.Time is
   begin
      return Dvd_Data (Self.Unchecked_Get).ORM_Published;
   end Published;

   ------------
   -- Region --
   ------------

   function Region (Self : Dvd) return Integer is
   begin
      return Integer_Value (Self, F_Dvds_Region);
   end Region;

   ------------
   -- Region --
   ------------

   function Region (Self : Detached_Dvd) return Integer is
   begin
      return Dvd_Data (Self.Unchecked_Get).ORM_Region;
   end Region;

   -----------
   -- Title --
   -----------

   function Title (Self : Book) return String is
   begin
      return String_Value (Self, F_Books_Title);
   end Title;

   -----------
   -- Title --
   -----------

   function Title (Self : Detached_Book) return String is
   begin
      return To_String (Book_Data (Self.Unchecked_Get).ORM_Title);
   end Title;

   -----------
   -- Title --
   -----------

   function Title (Self : Dvd) return String is
   begin
      return String_Value (Self, F_Dvds_Title);
   end Title;

   -----------
   -- Title --
   -----------

   function Title (Self : Detached_Dvd) return String is
   begin
      return To_String (Dvd_Data (Self.Unchecked_Get).ORM_Title);
   end Title;

   --------------------
   -- Borrowed_Books --
   --------------------

   function Borrowed_Books (Self : Customer'Class) return Books_Managers is
   begin
      return Filter (All_Books, Borrowed_By => Self.Id);
   end Borrowed_Books;

   --------------------
   -- Borrowed_Books --
   --------------------

   function Borrowed_Books
     (Self : Detached_Customer'Class)
     return Books_Managers is
   begin
      return Filter (All_Books, Borrowed_By => Self.Id);
   end Borrowed_Books;

   --------------------
   -- Borrowed_Books --
   --------------------

   function Borrowed_Books
     (Self : I_Customers_Managers'Class)
     return Books_Managers
   is
      Q : constant SQL_Query := I_Customers.Build_Query(Self, +DBA.Customers.Id);
   begin
      return All_Books.Filter
        (SQL_In(DBA.Books.Borrowed_By, Q));
   end Borrowed_Books;

   -------------------
   -- Borrowed_Dvds --
   -------------------

   function Borrowed_Dvds (Self : Customer'Class) return Dvds_Managers is
   begin
      return Filter (All_Dvds, Borrowed_By => Self.Id);
   end Borrowed_Dvds;

   -------------------
   -- Borrowed_Dvds --
   -------------------

   function Borrowed_Dvds (Self : Detached_Customer'Class) return Dvds_Managers
   is
   begin
      return Filter (All_Dvds, Borrowed_By => Self.Id);
   end Borrowed_Dvds;

   -------------------
   -- Borrowed_Dvds --
   -------------------

   function Borrowed_Dvds
     (Self : I_Customers_Managers'Class)
     return Dvds_Managers
   is
      Q : constant SQL_Query := I_Customers.Build_Query(Self, +DBA.Customers.Id);
   begin
      return All_Dvds.Filter
        (SQL_In(DBA.Dvds.Borrowed_By, Q));
   end Borrowed_Dvds;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Customer'Class) return Detached_Customer'Class
   is
      R : constant Detached_Customer'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Book'Class) return Detached_Book'Class
   is
      R : constant Detached_Book'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Dvd'Class) return Detached_Dvd'Class
   is
      R : constant Detached_Dvd'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Book'Class;
      Session : Session_Type)
     return Detached_Book'Class
   is
      Default        : Detached_Book;
      Result         : Detached_Book'Class := Detached_Book'Class (Session.Factory (Self, Default));
      Fk_Borrowed_By : Detached_Customer_Access;
      Lj             : constant Boolean := Self.Data.Follow_LJ;
      Tmp            : Book_Data;
   begin
      if Result.Is_Null then
         Result.Set (Book_DDR'
              (Detached_Data with Field_Count => 7, others => <>));
      end if;

      Tmp := Book_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         if LJ then
            FK_Borrowed_By := new Detached_Customer'Class'(
               I_Customers.Internal_Element
                 (Self, Upto_Books_0 (Self.Depth, LJ)).Detach);
         end if;

      end if;

      Tmp.ORM_Author         := To_Unbounded_String (String_Value (Self, F_Books_Author));
      Tmp.ORM_Borrowed_By    := Integer_Value (Self, F_Books_Borrowed_By);
      Tmp.ORM_FK_Borrowed_By := FK_Borrowed_By;
      Tmp.ORM_Id             := Integer_Value (Self, F_Books_Id);
      Tmp.ORM_Pages          := Integer_Value (Self, F_Books_Pages);
      Tmp.ORM_Published      := Time_Value (Self, F_Books_Published);
      Tmp.ORM_Title          := To_Unbounded_String (String_Value (Self, F_Books_Title));
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Customer'Class;
      Session : Session_Type)
     return Detached_Customer'Class
   is
      Default : Detached_Customer;
      Result  : Detached_Customer'Class := Detached_Customer'Class (Session.Factory (Self, Default));
      Tmp     : Customer_Data;
   begin
      if Result.Is_Null then
         Result.Set (Customer_DDR'
              (Detached_Data with Field_Count => 3, others => <>));
      end if;

      Tmp := Customer_Data (Result.Unchecked_Get);

      Tmp.ORM_First    := To_Unbounded_String (String_Value (Self, F_Customers_First));
      Tmp.ORM_Id       := Integer_Value (Self, F_Customers_Id);
      Tmp.ORM_Last     := To_Unbounded_String (String_Value (Self, F_Customers_Last));
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Dvd'Class;
      Session : Session_Type)
     return Detached_Dvd'Class
   is
      Default        : Detached_Dvd;
      Result         : Detached_Dvd'Class := Detached_Dvd'Class (Session.Factory (Self, Default));
      Fk_Borrowed_By : Detached_Customer_Access;
      Lj             : constant Boolean := Self.Data.Follow_LJ;
      Tmp            : Dvd_Data;
   begin
      if Result.Is_Null then
         Result.Set (Dvd_DDR'
              (Detached_Data with Field_Count => 7, others => <>));
      end if;

      Tmp := Dvd_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         if LJ then
            FK_Borrowed_By := new Detached_Customer'Class'(
               I_Customers.Internal_Element
                 (Self, Upto_Dvds_0 (Self.Depth, LJ)).Detach);
         end if;

      end if;

      Tmp.ORM_Author         := To_Unbounded_String (String_Value (Self, F_Dvds_Author));
      Tmp.ORM_Borrowed_By    := Integer_Value (Self, F_Dvds_Borrowed_By);
      Tmp.ORM_FK_Borrowed_By := FK_Borrowed_By;
      Tmp.ORM_Id             := Integer_Value (Self, F_Dvds_Id);
      Tmp.ORM_Published      := Time_Value (Self, F_Dvds_Published);
      Tmp.ORM_Region         := Integer_Value (Self, F_Dvds_Region);
      Tmp.ORM_Title          := To_Unbounded_String (String_Value (Self, F_Dvds_Title));
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   --------------------
   -- Do_Query_Books --
   --------------------

   procedure Do_Query_Books
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Books(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Pages
         & Table.Borrowed_By
         & Table.Id
         & Table.Title
         & Table.Author
         & Table.Published;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Customers(Aliases(Aliases(Base + 1)));
         begin if Follow_LJ then
            From := +Left_Join(Table, FK1, Table.Borrowed_By=FK1.Id);
         else
            From := +Table;
         end if;
         if Follow_LJ then
            C2 := No_Criteria;
            Do_Query_Customers(Fields, T, C2,Aliases(Base + 1),
               Aliases, Depth - 1, Follow_LJ);
            if Depth > 1 then
               Criteria := Criteria and C2;
            end if;
         end if;
      end;
   end if;
   end Do_Query_Books;

   ------------------------
   -- Do_Query_Customers --
   ------------------------

   procedure Do_Query_Customers
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      pragma Unreferenced (Criteria, Depth, Follow_LJ);
      Table : T_Numbered_Customers(Aliases(Base));
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.First
         & Table.Last;
      end if;
      From := Empty_Table_List;
   end Do_Query_Customers;

   -------------------
   -- Do_Query_Dvds --
   -------------------

   procedure Do_Query_Dvds
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Dvds(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Region
         & Table.Borrowed_By
         & Table.Id
         & Table.Title
         & Table.Author
         & Table.Published;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Customers(Aliases(Aliases(Base + 1)));
         begin if Follow_LJ then
            From := +Left_Join(Table, FK1, Table.Borrowed_By=FK1.Id);
         else
            From := +Table;
         end if;
         if Follow_LJ then
            C2 := No_Criteria;
            Do_Query_Customers(Fields, T, C2,Aliases(Base + 1),
               Aliases, Depth - 1, Follow_LJ);
            if Depth > 1 then
               Criteria := Criteria and C2;
            end if;
         end if;
      end;
   end if;
   end Do_Query_Dvds;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self  : Customers_Managers'Class;
      Id    : Integer := -1;
      First : String := No_Update;
      Last  : String := No_Update)
     return Customers_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Customers_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Customers.Id = Id;
      end if;
      if First /= No_Update then
         C := C and DBA.Customers.First = First;
      end if;
      if Last /= No_Update then
         C := C and DBA.Customers.Last = Last;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self        : Books_Managers'Class;
      Pages       : Integer := -1;
      Borrowed_By : Integer := -1;
      Id          : Integer := -1;
      Title       : String := No_Update;
      Author      : String := No_Update;
      Published   : Ada.Calendar.Time := No_Time)
     return Books_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Books_Managers;
   begin
      if Pages /= -1 then
         C := C and DBA.Books.Pages = Pages;
      end if;
      if Borrowed_By /= -1 then
         C := C and DBA.Books.Borrowed_By = Borrowed_By;
      end if;
      if Id /= -1 then
         C := C and DBA.Books.Id = Id;
      end if;
      if Title /= No_Update then
         C := C and DBA.Books.Title = Title;
      end if;
      if Author /= No_Update then
         C := C and DBA.Books.Author = Author;
      end if;
      if Published /= No_Time then
         C := C and DBA.Books.Published = Published;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self        : Dvds_Managers'Class;
      Region      : Integer := -1;
      Borrowed_By : Integer := -1;
      Id          : Integer := -1;
      Title       : String := No_Update;
      Author      : String := No_Update;
      Published   : Ada.Calendar.Time := No_Time)
     return Dvds_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Dvds_Managers;
   begin
      if Region /= -1 then
         C := C and DBA.Dvds.Region = Region;
      end if;
      if Borrowed_By /= -1 then
         C := C and DBA.Dvds.Borrowed_By = Borrowed_By;
      end if;
      if Id /= -1 then
         C := C and DBA.Dvds.Id = Id;
      end if;
      if Title /= No_Update then
         C := C and DBA.Dvds.Title = Title;
      end if;
      if Author /= No_Update then
         C := C and DBA.Dvds.Author = Author;
      end if;
      if Published /= No_Time then
         C := C and DBA.Dvds.Published = Published;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Book_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Borrowed_By);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Customer_Ddr) is
   begin
      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Dvd_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Borrowed_By);

      Free (Detached_Data (Self));
   end Free;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Customer'Class is
   begin
      return Detached_Customer'Class (Session.From_Cache ((0, Id), No_Detached_Customer));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Book'Class is
   begin
      return Detached_Book'Class (Session.From_Cache ((2000000, Id), No_Detached_Book));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Dvd'Class is
   begin
      return Detached_Dvd'Class (Session.From_Cache ((3000000, Id), No_Detached_Dvd));
   end From_Cache;

   --------------
   -- Get_Book --
   --------------

   function Get_Book
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Book'Class
   is
      R : constant Detached_Book'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Books_Managers := Filter
              (All_Books,
               Id => Id);
            L : I_Books.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Book;
            else

               declare
                  E : constant Book := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Books.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Book;

   ------------------
   -- Get_Customer --
   ------------------

   function Get_Customer
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Customer'Class
   is
      R : constant Detached_Customer'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Customers_Managers := Filter
              (All_Customers,
               Id => Id);
            L : I_Customers.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Customer;
            else

               declare
                  E : constant Customer := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Customers.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Customer;

   -------------
   -- Get_Dvd --
   -------------

   function Get_Dvd
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Dvd'Class
   is
      R : constant Detached_Dvd'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Dvds_Managers := Filter
              (All_Dvds,
               Id => Id);
            L : I_Dvds.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Dvd;
            else

               declare
                  E : constant Dvd := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Dvds.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Dvd;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Book;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Book_Data := Book_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (1) then
         A := A & (DBA.Books.Pages = D.ORM_Pages);
      end if;
      if Mask (2) then
         if D.ORM_Borrowed_By /= -1 then
            A := A & (DBA.Books.Borrowed_By = D.ORM_Borrowed_By);
         else

            declare
               D2 : constant Customer_Data :=
               Customer_data (D.ORM_FK_Borrowed_By.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Borrowed_By.all);
               end if;

               A := A & (DBA.Books.Borrowed_By = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (4) then
         A := A & (DBA.Books.Title = To_String (D.ORM_Title));
      end if;
      if Mask (5) then
         A := A & (DBA.Books.Author = To_String (D.ORM_Author));
      end if;
      if Mask (6) then
         A := A & (DBA.Books.Published = D.ORM_Published);
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Books, A, DBA.Books.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Books.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Customer;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Customer_Data := Customer_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Customers.First = To_String (D.ORM_First));
      end if;
      if Mask (3) then
         A := A & (DBA.Customers.Last = To_String (D.ORM_Last));
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Customers, A, DBA.Customers.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Customers.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Dvd;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (1) then
         A := A & (DBA.Dvds.Region = D.ORM_Region);
      end if;
      if Mask (2) then
         if D.ORM_Borrowed_By /= -1 then
            A := A & (DBA.Dvds.Borrowed_By = D.ORM_Borrowed_By);
         else

            declare
               D2 : constant Customer_Data :=
               Customer_data (D.ORM_FK_Borrowed_By.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Borrowed_By.all);
               end if;

               A := A & (DBA.Dvds.Borrowed_By = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (4) then
         A := A & (DBA.Dvds.Title = To_String (D.ORM_Title));
      end if;
      if Mask (5) then
         A := A & (DBA.Dvds.Author = To_String (D.ORM_Author));
      end if;
      if Mask (6) then
         A := A & (DBA.Dvds.Published = D.ORM_Published);
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Dvds, A, DBA.Dvds.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Dvds.Id);
      end if;
   end Insert_Or_Update;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Book)
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Books, DBA.Books.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Customer)
   is
      D : constant Customer_Data := Customer_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Customers, DBA.Customers.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Dvd)
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Dvds, DBA.Dvds.Id = D.ORM_Id));
   end Internal_Delete;

   --------------------------
   -- Internal_Query_Books --
   --------------------------

   procedure Internal_Query_Books
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Books(Fields, From, Criteria,
         0, Alias_Books, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Books;

   ------------------------------
   -- Internal_Query_Customers --
   ------------------------------

   procedure Internal_Query_Customers
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Customers(Fields, From, Criteria,
         0, Alias_Customers, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Customers;

   -------------------------
   -- Internal_Query_Dvds --
   -------------------------

   procedure Internal_Query_Dvds
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Dvds(Fields, From, Criteria,
         0, Alias_Dvds, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Dvds;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Book_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (2000000, No_Primary_Key);
      else
         return (2000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Customer_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (0, No_Primary_Key);
      else
         return (0, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Dvd_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (3000000, No_Primary_Key);
      else
         return (3000000, Self.ORM_Id);
      end if;
   end Key;

   --------------
   -- New_Book --
   --------------

   function New_Book return Detached_Book'Class
   is
      Result : Detached_Book;
      Data   : Book_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Book;

   ------------------
   -- New_Customer --
   ------------------

   function New_Customer return Detached_Customer'Class
   is
      Result : Detached_Customer;
      Data   : Customer_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Customer;

   -------------
   -- New_Dvd --
   -------------

   function New_Dvd return Detached_Dvd'Class
   is
      Result : Detached_Dvd;
      Data   : Dvd_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Dvd;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Book)
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Borrowed_By /= null then
            Self.Session.Persist (D.ORM_FK_Borrowed_By.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Dvd)
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Borrowed_By /= null then
            Self.Session.Persist (D.ORM_FK_Borrowed_By.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- Set_Author --
   ----------------

   procedure Set_Author (Self : Detached_Book; Value : String)
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
   begin
      D.ORM_Author := To_Unbounded_String (Value);
      Self.Set_Modified (5);
   end Set_Author;

   ----------------
   -- Set_Author --
   ----------------

   procedure Set_Author (Self : Detached_Dvd; Value : String)
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
   begin
      D.ORM_Author := To_Unbounded_String (Value);
      Self.Set_Modified (5);
   end Set_Author;

   ---------------------
   -- Set_Borrowed_By --
   ---------------------

   procedure Set_Borrowed_By (Self : Detached_Book; Value : Integer)
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Borrowed_By);
      D.ORM_Borrowed_By := Value;
      Self.Set_Modified (2);
   end Set_Borrowed_By;

   ---------------------
   -- Set_Borrowed_By --
   ---------------------

   procedure Set_Borrowed_By
     (Self  : Detached_Book;
      Value : Detached_Customer'Class)
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Borrowed_By);
      D.ORM_Borrowed_By := Value.Id;
      D.ORM_FK_Borrowed_By := new Detached_Customer'Class'(Value);

      Self.Set_Modified (2);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Borrowed_By.all);
      end if;
   end Set_Borrowed_By;

   ---------------------
   -- Set_Borrowed_By --
   ---------------------

   procedure Set_Borrowed_By (Self : Detached_Dvd; Value : Integer)
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Borrowed_By);
      D.ORM_Borrowed_By := Value;
      Self.Set_Modified (2);
   end Set_Borrowed_By;

   ---------------------
   -- Set_Borrowed_By --
   ---------------------

   procedure Set_Borrowed_By
     (Self  : Detached_Dvd;
      Value : Detached_Customer'Class)
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Borrowed_By);
      D.ORM_Borrowed_By := Value.Id;
      D.ORM_FK_Borrowed_By := new Detached_Customer'Class'(Value);

      Self.Set_Modified (2);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Borrowed_By.all);
      end if;
   end Set_Borrowed_By;

   ---------------
   -- Set_First --
   ---------------

   procedure Set_First (Self : Detached_Customer; Value : String)
   is
      D : constant Customer_Data := Customer_Data (Self.Unchecked_Get);
   begin
      D.ORM_First := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_First;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (Self : Detached_Customer; Value : String)
   is
      D : constant Customer_Data := Customer_Data (Self.Unchecked_Get);
   begin
      D.ORM_Last := To_Unbounded_String (Value);
      Self.Set_Modified (3);
   end Set_Last;

   ---------------
   -- Set_Pages --
   ---------------

   procedure Set_Pages (Self : Detached_Book; Value : Integer)
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
   begin
      D.ORM_Pages := Value;
      Self.Set_Modified (1);
   end Set_Pages;

   -------------------
   -- Set_Published --
   -------------------

   procedure Set_Published (Self : Detached_Book; Value : Ada.Calendar.Time)
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
   begin
      D.ORM_Published := Value;
      Self.Set_Modified (6);
   end Set_Published;

   -------------------
   -- Set_Published --
   -------------------

   procedure Set_Published (Self : Detached_Dvd; Value : Ada.Calendar.Time)
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
   begin
      D.ORM_Published := Value;
      Self.Set_Modified (6);
   end Set_Published;

   ----------------
   -- Set_Region --
   ----------------

   procedure Set_Region (Self : Detached_Dvd; Value : Integer)
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
   begin
      D.ORM_Region := Value;
      Self.Set_Modified (1);
   end Set_Region;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Self : Detached_Book; Value : String)
   is
      D : constant Book_Data := Book_Data (Self.Unchecked_Get);
   begin
      D.ORM_Title := To_Unbounded_String (Value);
      Self.Set_Modified (4);
   end Set_Title;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Self : Detached_Dvd; Value : String)
   is
      D : constant Dvd_Data := Dvd_Data (Self.Unchecked_Get);
   begin
      D.ORM_Title := To_Unbounded_String (Value);
      Self.Set_Modified (4);
   end Set_Title;
end Orm;

