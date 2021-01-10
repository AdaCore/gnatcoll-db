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

with GNATCOLL.SQL; use GNATCOLL.SQL;
pragma Warnings (Off, "no entities of * are referenced");
pragma Warnings (Off, "use clause for package * has no effect");
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
pragma Warnings (On, "no entities of * are referenced");
pragma Warnings (On, "use clause for package * has no effect");
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Media
      (Table_Name : Cst_String_Access;
       Instance   : Cst_String_Access;
       Index      : Integer)
   is abstract new SQL_Table (Table_Name, Instance, Index) with
   record
      Id : SQL_Field_Integer (Table_Name, Instance, N_Id, Index);
      --  Auto-generated id

      Title : SQL_Field_Text (Table_Name, Instance, N_Title, Index);
      --  The title of the media

      Author : SQL_Field_Text (Table_Name, Instance, N_Author, Index);
      --  The author

      Published : SQL_Field_Date (Table_Name, Instance, N_Published, Index);
      --  Publication date

   end record;

   type T_Abstract_Books
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new T_Media (Ta_Books, Instance, Index) with
   record
      Pages : SQL_Field_Integer (Ta_Books, Instance, N_Pages, Index);
      Borrowed_By : SQL_Field_Integer (Ta_Books, Instance, N_Borrowed_By, Index);
      --  Who borrowed the media

   end record;

   type T_Books (Instance : Cst_String_Access)
      is new T_Abstract_Books (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Books (Index : Integer)
      is new T_Abstract_Books (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Customers
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Customers, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Customers, Instance, N_Id, Index);
      --  Auto-generated id

      First : SQL_Field_Text (Ta_Customers, Instance, N_First, Index);
      --  Customer's first name

      Last : SQL_Field_Text (Ta_Customers, Instance, N_Last, Index);
      --  Customer's last name

   end record;

   type T_Customers (Instance : Cst_String_Access)
      is new T_Abstract_Customers (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Customers (Index : Integer)
      is new T_Abstract_Customers (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Dvds
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new T_Media (Ta_Dvds, Instance, Index) with
   record
      Region : SQL_Field_Integer (Ta_Dvds, Instance, N_Region, Index);
      Borrowed_By : SQL_Field_Integer (Ta_Dvds, Instance, N_Borrowed_By, Index);
      --  Who borrowed the media

   end record;

   type T_Dvds (Instance : Cst_String_Access)
      is new T_Abstract_Dvds (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Dvds (Index : Integer)
      is new T_Abstract_Dvds (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   function FK (Self : T_Books'Class; Foreign : T_Customers'Class) return SQL_Criteria;
   function FK (Self : T_Dvds'Class; Foreign : T_Customers'Class) return SQL_Criteria;
   Books : T_Books (null);
   Customers : T_Customers (null);
   Dvds : T_Dvds (null);
end Database;
