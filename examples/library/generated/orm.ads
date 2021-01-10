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
with Ada.Calendar; use Ada.Calendar;
with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Database; use Database;
with GNAT.Calendar; use GNAT.Calendar;
with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.SQL; use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Orm; use GNATCOLL.SQL.Orm;
with GNATCOLL.SQL.Orm.Impl; use GNATCOLL.SQL.Orm.Impl;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
with GNATCOLL.Tribooleans; use GNATCOLL.Tribooleans;
with System.Address_Image;
pragma Warnings (On);
pragma Style_Checks (Off);

package Orm is
   package DBA renames Database;
   subtype Related_Depth is Integer range 0 .. 3;

   ---------------------
   -- Elements: Media --
   ---------------------
   --  Interfaces corresponding to abstract tables in the schema

   type Media is interface;

   function Author (Self : Media) return String is abstract;
   --  The author

   function Id (Self : Media) return Integer is abstract;
   --  Auto-generated id

   function Published (Self : Media) return Ada.Calendar.Time is abstract;
   --  Publication date

   function Title (Self : Media) return String is abstract;

   -----------
   -- Types --
   -----------
   --  Detached_* elements extract the value from the list and store them
    --  locally. As a result, they remain valid even if the list is modified,
    --  but require more memory to store.
    --
    --  Other elements are only valid while the list from which they are
    --  created is not modified(see Element below). As soon as you iterate the
    --  list this element becomes invalid.
    --
    --  Direct lists are stored in memory, and can be traversed in any order.
    --  Forward lists can only be iterated forward. With some database backends
    --  this is much more efficient since only the current element needs to be
    --  stored in memory(and retrieved from the server).

   type Book is new Orm_Element and Media with null record;
   type Book_DDR is new Detached_Data (7) with private;
   type Detached_Book is  --  Get() returns a Book_DDR
   new Sessions.Detached_Element and Media with private;
   type Detached_Book_Access is access all Detached_Book'Class;
   No_Detached_Book : constant Detached_Book;
   No_Book : constant Book;

   type Customer is new Orm_Element with null record;
   type Customer_DDR is new Detached_Data (3) with private;
   type Detached_Customer is  --  Get() returns a Customer_DDR
   new Sessions.Detached_Element with private;
   type Detached_Customer_Access is access all Detached_Customer'Class;
   No_Detached_Customer : constant Detached_Customer;
   No_Customer : constant Customer;

   type Dvd is new Orm_Element and Media with null record;
   type Dvd_DDR is new Detached_Data (7) with private;
   type Detached_Dvd is  --  Get() returns a Dvd_DDR
   new Sessions.Detached_Element and Media with private;
   type Detached_Dvd_Access is access all Detached_Dvd'Class;
   No_Detached_Dvd : constant Detached_Dvd;
   No_Dvd : constant Dvd;


   -------------------------
   -- Elements: Customers --
   -------------------------

   function "=" (Op1 : Customer; Op2 : Customer) return Boolean;
   function "="
     (Op1 : Detached_Customer;
      Op2 : Detached_Customer)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function First (Self : Customer) return String;
   function First (Self : Detached_Customer) return String;
   procedure Set_First (Self : Detached_Customer; Value : String);
   --  Customer's first name

   function Id (Self : Customer) return Integer;
   function Id (Self : Detached_Customer) return Integer;
   --  Auto-generated id

   function Last (Self : Customer) return String;
   function Last (Self : Detached_Customer) return String;
   procedure Set_Last (Self : Detached_Customer; Value : String);
   --  Customer's last name

   function Detach (Self : Customer'Class) return Detached_Customer'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Customer'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Customer return Detached_Customer'Class;

   ---------------------
   -- Elements: Books --
   ---------------------

   function "=" (Op1 : Book; Op2 : Book) return Boolean;
   function "=" (Op1 : Detached_Book; Op2 : Detached_Book) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Author (Self : Book) return String;
   function Author (Self : Detached_Book) return String;
   procedure Set_Author (Self : Detached_Book; Value : String);
   --  The author

   function Borrowed_By (Self : Book) return Integer;
   function Borrowed_By (Self : Detached_Book) return Integer;
   procedure Set_Borrowed_By (Self : Detached_Book; Value : Integer);
   function Borrowed_By (Self : Book) return Customer'Class;
   function Borrowed_By (Self : Detached_Book) return Detached_Customer'Class;
   procedure Set_Borrowed_By
     (Self  : Detached_Book;
      Value : Detached_Customer'Class);
   --  Who borrowed the media

   function Id (Self : Book) return Integer;
   function Id (Self : Detached_Book) return Integer;
   --  Auto-generated id

   function Pages (Self : Book) return Integer;
   function Pages (Self : Detached_Book) return Integer;
   procedure Set_Pages (Self : Detached_Book; Value : Integer);

   function Published (Self : Book) return Ada.Calendar.Time;
   function Published (Self : Detached_Book) return Ada.Calendar.Time;
   procedure Set_Published (Self : Detached_Book; Value : Ada.Calendar.Time);
   --  Publication date

   function Title (Self : Book) return String;
   function Title (Self : Detached_Book) return String;
   procedure Set_Title (Self : Detached_Book; Value : String);
   --  The title of the media

   function Detach (Self : Book'Class) return Detached_Book'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Book'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Book return Detached_Book'Class;

   --------------------
   -- Elements: Dvds --
   --------------------

   function "=" (Op1 : Dvd; Op2 : Dvd) return Boolean;
   function "=" (Op1 : Detached_Dvd; Op2 : Detached_Dvd) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Author (Self : Dvd) return String;
   function Author (Self : Detached_Dvd) return String;
   procedure Set_Author (Self : Detached_Dvd; Value : String);
   --  The author

   function Borrowed_By (Self : Dvd) return Integer;
   function Borrowed_By (Self : Detached_Dvd) return Integer;
   procedure Set_Borrowed_By (Self : Detached_Dvd; Value : Integer);
   function Borrowed_By (Self : Dvd) return Customer'Class;
   function Borrowed_By (Self : Detached_Dvd) return Detached_Customer'Class;
   procedure Set_Borrowed_By
     (Self  : Detached_Dvd;
      Value : Detached_Customer'Class);
   --  Who borrowed the media

   function Id (Self : Dvd) return Integer;
   function Id (Self : Detached_Dvd) return Integer;
   --  Auto-generated id

   function Published (Self : Dvd) return Ada.Calendar.Time;
   function Published (Self : Detached_Dvd) return Ada.Calendar.Time;
   procedure Set_Published (Self : Detached_Dvd; Value : Ada.Calendar.Time);
   --  Publication date

   function Region (Self : Dvd) return Integer;
   function Region (Self : Detached_Dvd) return Integer;
   procedure Set_Region (Self : Detached_Dvd; Value : Integer);

   function Title (Self : Dvd) return String;
   function Title (Self : Detached_Dvd) return String;
   procedure Set_Title (Self : Detached_Dvd; Value : String);
   --  The title of the media

   function Detach (Self : Dvd'Class) return Detached_Dvd'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Dvd'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Dvd return Detached_Dvd'Class;

   --------------------------------------
   -- Managers(Implementation Details) --
   --------------------------------------

   procedure Internal_Query_Books
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Customers
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Dvds
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   -------------------
   -- Manager Types --
   -------------------

   type I_Books_Managers is abstract new Manager with null record;
   package I_Books is new Generic_Managers
     (I_Books_Managers, Book, Related_Depth, DBA.Books,
      Internal_Query_Books);
   type Books_Managers is new I_Books.Manager with null record;
   subtype Books_Stmt is I_Books.ORM_Prepared_Statement;

   subtype Book_List is I_Books.List;
   subtype Direct_Book_List is I_Books.Direct_List;
   Empty_Book_List : constant Book_List := I_Books.Empty_List;
   Empty_Direct_Book_List : constant Direct_Book_List :=
   I_Books.Empty_Direct_List;

   type I_Customers_Managers is abstract new Manager with null record;
   package I_Customers is new Generic_Managers
     (I_Customers_Managers, Customer, Related_Depth, DBA.Customers,
      Internal_Query_Customers);
   type Customers_Managers is new I_Customers.Manager with null record;
   subtype Customers_Stmt is I_Customers.ORM_Prepared_Statement;

   subtype Customer_List is I_Customers.List;
   subtype Direct_Customer_List is I_Customers.Direct_List;
   Empty_Customer_List : constant Customer_List := I_Customers.Empty_List;
   Empty_Direct_Customer_List : constant Direct_Customer_List :=
   I_Customers.Empty_Direct_List;

   type I_Dvds_Managers is abstract new Manager with null record;
   package I_Dvds is new Generic_Managers
     (I_Dvds_Managers, Dvd, Related_Depth, DBA.Dvds,
      Internal_Query_Dvds);
   type Dvds_Managers is new I_Dvds.Manager with null record;
   subtype Dvds_Stmt is I_Dvds.ORM_Prepared_Statement;

   subtype Dvd_List is I_Dvds.List;
   subtype Direct_Dvd_List is I_Dvds.Direct_List;
   Empty_Dvd_List : constant Dvd_List := I_Dvds.Empty_List;
   Empty_Direct_Dvd_List : constant Direct_Dvd_List :=
   I_Dvds.Empty_Direct_List;


   ------------------------
   -- Manager: Customers --
   ------------------------

   function Borrowed_Books (Self : Customer'Class) return Books_Managers;
   function Borrowed_Books
     (Self : Detached_Customer'Class)
     return Books_Managers;
   function Borrowed_Books
     (Self : I_Customers_Managers'Class)
     return Books_Managers;

   function Borrowed_Dvds (Self : Customer'Class) return Dvds_Managers;
   function Borrowed_Dvds (Self : Detached_Customer'Class) return Dvds_Managers;
   function Borrowed_Dvds
     (Self : I_Customers_Managers'Class)
     return Dvds_Managers;

   function Filter
     (Self  : Customers_Managers'Class;
      Id    : Integer := -1;
      First : String := No_Update;
      Last  : String := No_Update)
     return Customers_Managers;

   function Get_Customer
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Customer'Class;

   --------------------
   -- Manager: Books --
   --------------------

   function Filter
     (Self        : Books_Managers'Class;
      Pages       : Integer := -1;
      Borrowed_By : Integer := -1;
      Id          : Integer := -1;
      Title       : String := No_Update;
      Author      : String := No_Update;
      Published   : Ada.Calendar.Time := No_Time)
     return Books_Managers;

   function Get_Book
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Book'Class;

   -------------------
   -- Manager: Dvds --
   -------------------

   function Filter
     (Self        : Dvds_Managers'Class;
      Region      : Integer := -1;
      Borrowed_By : Integer := -1;
      Id          : Integer := -1;
      Title       : String := No_Update;
      Author      : String := No_Update;
      Published   : Ada.Calendar.Time := No_Time)
     return Dvds_Managers;

   function Get_Dvd
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Dvd'Class;

   --------------
   -- Managers --
   --------------

   All_Books : constant Books_Managers :=
     (I_Books.All_Managers with null record);

   All_Customers : constant Customers_Managers :=
     (I_Customers.All_Managers with null record);

   All_Dvds : constant Dvds_Managers :=
     (I_Dvds.All_Managers with null record);


   --------------
   -- Internal --
   --------------

   overriding procedure Free (Self : in out Book_Ddr);
   overriding procedure Free (Self : in out Customer_Ddr);
   overriding procedure Free (Self : in out Dvd_Ddr);

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Book;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Customer;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Dvd;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);

   overriding procedure Internal_Delete (Self : Detached_Book);
   overriding procedure Internal_Delete (Self : Detached_Customer);
   overriding procedure Internal_Delete (Self : Detached_Dvd);

   overriding function Key (Self : Book_Ddr) return Element_Key;
   overriding function Key (Self : Customer_Ddr) return Element_Key;
   overriding function Key (Self : Dvd_Ddr) return Element_Key;

   overriding procedure On_Persist (Self : Detached_Book);
   overriding procedure On_Persist (Self : Detached_Dvd);

private

    type Book_DDR is new Detached_Data (7) with record
       ORM_Author         : Unbounded_String := Null_Unbounded_String;
       ORM_Borrowed_By    : Integer := -1;
       ORM_FK_Borrowed_By : Detached_Customer_Access := null;
       ORM_Id             : Integer := -1;
       ORM_Pages          : Integer := 100;
       ORM_Published      : Ada.Calendar.Time := No_Time;
       ORM_Title          : Unbounded_String := Null_Unbounded_String;
    end record;
    type Book_Data is access all Book_DDR;

    type Customer_DDR is new Detached_Data (3) with record
       ORM_First    : Unbounded_String := Null_Unbounded_String;
       ORM_Id       : Integer := -1;
       ORM_Last     : Unbounded_String := Null_Unbounded_String;
    end record;
    type Customer_Data is access all Customer_DDR;

    type Dvd_DDR is new Detached_Data (7) with record
       ORM_Author         : Unbounded_String := Null_Unbounded_String;
       ORM_Borrowed_By    : Integer := -1;
       ORM_FK_Borrowed_By : Detached_Customer_Access := null;
       ORM_Id             : Integer := -1;
       ORM_Published      : Ada.Calendar.Time := No_Time;
       ORM_Region         : Integer := 1;
       ORM_Title          : Unbounded_String := Null_Unbounded_String;
    end record;
    type Dvd_Data is access all Dvd_DDR;


    type Detached_Book
       is new Sessions.Detached_Element and Media with null record;
    No_Book : constant Book :=(No_Orm_Element with null record);
    No_Detached_Book : constant Detached_Book :=
      (Sessions.Detached_Element with null record);

    type Detached_Customer
       is new Sessions.Detached_Element with null record;
    No_Customer : constant Customer :=(No_Orm_Element with null record);
    No_Detached_Customer : constant Detached_Customer :=
      (Sessions.Detached_Element with null record);

    type Detached_Dvd
       is new Sessions.Detached_Element and Media with null record;
    No_Dvd : constant Dvd :=(No_Orm_Element with null record);
    No_Detached_Dvd : constant Detached_Dvd :=
      (Sessions.Detached_Element with null record);

end Orm;
