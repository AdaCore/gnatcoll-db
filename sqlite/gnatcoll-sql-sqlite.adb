------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2020, AdaCore                     --
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

with GNATCOLL.SQL.Sqlite.Builder;

package body GNATCOLL.SQL.Sqlite is

   type Sqlite_Engine is new Database_Engine with null record;

   overriding function Setup
     (Engine  : Sqlite_Engine;
      Options : Name_Values.Map;
      Errors  : access Error_Reporter'Class) return Database_Description;

   -----------
   -- Setup --
   -----------

   function Setup
     (Database      : String;
      Cache_Support : Boolean := False;
      Errors        : access Error_Reporter'Class := null;
      Is_URI        : Boolean := False)
      return Database_Description
   is
      Result : Sqlite_Description_Access;
   begin
      Result := new Sqlite_Description
        (Caching => Cache_Support, Errors => Errors);
      Result.Dbname := new String'(Database);
      Result.Is_URI := Is_URI;
      return Database_Description (Result);
   end Setup;

   -----------
   -- Setup --
   -----------

   overriding function Setup
     (Engine  : Sqlite_Engine;
      Options : Name_Values.Map;
      Errors  : access Error_Reporter'Class) return Database_Description
   is
      pragma Unreferenced (Engine);

      type Setup_Parameters is (Database, Is_URI, Caching);
      Params : array (Setup_Parameters) of Name_Values.Cursor;

      function Value (P : Setup_Parameters; Default : String) return String is
        (if Name_Values.Has_Element (Params (P))
         then Name_Values.Element (Params (P)) else Default);
   begin
      for C in Options.Iterate loop
         Params (Setup_Parameters'Value (Name_Values.Key (C))) := C;
      end loop;

      return Setup
        (Database      => Value (Database, ""),
         Is_URI        => Boolean'Value (Value (Is_URI, "False")),
         Cache_Support => Boolean'Value (Value (Caching, "False")),
         Errors        => Errors);
   end Setup;

   ----------------------
   -- Build_Connection --
   ----------------------

   overriding function Build_Connection
     (Self : access Sqlite_Description) return Database_Connection
   is
      DB : Database_Connection;
   begin
      DB := GNATCOLL.SQL.Sqlite.Builder.Build_Connection (Self);
      Reset_Connection (DB);
      return DB;
   end Build_Connection;

   ---------------
   -- Is_Sqlite --
   ---------------

   function Is_Sqlite
     (DB : access Database_Connection_Record'Class) return Boolean
   is
   begin
      return Get_Description (DB).all in Sqlite_Description;
   end Is_Sqlite;

   -------------
   -- DB_Name --
   -------------

   function DB_Name
     (DB : access Database_Connection_Record'Class) return String is
   begin
      return Sqlite_Description (Get_Description (DB).all).Dbname.all;
   exception
      when Constraint_Error =>
         --  Probably not a Sqlite_Description
         return "";
   end DB_Name;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Sqlite_Description) is
   begin
      Free (Self.Dbname);
   end Free;

   ------------
   -- Backup --
   ------------

   function Backup
     (DB1             : access Database_Connection_Record'Class;
      DB2             : String;
      From_DB1_To_DB2 : Boolean := True) return Boolean
   is
   begin
      return GNATCOLL.SQL.Sqlite.Builder.Backup (DB1, DB2, From_DB1_To_DB2);
   end Backup;

   ------------
   -- Backup --
   ------------

   function Backup
     (From : access Database_Connection_Record'Class;
      To   : access Database_Connection_Record'Class) return Boolean is
   begin
      return GNATCOLL.SQL.Sqlite.Builder.Backup (From, To);
   end Backup;

end GNATCOLL.SQL.Sqlite;
