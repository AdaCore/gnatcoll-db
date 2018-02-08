------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;

package GNATCOLL.DB2Ada is

   type Db2Ada_Description is access function
      (DB_Type  : String;
       Database : String;
       User     : String;
       Host     : String;
       Password : String;
       Port     : Integer)
   return Database_Description;
   --  Given access parameter return a Database_Description.
   --
   --  DB_Type is the database backend name. Function should return null if the
   --  given backend is not supported.
   --
   --  Db2Ada_Description received the following connection parameters:
   --  * Database: the database name
   --  * User:     user name
   --  * Host:     host name
   --  * Password: password to access the database
   --  * Port:     port number

end GNATCOLL.DB2Ada;
