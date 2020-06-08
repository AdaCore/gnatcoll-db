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

with GNATCOLL.DB2Ada.Main;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Postgres;

procedure GNATCOLL_Postgres2Ada is

   function Postgres_Description (DB_Type  : String;
                                Database : String;
                                User     : String;
                                Host     : String;
                                Password : String;
                                Port     : Integer)
                                 return Database_Description;
   function Postgres_Description (DB_Type  : String;
                                Database : String;
                                User     : String;
                                Host     : String;
                                Password : String;
                                Port     : Integer)
                                 return Database_Description
   is
   begin
      if DB_Type /= "postgresql" then
         return null;
      end if;
      return GNATCOLL.SQL.Postgres.Setup
         (Database      => Database,
          User          => User,
          Host          => Host,
          Password      => Password,
          Port          => Port,
          Cache_Support => False);
   end Postgres_Description;

begin
   GNATCOLL.DB2ADA.Main (
      "postgresql",
      Postgres_Description'Unrestricted_Access);
end GNATCOLL_Postgres2Ada;
