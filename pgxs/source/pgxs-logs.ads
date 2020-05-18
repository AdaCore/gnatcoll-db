------------------------------------------------------------------------------
--               PostgreSQL server extension modules binding                --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
--  Access to PostgreSQL error reporting/logging features

with Interfaces.C;

private with PGXS.Types;

package PGXS.Logs is

   type Error_Level is private;

   Debug_5         : constant Error_Level;
   Debug_4         : constant Error_Level;
   Debug_3         : constant Error_Level;
   Debug_2         : constant Error_Level;
   Debug_1         : constant Error_Level;
   Log             : constant Error_Level;
   Log_Server_Only : constant Error_Level;
   Info            : constant Error_Level;
   Notice          : constant Error_Level;
   Warning         : constant Error_Level;
   Error           : constant Error_Level;

   procedure Report
     (Level   : Error_Level;
      Message : Interfaces.C.char_array)
     with Import, Convention => C, Link_Name => "__ada_PG_ereport";
   --  Reports message of given level. Message must be C-style 'nul' terminated
   --  string.

private

   type Error_Level is new PGXS.Types.Int_32;

   Debug_5         : constant Error_Level := 10;
   Debug_4         : constant Error_Level := 11;
   Debug_3         : constant Error_Level := 12;
   Debug_2         : constant Error_Level := 13;
   Debug_1         : constant Error_Level := 14;
   Log             : constant Error_Level := 15;
   Log_Server_Only : constant Error_Level := 16;
   Info            : constant Error_Level := 17;
   Notice          : constant Error_Level := 18;
   Warning         : constant Error_Level := 19;
   Error           : constant Error_Level := 20;

end PGXS.Logs;
