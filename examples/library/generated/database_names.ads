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
package Database_Names is
   pragma Style_Checks (Off);
   TC_Books : aliased constant String := "books";
   Ta_Books : constant Cst_String_Access := TC_Books'Access;
   TC_Customers : aliased constant String := "customers";
   Ta_Customers : constant Cst_String_Access := TC_Customers'Access;
   TC_Dvds : aliased constant String := "dvds";
   Ta_Dvds : constant Cst_String_Access := TC_Dvds'Access;
   TC_Media : aliased constant String := "media";
   Ta_Media : constant Cst_String_Access := TC_Media'Access;

   NC_Author : aliased constant String := "author";
   N_Author : constant Cst_String_Access := NC_author'Access;
   NC_Borrowed_By : aliased constant String := "borrowed_by";
   N_Borrowed_By : constant Cst_String_Access := NC_borrowed_by'Access;
   NC_First : aliased constant String := """first""";
   N_First : constant Cst_String_Access := NC_first'Access;
   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_Last : aliased constant String := """last""";
   N_Last : constant Cst_String_Access := NC_last'Access;
   NC_Pages : aliased constant String := "pages";
   N_Pages : constant Cst_String_Access := NC_pages'Access;
   NC_Published : aliased constant String := "published";
   N_Published : constant Cst_String_Access := NC_published'Access;
   NC_Region : aliased constant String := "region";
   N_Region : constant Cst_String_Access := NC_region'Access;
   NC_Title : aliased constant String := "title";
   N_Title : constant Cst_String_Access := NC_title'Access;
end Database_Names;
