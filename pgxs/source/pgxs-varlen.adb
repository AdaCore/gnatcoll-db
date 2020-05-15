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

with PGXS.Types;

package body PGXS.Varlen is

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Size : System.Storage_Elements.Storage_Count) return Varlen_A
   is
      function palloc_varlena (Size : PGXS.Types.Int_32) return Varlen_A
        with Import, Convention => C, Link_Name => "__ada_palloc_varlena";

      procedure Set_Size (Item : Varlen_A; Size : PGXS.Types.Int_32)
        with Import, Convention => C, Link_Name => "__ada_SET_VARSIZE";

   begin
      return Result : Varlen_A := palloc_varlena (PGXS.Types.Int_32 (Size)) do
         Set_Size (Result, PGXS.Types.Int_32 (Size));
      end return;
   end Allocate;

   ----------
   -- Size --
   ----------

   function Size
     (Item : Varlen_A) return System.Storage_Elements.Storage_Count
   is
      function Imported (Item : Varlen_A) return PGXS.Types.Int_32
        with Import, Convention => C, Link_Name => "__ada_VARSIZE_ANY_EXHDR";

   begin
      return System.Storage_Elements.Storage_Count (Imported (Item));
   end Size;

end PGXS.Varlen;
