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
--  Memory pool on top of PostgreSQL memory allocator.
--
--  Allocated memory is not need to be freed manually, all allocated chunks
--  freed when memory context is freed.

with System.Storage_Elements;
with System.Storage_Pools;

private with PGXS.Types;

package PGXS.Pools is

   type Memory_Context_Pool is
     new System.Storage_Pools.Root_Storage_Pool with private;

private

   type Memory_Context_Pool is
     new System.Storage_Pools.Root_Storage_Pool with null record;

   overriding procedure Allocate
     (Pool                     : in out Memory_Context_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool                     : in out Memory_Context_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is null;

   overriding function Storage_Size
     (Pool : Memory_Context_Pool)
      return System.Storage_Elements.Storage_Count is
     (System.Storage_Elements.Storage_Count (PGXS.Types.Int_32'Last));

end PGXS.Pools;
