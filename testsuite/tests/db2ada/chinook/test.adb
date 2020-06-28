------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with Ada.Strings.Maps.Constants; use Ada.Strings.Maps;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Text_IO;                use Ada.Text_IO;

with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL;
with GNATCOLL.SQL.Sqlite;

with Database;            use Database;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   DB : Exec.Database_Connection := Sqlite.Setup ("db.db").Build_Connection;
   Lines : constant Natural :=
             (if Argument_Count = 0 then 4
              elsif Is_Subset
                      (To_Set (Argument (1)), Constants.Decimal_Digit_Set)
              then Natural'Value (Argument (1)) else Natural'Last);

   --------------------
   -- Select_Request --
   --------------------

   procedure Select_Request (Fields : SQL_Field_List'Class) is
      use type Exec.Field_Index;
      FC : Exec.Forward_Cursor;
      Q  : SQL_Query :=
             SQL_Select
               (Fields, Order_By => Expression (1), Auto_Complete => True);
      function Delimit (Index : Exec.Field_Index) return Character is
        (if Index = 0 then ASCII.LF else ASCII.HT);
   begin
      FC.Fetch (DB, Q);

      for J in 0 .. FC.Field_Count - 1 loop
         Put (Delimit (J) & FC.Field_Name (J));
      end loop;

      while FC.Has_Row loop
         for J in 0 .. FC.Field_Count - 1 loop
            declare
               Value : constant String :=
                         (if FC.Is_Null (J) then "(null)"
                          else FC.Value (J));
            begin
               if FC.Current <= Lines then
                  Put (Delimit (J));

                  for C of Value loop
                     --  Don't print non ascii characters.
                     --  GNATPython has problems with it.

                     Put (if Character'Pos (C) > 127 then '_' else C);
                  end loop;

               end if;
            end;
         end loop;

         FC.Next;
      end loop;
   end Select_Request;

begin
   Select_Request (Album.Albumid & Album.Artistid & Album.Title);
   Select_Request (Artist.Artistid & Artist.Name);
   Select_Request
     (Customer.Customerid & Customer.Supportrepid & Customer.Firstname
      & Customer.Lastname & Customer.Company & Customer.Address & Customer.City
      & Customer.Country & Customer.Postalcode & Customer.Phone
      & Customer.Email);
   Select_Request
     (Employee.Employeeid & Employee.Reportsto & Employee.Lastname
      & Employee.Firstname & Employee.Title & Employee.Birthdate
      & Employee.Hiredate & Employee.Address & Employee.City & Employee.State
      & Employee.Country & Employee.Postalcode & Employee.Phone & Employee.Fax
      & Employee.Email);
   Select_Request (Genre.Genreid & Genre.Name);
   Select_Request
     (Invoice.Invoiceid & Invoice.Customerid & Invoice.Total
      & Invoice.Invoicedate & Invoice.Billingaddress & Invoice.Billingcity
      & Invoice.Billingstate & Invoice.Billingcountry
      & Invoice.Billingpostalcode);
   Select_Request
     (Invoiceline.Invoicelineid & Invoiceline.Invoiceid & Invoiceline.Trackid
      & Invoiceline.Unitprice & Invoiceline.Quantity);
   Select_Request (Mediatype.Mediatypeid & Mediatype.Name);
   Select_Request (Playlist.Playlistid & Playlist.Name);
   Select_Request (Playlisttrack.Playlistid & Playlisttrack.Trackid);
   Select_Request
     (Track.Trackid & Track.Albumid  & Track.Mediatypeid & Track.Genreid
      & Track.Milliseconds & Track.Bytes & Track.Unitprice & Track.Composer
      & Track.Name);
   New_Line;
   return A.Report;
end Test;
