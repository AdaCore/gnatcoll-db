with Ada.Calendar;          use Ada.Calendar;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
--  with GNATCOLL.SQL.Postgres; use GNATCOLL.SQL;
with GNATCOLL.SQL.Sqlite;   use GNATCOLL.SQL;
with GNATCOLL.Utils;

with GNAT.Calendar.Time_IO;

with Test_Assert; use Test_Assert;

function Test return Integer is
   --  DB_Descr : Database_Description := Postgres.Setup ("test");
   DB_Descr : Database_Description := Sqlite.Setup ("test.db");
   DBC      : Database_Connection  := DB_Descr.Build_Connection;
   RS       : Forward_Cursor;
   Old      : constant Time :=
                Time_Of
                  (Year_Number'First, Month_Number'First, Day_Number'First);
   Now      : constant Time := Clock;
   Stmt     : Prepared_Statement'Class :=
                Prepare
                  ("insert into Keep_Timestamp values ($1)",
                   Name => "add_timestamp",
                   On_Server => True);

   Dts : array (1 .. 24) of Time;

begin
   Execute (DBC, "create table Keep_Timestamp (DT Timestamp)");
   if not DBC.Success then
      Put_Line (DBC.Error);
   end if;
   DBC.Commit;

   Execute (DBC, "delete from Keep_Timestamp");
   if not DBC.Success then
      Put_Line (DBC.Error);
   end if;

   for J in Dts'First .. Dts'Length / 2 - 1 loop
      Dts (J) := Old + 987654.321_098_765 * J;
   end loop;

   for J in Dts'Length / 2 + 1 .. Dts'Last loop
      Dts (J) := Now + 1234_567.890_123_456 * J;
   end loop;

   Dts (Dts'Length / 2) := Time_Of (2000, 9, 8, (7.0 * 60 + 34.0) * 60 + 56.0);

   for J in reverse Dts'Range loop
      Execute (DBC, Stmt, (1 => +Dts (J)));

      if not DBC.Success then
         Put_Line (DBC.Error);
      end if;
   end loop;

   DBC.Commit;

   Fetch (RS, DBC, "select * from Keep_Timestamp order by 1");

   for J in Dts'Range loop
      Assert
        (Dts (J) = Time_Value (RS, 0),
         "Check date " & Value (RS, 0) & ' ' &
           GNAT.Calendar.Time_IO.Image
           (Dts (J), "%Y-%m-%d %H:%M:%S.%o")
         & ' ' & Duration'Image (Dts (J) - Time_Value (RS, 0)));

      Next (RS);
   end loop;

   Assert (not Has_Row (RS), "Done.");

   return Report;
end Test;
