------------------------------------------------------------------------------
--                       Database interface utilities                       --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

with Test_Assert;              use Test_Assert;
with Database;                 use Database;
with Ada.Calendar.Formatting;
with Ada.Text_IO;              use Ada.Text_IO;
with GNATCOLL.SQL.Postgres;
with GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite;

package body GNATCOLL.SQL.Unit_Tests is
   --  use T_Action_Item;

   Name_Foo : aliased constant String := "foo";

   type My_Formatter is new Formatter with null record;
   overriding
   function Field_Type_Autoincrement (Self : My_Formatter) return String;
   overriding
   function Field_Type_Money (Self : My_Formatter) return String;

   overriding
   function Field_Type_Autoincrement (Self : My_Formatter) return String is
      pragma Unreferenced (Self);
   begin
      return "INTEGER";
   end Field_Type_Autoincrement;

   overriding
   function Field_Type_Money (Self : My_Formatter) return String is
      pragma Unreferenced (Self);
   begin
      return "NUMERIC";
   end Field_Type_Money;

   Format : My_Formatter;

   function TS (Self : SQL_Table_Or_List'Class) return String is
   begin
      return To_String (Self, Format);
   end TS;

   function TS
      (Self : SQL_Field'Class; Long : Boolean := True) return String
   is
   begin
      return To_String (Self, Format, Long);
   end TS;

   function TS
     (Self : SQL_Criteria; Long : Boolean := True) return String is
   begin
      return To_String (Self, Format, Long);
   end TS;

   function TS (Self : SQL_Query) return Unbounded_String is
   begin
      return To_String (Self, Format);
   end TS;

   function TS (Self : SQL_Query) return String is
   begin
      return To_String (TS (Self));
   end TS;

   function TS (Self : SQL_Field_List) return String is
   begin
      return To_String (Self, Format);
   end TS;

   ---------------
   -- Do_Tables --
   ---------------

   procedure Do_Tables is
      T1 : T_Action_Item (Instance => Name_Foo'Access);
      T2 : constant SQL_Table_List := T1 & Action_Item;
   begin
      Assert (TS (Action_Item), "action_item", "Normal table");
      Assert (TS (T1), "action_item foo", "Renaming table");
      Assert (TS (Action_Item & T1), "action_item, action_item foo",
              "A list of two tables");
      Assert (TS (T2), "action_item foo, action_item",
              "A list of two tables, through local variable");
      Assert
        (TS (Left_Join
               (Staff, Staff_Email, Staff.Id = Staff_Email.Staff)),
        "(staff LEFT JOIN staff_email ON staff.id=staff_email.staff)",
         "A left join");
      Assert (TS (Rename
                    (Left_Join (Staff, Staff_Email,
                                Staff.Id = Staff_Email.Staff),
                     Name_Foo'Access)),
        "(staff LEFT JOIN staff_email"
        & " ON staff.id=staff_email.staff) foo",
        "A renamed left join");
   end Do_Tables;

   ---------------
   -- Do_Fields --
   ---------------

   procedure Do_Fields is
      T1 : T_Action_Item (Instance => Name_Foo'Access);
   begin
      Assert (TS (Action_Item.Date_Done), "action_item.date_done",
              "Normal field");
      Assert (TS (T1.Date_Done), "foo.date_done", "Time field through rename");
      Assert
        (TS (T1.Priority), "foo.priority", "Integer field through rename");
      Assert (TS (T1.What_Done), "foo.what_done", "Text field through rename");
      Assert (TS (As (T1.Priority, "PRIO")),
              "foo.priority AS PRIO", "Renamed field");
      Assert (TS (T1.Priority & Action_Item.What_Done),
              "foo.priority, action_item.what_done",
              "printing list of fields");
      Assert (TS (To_List
                   ((0 => +T1.Priority,
                     1 => +Action_Item.What_Done))),
              "foo.priority, action_item.what_done",
              "printing list of fields through array");
      Assert (TS ((T1.Priority & Action_Item.What_Done) & T1.Date_Done),
              "foo.priority, action_item.what_done, foo.date_done",
              "Concatenating two lists of strings => no parenthesis");
      Assert (TS (T1.Priority & 5), "foo.priority, 5",
              "field list with integer");
      Assert (TS (T1.Priority & "value'foo"), "foo.priority, 'value''foo'",
              "field list with string");
      Assert (TS (Concat ("prefix" & T1.Priority & "suffix")),
              "'prefix' || foo.priority || 'suffix'",
              "Concatenating a string");
      declare
         C : constant SQL_Field'Class := Concat ("prefix" & T1.Priority);
      begin
         Assert (TS (Concat (C & "suffix")),
              "'prefix' || foo.priority || 'suffix'",
              "Concatenating a string through tmp var");
         Assert (TS (C & "suffix"),
              "'prefix' || foo.priority, 'suffix'",
              "Concatenating a string in a list");
      end;

      Assert (TS (Apply (Func_Min, T1.Priority)),
              "min (foo.priority)", "applying aggregate with single arg");
      Assert
        (TS (Apply ("group_concat",
                    T1.Priority & Concat ("A" & T1.What_Done))),
         "group_concat (foo.priority, 'A' || foo.what_done)",
         "Applying aggregate with two args");

      Assert
        (TS (Apply ("group_concat", T1.Priority, Order_By => T1.Priority)),
         "group_concat (foo.priority ORDER BY foo.priority)",
         "Applying aggregate with ORDER BY");

      Assert (TS (Extract (Action_Item.Date_Done, "year")),
              "EXTRACT (year from action_item.date_done)",
              "Using the extract function");

      Assert (TS (Tuple (Action_Item.Date_Done & Action_Item.Date_Done)),
              "(action_item.date_done, action_item.date_done)",
              "Using the tuple function");

      Assert (TS (Coalesce (Concat (Action_Item.Who_Done
                                    & " done")
                            & Action_Item.Date_Done)),
        "COALESCE (action_item.who_done || ' done'," &
        " action_item.date_done)",
        "Nesting coalesce in concat");
   end Do_Fields;

   -----------------
   -- Do_Criteria --
   -----------------

   procedure Do_Criteria is
      T1 : T_Action_Item (Instance => Name_Foo'Access);
      C  : SQL_Criteria;
      L  : Criteria_List;
   begin
      Assert (Length (C), 0, "Length");
      Assert (not Is_Or (C), "not OR");
      Assert (not Is_And (C), "not And");

      C := T1.Priority = T1.Status;
      Assert (TS (C), "foo.priority=foo.status", "simple test");
      Assert (Length (C), 1, "Length");
      Assert (not Is_Or (C), "not OR");
      Assert (not Is_And (C), "not And");

      C := C and Action_Item.Priority = T1.Priority;
      Assert (TS (C),
              "foo.priority=foo.status"
              & " AND action_item.priority=foo.priority",
              "simple test with AND");
      Assert (Length (C), 2, "Length");
      Assert (Is_And (C), "is AND");
      Assert (not Is_Or (C), "not OR");

      C := C or T1.What_Done = Expression ("value");
      Assert (TS (C),
              "(foo.priority=foo.status"
              & " AND action_item.priority=foo.priority)"
              & " OR foo.what_done='value'",
              "Mixing AND and OR");
      Assert (Length (C), 2, "Length");
      Assert (not Is_And (C), "not AND");
      Assert (Is_Or (C), "is OR");

      C := Config.Sales_Message = True;
      Assert (TS (C), "config.sales_message", "a boolean test for true");
      Assert (Length (C), 1, "Length");
      Assert (not Is_And (C), "not AND");
      Assert (not Is_Or (C), "not OR");

      C := T1.Priority = T1.Status and Config.Sales_Message;
      Assert (TS (C), "foo.priority=foo.status"
              & " AND config.sales_message",
              "a simplified boolean test for true");

      C := Config.Sales_Message = False;
      Assert (TS (C), "not config.sales_message", "a boolean test for false");
      C := T1.Priority = T1.Status and not Config.Sales_Message;
      Assert (TS (C), "foo.priority=foo.status"
              & " AND not config.sales_message",
              "a simplified boolean test for false");

      C := SQL_Between (T1.Status, T1.Priority, T1.Id);
      Assert (TS (C), "foo.status BETWEEN foo.priority AND foo.id",
              "between condition test");
      Assert (Length (C), 1, "Condition length for single between");

      C := SQL_Not_Between (T1.Id, T1.Status, T1.Priority);
      Assert (TS (C), "foo.id NOT BETWEEN foo.status AND foo.priority",
              "not between condition test");
      Assert (Length (C), 1, "Condition length for single not between");

      C := No_Criteria;
      Assert (Length (C), 0, "Condition length for empty criteria");

      C :=  C or (T1.Priority = T1.Status and
                  Action_Item.Priority = T1.Priority);
      Assert (TS (C), "foo.priority=foo.status AND"
              & " action_item.priority=foo.priority");
      Assert (Length (C), 2, "Condition length over 'AND'");

      C :=  C or (T1.What_Done = Expression ("value")
                  and Config.Sales_Message = False);
      Assert (TS (C), "(foo.priority=foo.status AND"
              & " action_item.priority=foo.priority) OR"
              & " (foo.what_done='value' AND not config.sales_message)");
      Assert (Length (C), 2, "Condition length over 'OR'");

      C :=  C or (T1.What_Done = Expression ("value")
                  and Action_Item.Priority = Expression (2));
      Assert (TS (C, False), "(foo.priority=foo.status AND"
              & " action_item.priority=foo.priority) OR"
              & " (foo.what_done='value' AND not config.sales_message) OR"
              & " (foo.what_done='value' AND action_item.priority=2)");
      Assert (Length (C), 3, "One more condition length over 'OR'");
      Assert (not Is_And (C), "not AND");
      Assert (Is_Or (C), "is OR");

      L.Append (T1.What_Done = Expression ("value"));
      L.Append (Action_Item.Priority = Expression (2));
      L.Append (T1.What_Done = Expression ("value")
                  and Config.Sales_Message = False);
      L.Append (T1.Priority = T1.Status and
                  Action_Item.Priority = T1.Priority);
      L.Append (T1.Priority = T1.Status or Config.Sales_Message);
      Assert
        (TS (Combine (L, Criteria_And)),
         "foo.what_done='value' AND action_item.priority=2 AND"
         & " (foo.what_done='value' AND not config.sales_message) AND"
         & " (foo.priority=foo.status AND action_item.priority=foo.priority)"
         & " AND (foo.priority=foo.status OR config.sales_message)");
      Assert
        (TS (Combine (L, Criteria_Or)),
         "foo.what_done='value' OR action_item.priority=2 OR"
         & " (foo.what_done='value' AND not config.sales_message) OR"
         & " (foo.priority=foo.status AND action_item.priority=foo.priority)"
         & " OR (foo.priority=foo.status OR config.sales_message)");

      C := SQL_In (T1.What_Done, "");
      Assert (TS (C), "FALSE");

      C := SQL_In (T1.What_Done, Empty_Field_List);
      Assert (TS (C), "FALSE");

      C := SQL_Not_In (T1.What_Done, "");
      Assert (TS (C), "TRUE");

      C := SQL_Not_In (T1.What_Done, Empty_Field_List);
      Assert (TS (C), "TRUE");
   end Do_Criteria;

   ----------------
   -- Do_Queries --
   ----------------

   procedure Do_Queries is
      Q, Q2, Q3 : SQL_Query;
   begin
      Q := SQL_Select
        (Fields => Action_Item.Priority,
         From   => Action_Item,
         Where  => Action_Item.Priority = 5);
      Assert
        (TS (Q), "SELECT action_item.priority"
         & " FROM action_item WHERE action_item.priority=5",
         "simple select query");

      Q := SQL_Select
        (Fields => Action_Item.Priority & Action_Item.What_Done,
         From   => Action_Item,
         Where  => Action_Item.Priority = 5);
      Assert
        (TS (Q), "SELECT action_item.priority, action_item.what_done"
         & " FROM action_item WHERE action_item.priority=5",
         "simple select query with multiple fields");

      Q := SQL_Select
        (Fields => Action_Item.Priority
                   & Apply (Func_Min, Action_Item.What_Done),
         From   => Action_Item,
         Auto_Complete => True);
      Assert
         (TS (Q),
          "SELECT action_item.priority, min (action_item.what_done)"
          & " FROM action_item GROUP BY action_item.priority",
          "query with 'group by' autocomplete");

      Q := SQL_Select
        (Fields   => Action_Item.Priority & Action_Item.What_Done,
         From     => Action_Item,
         Where    => Action_Item.Priority = 5,
         Distinct => True,
         Order_By => Action_Item.Priority);
      Assert
        (TS (Q), "SELECT DISTINCT action_item.priority, action_item.what_done"
         & " FROM action_item WHERE action_item.priority=5"
         & " ORDER BY action_item.priority",
         "simple select query with multiple fields and distinct");

      Q := SQL_Select
        (Fields    => Staff_Email.Email_Address,
         From      => Staff_Email & Staff,
         Where     => Staff_Email.FK (Staff));
      Assert
        (TS (Q), "SELECT staff_email.email_address FROM staff_email, staff"
         & " WHERE staff_email.staff=staff.id",
         "Query with foreign key");

      Q := SQL_Select
        (Fields    => To_List
           ((0 => +Mailing_List_Subscription_Type.Name,
             1 => +Staff.Login,
             2 => +Mailing_List_Recipients.Email)),
         From     => Staff_Email & Staff & Mailing_List_Subscription_Type
           & Mailing_List_Recipients,
         Where    => Mailing_List_Recipients.Subscription_Type =
           Mailing_List_Subscription_Type.Id
           and Mailing_List_Recipients.Email = Staff_Email.Id
           and Staff_Email.Staff = Staff.Id
           and Mailing_List_Recipients.List = 2,
         Order_By => Staff.Login);
      Assert
        (TS (Q),
         "SELECT mailing_list_subscription_type.""name"","
         & " staff.login,"
         & " mailing_list_recipients.email"
         & " FROM staff_email, staff, mailing_list_subscription_type,"
         & " mailing_list_recipients"
         & " WHERE mailing_list_recipients.subscription_type="
         & "mailing_list_subscription_type.id"
         & " AND mailing_list_recipients.email="
         & "staff_email.id"
         & " AND staff_email.staff=staff.id"
         & " AND mailing_list_recipients.list=2"
         & " ORDER BY staff.login",
         "Complex query from crm_lists.adb");

      Q := SQL_Insert ((Region.Name = "foo") & (Region.Id = 4));
      Assert
        (TS (Q),
         "INSERT INTO region (""name"", id) VALUES ('foo', 4)",
         "Auto-complete insert query");

      Q := SQL_Insert ((Action_Item.Se_Nb = Contract.Se_Nb)
                         & (Action_Item.Status = 0)
                         & (Action_Item.Who_Done = Staff.Id),
                       Where => Contract.Contract_Nb = 2007062701
                         and Staff.Login = "briot");
      Assert
        (TS (Q),
         "INSERT INTO action_item (se_nb, status, who_done)"
         & " SELECT contract.se_nb, 0, staff.id FROM staff, contract"
         & " WHERE contract.contract_nb=2007062701 AND staff.login='briot'",
         "Transform insert with assignment into insert with subquery");

      begin
         Q := SQL_Insert ((Action_Item.Se_Nb = 422)
                          & (Staff.Id = Action_Item.Who_Done),
                          Where => Staff.Login = "briot");
         Assert (TS (Q), "FAILED",
                 "Ambiguous insert statement impacting two tables");
      exception
         when Program_Error =>
            null;
      end;

      Q2 := SQL_Select
        (Concat ("new=" & Region.Name),
         Where => Region.Id = 1);
      Auto_Complete (Q2);

      Q := SQL_Insert (Region.Name, Q2);
      Assert
        (TS (Q),
         "INSERT INTO region (""name"") SELECT 'new=' || region.""name"""
         & " FROM region WHERE region.id=1",
         "Auto-complete insert query");

      Q := SQL_Insert (Staff.Login = Null_Field_Text);
      Assert
        (TS (Q), "INSERT INTO staff (login) VALUES (NULL)",
         "Setting a field to NULL");

      Q := SQL_Insert ((Staff.Login = Null_Field_Text)
                       & (Staff.Id = Region.Id),
                       Where => Region.Id = 0);
      Auto_Complete (Q);
      Assert
        (TS (Q), "INSERT INTO staff (login, id)"
         & " SELECT NULL, region.id FROM region"
         & " WHERE region.id=0",
         "Setting a field to NULL");

      Q := SQL_Update
        (Staff, Staff.Salary = 666.66, Where => Staff.Id = 1);
      Assert
        (TS (Q),
         "UPDATE staff SET salary=666.66 WHERE staff.id=1",
         "Test an update onto staff salary");

      Q := SQL_Update
        (Region, Region.Name = "foo", Where => Region.Id = 1);
      Assert
        (TS (Q),
         "UPDATE region SET ""name""='foo' WHERE region.id=1",
         "Simple update query");

      Q := SQL_Update
        (Region, Region.Name = "foo",
         Where => Region.Id = Staff.Region
            and Staff.Login = "foo");
      Auto_Complete (Q);
      Assert
        (TS (Q),
         "UPDATE region SET ""name""='foo' FROM staff"
         & " WHERE region.id=staff.region AND staff.login='foo'",
         "auto-complete update with from");

      --  Test that we can extend queries without impacting the original
      --  query

      begin
         Q := SQL_Insert (Staff.Login = Null_Field_Text);
         Q := Q.Where_And (Staff.Login = Null_Field_Text);  --  illegal
         Assert ("No exception", "An exception",
                 "Shouldn't be able to modify an INSERT, only SELECT");
      exception
         when Program_Error =>
            null;
      end;

      Q := SQL_Select (Staff.Login, Staff);
      Q2 := Q.Where_And (Staff.Id = 1);
      Q3 := Q2.Where_And (Staff.Region = 2);
      Assert (TS (Q), "SELECT staff.login FROM staff",
              "Unmodified original query");
      Assert (TS (Q2),
              "SELECT staff.login FROM staff WHERE staff.id=1",
              "Extended query (1)");
      Assert (TS (Q3),
              "SELECT staff.login FROM staff WHERE staff.id=1"
              & " AND staff.region=2",
              "Extended query (2)");

      Q2 := Q.Order_By (Staff.Login);
      Q3 := Q2.Order_By (Staff.Id);
      Assert (TS (Q), "SELECT staff.login FROM staff",
              "Unmodified original query");
      Assert (TS (Q2),
              "SELECT staff.login FROM staff ORDER BY staff.login",
              "Extended order_by (1)");
      Assert (TS (Q3),
              "SELECT staff.login FROM staff ORDER BY staff.id,"
              & " staff.login",
              "Extended order_by (2)");

      Assert (TS (SQL_Create_Table ("tbl", As => Q)),
              "CREATE TABLE tbl AS (SELECT staff.login FROM staff)");
      Assert (TS (SQL_Create_Table (Temp => True, Name => "tbl", As => Q)),
              "CREATE TEMPORARY TABLE tbl ON COMMIT PRESERVE ROWS"
              & " AS (SELECT staff.login FROM staff)");
      Assert (TS (SQL_Create_Table (Temp => True, Name => "tbl",
                              On_Commit => Drop, As => Q)),
              "CREATE TEMPORARY TABLE tbl ON COMMIT DROP"
              & " AS (SELECT staff.login FROM staff)");
   end Do_Queries;

   -----------------
   -- Do_Postgres --
   -----------------

   --  Postgres specific extensions

   procedure Do_Postgres is
      use GNATCOLL.SQL.Postgres;
      Q : SQL_Query;
   begin
      Q := SQL_Update
             (Staff, Staff.Salary = 666.66, Where => Staff.Id = 1)
           & Returning (Fields => Staff.Salary & Staff.Id);
      Assert
        (TS (Q),
         "UPDATE staff SET salary=666.66 WHERE staff.id=1"
         & " RETURNING staff.salary, staff.id",
         "Test an update onto staff salary, returning updated info");

      Q := SQL_Select (Staff.Login, Staff)
           & For_Update;
      Assert (TS (Q),
              "SELECT staff.login FROM staff FOR UPDATE",
              "For update (no table)");

      Q := Q & For_Update (+Staff);
      Assert (TS (Q),
              "SELECT staff.login FROM staff FOR UPDATE OF staff",
              "For update (with table)");

      Q := Q & For_Update (No_Wait => True);
      Assert (TS (Q),
              "SELECT staff.login FROM staff FOR UPDATE OF staff NO WAIT",
              "For update (with table, no wait)");

      Q := SQL_Select (Staff.Login, Staff)
           & For_Update (No_Wait => True);
      Assert (TS (Q),
              "SELECT staff.login FROM staff FOR UPDATE NO WAIT",
              "For update (no table, no wait)");
   end Do_Postgres;

   -------------------
   -- Do_Parameters --
   -------------------

   procedure Do_Parameters is
      use GNATCOLL.SQL, GNATCOLL.SQL.Exec, Ada.Calendar;

      Now    : constant Time := Clock;
      Value  : constant Float := 5.25;
      Price  : constant T_Money := 24567.17;
      Params : SQL_Parameters :=
         (1 => +"string", 2 => +2, 3 => As_Bigint (3), 4 => +True,
          5 => +Value, 6 => As_Long_Float (6.125), 7 => +'U', 8 => +Price,
          9 => <>, 10 => +Now);
      Memory : constant Database_Connection :=
                 GNATCOLL.SQL.Sqlite.Setup (":memory:").Build_Connection;
   begin
      Assert
         (Image (Memory.all, Params),
          ", string, 2, 3, 1, 5.25000E+00, 6.12500000000000E+00, U,"
          & " 2456717, NULL, " & Formatting.Image (Now));
   end Do_Parameters;

   --------------
   -- Do_Tests --
   --------------

   procedure Do_Tests is
   begin
      Do_Tables;
      Do_Fields;
      Do_Criteria;
      Do_Queries;
      Do_Postgres;
      Do_Parameters;
   end Do_Tests;

end GNATCOLL.SQL.Unit_Tests;
