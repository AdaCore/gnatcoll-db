------------------------------------------------------------------------------
--                       Database interface utilities                       --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
------------------------------------------------------------------------------

with GNATCOLL.SQL; use GNATCOLL.SQL;
pragma Warnings (Off, "no entities of * are referenced");
pragma Warnings (Off, "use clause for package * has no effect");
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
pragma Warnings (On, "no entities of * are referenced");
pragma Warnings (On, "use clause for package * has no effect");
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Action_Item
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Action_Item, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Action_Item, Instance, N_Id, Index);
      Date_Done : SQL_Field_Time (Ta_Action_Item, Instance, N_Date_Done, Index);
      Priority : SQL_Field_Integer (Ta_Action_Item, Instance, N_Priority, Index);
      Se_Nb : SQL_Field_Integer (Ta_Action_Item, Instance, N_Se_Nb, Index);
      Status : SQL_Field_Integer (Ta_Action_Item, Instance, N_Status, Index);
      What_Done : SQL_Field_Text (Ta_Action_Item, Instance, N_What_Done, Index);
      Who_Done : SQL_Field_Integer (Ta_Action_Item, Instance, N_Who_Done, Index);
   end record;

   type T_Action_Item (Instance : Cst_String_Access)
      is new T_Abstract_Action_Item (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Action_Item (Index : Integer)
      is new T_Abstract_Action_Item (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Config
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Config, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Config, Instance, N_Id, Index);
      Sales_Message : SQL_Field_Boolean (Ta_Config, Instance, N_Sales_Message, Index);
   end record;

   type T_Config (Instance : Cst_String_Access)
      is new T_Abstract_Config (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Config (Index : Integer)
      is new T_Abstract_Config (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Contract
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Contract, Instance, Index) with
   record
      Contract_Nb : SQL_Field_Integer (Ta_Contract, Instance, N_Contract_Nb, Index);
      Contract_Type : SQL_Field_Integer (Ta_Contract, Instance, N_Contract_Type, Index);
      Se_Nb : SQL_Field_Integer (Ta_Contract, Instance, N_Se_Nb, Index);
   end record;

   type T_Contract (Instance : Cst_String_Access)
      is new T_Abstract_Contract (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Contract (Index : Integer)
      is new T_Abstract_Contract (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Mailing_List
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Mailing_List, Instance, Index) with
   record
      Default_Status : SQL_Field_Integer (Ta_Mailing_List, Instance, N_Default_Status, Index);
      Id : SQL_Field_Integer (Ta_Mailing_List, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_Mailing_List, Instance, N_Name, Index);
   end record;

   type T_Mailing_List (Instance : Cst_String_Access)
      is new T_Abstract_Mailing_List (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Mailing_List (Index : Integer)
      is new T_Abstract_Mailing_List (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Mailing_List_Recipients
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Mailing_List_Recipients, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Mailing_List_Recipients, Instance, N_Id, Index);
      Email : SQL_Field_Integer (Ta_Mailing_List_Recipients, Instance, N_Email, Index);
      List : SQL_Field_Integer (Ta_Mailing_List_Recipients, Instance, N_List, Index);
      Subscription_Type : SQL_Field_Integer (Ta_Mailing_List_Recipients, Instance, N_Subscription_Type, Index);
   end record;

   type T_Mailing_List_Recipients (Instance : Cst_String_Access)
      is new T_Abstract_Mailing_List_Recipients (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Mailing_List_Recipients (Index : Integer)
      is new T_Abstract_Mailing_List_Recipients (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Mailing_List_Subscription_Type
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Mailing_List_Subscription_Type, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Mailing_List_Subscription_Type, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_Mailing_List_Subscription_Type, Instance, N_Name, Index);
   end record;

   type T_Mailing_List_Subscription_Type (Instance : Cst_String_Access)
      is new T_Abstract_Mailing_List_Subscription_Type (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Mailing_List_Subscription_Type (Index : Integer)
      is new T_Abstract_Mailing_List_Subscription_Type (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Region
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Region, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Region, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_Region, Instance, N_Name, Index);
   end record;

   type T_Region (Instance : Cst_String_Access)
      is new T_Abstract_Region (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Region (Index : Integer)
      is new T_Abstract_Region (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Sales_Entity
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Sales_Entity, Instance, Index) with
   record
      Act_Nb : SQL_Field_Integer (Ta_Sales_Entity, Instance, N_Act_Nb, Index);
      Date_Created : SQL_Field_Date (Ta_Sales_Entity, Instance, N_Date_Created, Index);
      Region : SQL_Field_Integer (Ta_Sales_Entity, Instance, N_Region, Index);
      Sales_Rep : SQL_Field_Integer (Ta_Sales_Entity, Instance, N_Sales_Rep, Index);
      Se_Nb : SQL_Field_Integer (Ta_Sales_Entity, Instance, N_Se_Nb, Index);
   end record;

   type T_Sales_Entity (Instance : Cst_String_Access)
      is new T_Abstract_Sales_Entity (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Sales_Entity (Index : Integer)
      is new T_Abstract_Sales_Entity (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Staff
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Staff, Instance, Index) with
   record
      Active : SQL_Field_Boolean (Ta_Staff, Instance, N_Active, Index);
      Id : SQL_Field_Integer (Ta_Staff, Instance, N_Id, Index);
      Login : SQL_Field_Text (Ta_Staff, Instance, N_Login, Index);
      Preferred_Email : SQL_Field_Integer (Ta_Staff, Instance, N_Preferred_Email, Index);
      Region : SQL_Field_Integer (Ta_Staff, Instance, N_Region, Index);
      Salary : SQL_Field_Money (Ta_Staff, Instance, N_Salary, Index);
   end record;

   type T_Staff (Instance : Cst_String_Access)
      is new T_Abstract_Staff (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Staff (Index : Integer)
      is new T_Abstract_Staff (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Staff_Email
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Staff_Email, Instance, Index) with
   record
      Email_Address : SQL_Field_Text (Ta_Staff_Email, Instance, N_Email_Address, Index);
      Id : SQL_Field_Integer (Ta_Staff_Email, Instance, N_Id, Index);
      Staff : SQL_Field_Integer (Ta_Staff_Email, Instance, N_Staff, Index);
   end record;

   type T_Staff_Email (Instance : Cst_String_Access)
      is new T_Abstract_Staff_Email (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Staff_Email (Index : Integer)
      is new T_Abstract_Staff_Email (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Subscription
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Subscription, Instance, Index) with
   record
      Subscription_Nb : SQL_Field_Integer (Ta_Subscription, Instance, N_Subscription_Nb, Index);
      Contract_Type : SQL_Field_Integer (Ta_Subscription, Instance, N_Contract_Type, Index);
   end record;

   type T_Subscription (Instance : Cst_String_Access)
      is new T_Abstract_Subscription (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Subscription (Index : Integer)
      is new T_Abstract_Subscription (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Tn_Status
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Tn_Status, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Tn_Status, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_Tn_Status, Instance, N_Name, Index);
   end record;

   type T_Tn_Status (Instance : Cst_String_Access)
      is new T_Abstract_Tn_Status (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Tn_Status (Index : Integer)
      is new T_Abstract_Tn_Status (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Tracking_Number
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Tracking_Number, Instance, Index) with
   record
      Assignee : SQL_Field_Integer (Ta_Tracking_Number, Instance, N_Assignee, Index);
      Created_By : SQL_Field_Integer (Ta_Tracking_Number, Instance, N_Created_By, Index);
      Tn : SQL_Field_Text (Ta_Tracking_Number, Instance, N_Tn, Index);
   end record;

   type T_Tracking_Number (Instance : Cst_String_Access)
      is new T_Abstract_Tracking_Number (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Tracking_Number (Index : Integer)
      is new T_Abstract_Tracking_Number (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Wavefront
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Wavefront, Instance, Index) with
   record
      Comment : SQL_Field_Text (Ta_Wavefront, Instance, N_Comment, Index);
      Delivered_By : SQL_Field_Integer (Ta_Wavefront, Instance, N_Delivered_By, Index);
      Delivery_Date : SQL_Field_Date (Ta_Wavefront, Instance, N_Delivery_Date, Index);
      Id : SQL_Field_Integer (Ta_Wavefront, Instance, N_Id, Index);
      Request_Date : SQL_Field_Date (Ta_Wavefront, Instance, N_Request_Date, Index);
      Requested_By : SQL_Field_Integer (Ta_Wavefront, Instance, N_Requested_By, Index);
      Se_Nb : SQL_Field_Integer (Ta_Wavefront, Instance, N_Se_Nb, Index);
      Status : SQL_Field_Integer (Ta_Wavefront, Instance, N_Status, Index);
   end record;

   type T_Wavefront (Instance : Cst_String_Access)
      is new T_Abstract_Wavefront (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Wavefront (Index : Integer)
      is new T_Abstract_Wavefront (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Wavefront_Status
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Wavefront_Status, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Wavefront_Status, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_Wavefront_Status, Instance, N_Name, Index);
   end record;

   type T_Wavefront_Status (Instance : Cst_String_Access)
      is new T_Abstract_Wavefront_Status (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Wavefront_Status (Index : Integer)
      is new T_Abstract_Wavefront_Status (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Wavefront_Tn
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Wavefront_Tn, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Wavefront_Tn, Instance, N_Id, Index);
      Tn : SQL_Field_Text (Ta_Wavefront_Tn, Instance, N_Tn, Index);
      Wave : SQL_Field_Integer (Ta_Wavefront_Tn, Instance, N_Wave, Index);
   end record;

   type T_Wavefront_Tn (Instance : Cst_String_Access)
      is new T_Abstract_Wavefront_Tn (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Wavefront_Tn (Index : Integer)
      is new T_Abstract_Wavefront_Tn (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   function FK (Self : T_Action_Item'Class; Foreign : T_Sales_Entity'Class) return SQL_Criteria;
   function FK (Self : T_Action_Item'Class; Foreign : T_Staff'Class) return SQL_Criteria;
   function FK (Self : T_Contract'Class; Foreign : T_Sales_Entity'Class) return SQL_Criteria;
   function FK (Self : T_Mailing_List'Class; Foreign : T_Tn_Status'Class) return SQL_Criteria;
   function FK (Self : T_Mailing_List_Recipients'Class; Foreign : T_Staff_Email'Class) return SQL_Criteria;
   function FK (Self : T_Mailing_List_Recipients'Class; Foreign : T_Mailing_List'Class) return SQL_Criteria;
   function FK (Self : T_Mailing_List_Recipients'Class; Foreign : T_Mailing_List_Subscription_Type'Class) return SQL_Criteria;
   function FK (Self : T_Sales_Entity'Class; Foreign : T_Region'Class) return SQL_Criteria;
   function FK (Self : T_Sales_Entity'Class; Foreign : T_Staff'Class) return SQL_Criteria;
   function FK (Self : T_Staff'Class; Foreign : T_Staff_Email'Class) return SQL_Criteria;
   function FK (Self : T_Staff'Class; Foreign : T_Region'Class) return SQL_Criteria;
   function FK (Self : T_Staff_Email'Class; Foreign : T_Staff'Class) return SQL_Criteria;
   function FK (Self : T_Subscription'Class; Foreign : T_Contract'Class) return SQL_Criteria;
   function FK (Self : T_Wavefront'Class; Foreign : T_Staff'Class) return SQL_Criteria;
   function FK (Self : T_Wavefront'Class; Foreign : T_Sales_Entity'Class) return SQL_Criteria;
   function FK (Self : T_Wavefront'Class; Foreign : T_Wavefront_Status'Class) return SQL_Criteria;
   function FK (Self : T_Wavefront_Tn'Class; Foreign : T_Tracking_Number'Class) return SQL_Criteria;
   function FK (Self : T_Wavefront_Tn'Class; Foreign : T_Wavefront'Class) return SQL_Criteria;
   Action_Item : T_Action_Item (null);
   Config : T_Config (null);
   Contract : T_Contract (null);
   Mailing_List : T_Mailing_List (null);
   Mailing_List_Recipients : T_Mailing_List_Recipients (null);
   Mailing_List_Subscription_Type : T_Mailing_List_Subscription_Type (null);
   Region : T_Region (null);
   Sales_Entity : T_Sales_Entity (null);
   Staff : T_Staff (null);
   Staff_Email : T_Staff_Email (null);
   Subscription : T_Subscription (null);
   Tn_Status : T_Tn_Status (null);
   Tracking_Number : T_Tracking_Number (null);
   Wavefront : T_Wavefront (null);
   Wavefront_Status : T_Wavefront_Status (null);
   Wavefront_Tn : T_Wavefront_Tn (null);
end Database;
