------------------------------------------------------------------------------
--                       Database interface utilities                       --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
------------------------------------------------------------------------------

package body Database is
   pragma Style_Checks (Off);

   function FK (Self : T_Action_Item'Class; Foreign : T_Sales_Entity'Class) return SQL_Criteria is
   begin
      return Self.Se_Nb = Foreign.Se_Nb;
   end FK;

   function FK (Self : T_Action_Item'Class; Foreign : T_Staff'Class) return SQL_Criteria is
   begin
      return Self.Who_Done = Foreign.Id;
   end FK;

   function FK (Self : T_Contract'Class; Foreign : T_Sales_Entity'Class) return SQL_Criteria is
   begin
      return Self.Se_Nb = Foreign.Se_Nb;
   end FK;

   function FK (Self : T_Mailing_List'Class; Foreign : T_Tn_Status'Class) return SQL_Criteria is
   begin
      return Self.Default_Status = Foreign.Id;
   end FK;

   function FK (Self : T_Mailing_List_Recipients'Class; Foreign : T_Staff_Email'Class) return SQL_Criteria is
   begin
      return Self.Email = Foreign.Id;
   end FK;

   function FK (Self : T_Mailing_List_Recipients'Class; Foreign : T_Mailing_List'Class) return SQL_Criteria is
   begin
      return Self.List = Foreign.Id;
   end FK;

   function FK (Self : T_Mailing_List_Recipients'Class; Foreign : T_Mailing_List_Subscription_Type'Class) return SQL_Criteria is
   begin
      return Self.Subscription_Type = Foreign.Id;
   end FK;

   function FK (Self : T_Sales_Entity'Class; Foreign : T_Region'Class) return SQL_Criteria is
   begin
      return Self.Region = Foreign.Id;
   end FK;

   function FK (Self : T_Sales_Entity'Class; Foreign : T_Staff'Class) return SQL_Criteria is
   begin
      return Self.Sales_Rep = Foreign.Id;
   end FK;

   function FK (Self : T_Staff'Class; Foreign : T_Staff_Email'Class) return SQL_Criteria is
   begin
      return Self.Preferred_Email = Foreign.Id;
   end FK;

   function FK (Self : T_Staff'Class; Foreign : T_Region'Class) return SQL_Criteria is
   begin
      return Self.Region = Foreign.Id;
   end FK;

   function FK (Self : T_Staff_Email'Class; Foreign : T_Staff'Class) return SQL_Criteria is
   begin
      return Self.Staff = Foreign.Id;
   end FK;

   function FK (Self : T_Subscription'Class; Foreign : T_Contract'Class) return SQL_Criteria is
   begin
      return Self.Subscription_Nb = Foreign.Contract_Nb;
   end FK;

   function FK (Self : T_Wavefront'Class; Foreign : T_Staff'Class) return SQL_Criteria is
   begin
      return Self.Delivered_By = Foreign.Id;
   end FK;

   function FK (Self : T_Wavefront'Class; Foreign : T_Sales_Entity'Class) return SQL_Criteria is
   begin
      return Self.Se_Nb = Foreign.Se_Nb;
   end FK;

   function FK (Self : T_Wavefront'Class; Foreign : T_Wavefront_Status'Class) return SQL_Criteria is
   begin
      return Self.Status = Foreign.Id;
   end FK;

   function FK (Self : T_Wavefront_Tn'Class; Foreign : T_Tracking_Number'Class) return SQL_Criteria is
   begin
      return Self.Tn = Foreign.Tn;
   end FK;

   function FK (Self : T_Wavefront_Tn'Class; Foreign : T_Wavefront'Class) return SQL_Criteria is
   begin
      return Self.Wave = Foreign.Id;
   end FK;
end Database;
