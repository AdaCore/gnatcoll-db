with Colorspkg; use Colorspkg;
with Test_Assert;
function Test return Integer is
   package A renames Test_Assert;
begin
   A.Assert (Image_Color_Id (Color_Dark_Gray) = "dark gray");
   return A.Report;
end Test;
