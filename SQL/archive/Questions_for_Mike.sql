use Frank_data

select   Case_no, Service_From_Date, --Originator_Plan_Name_Type, 
Procedure_Code, Header_diagnosis_code1, Billing_Provider_Name, Rendering_Provider_Name, Originator_Plan_Name, Originator_Plan_Name_Type, 
Related_Plan_ID,
Related_Plan_Name, 
Related_Plan_Name_Type,
Transaction_Type,
paid_units


select *
from Frank_data.dbo.tblCC360_sinceFY14_w_WCHO CC
where Service_From_Date = '2015-10-06' -->= '1/1/15'

order by --Service_From_Date, 
 Revenue_Code, Procedure_Code



-- select distinct Billing_Provider_Name

select distinct Place_of_Service, Facility_Type, --Billing_Provider_Name, Rendering_Provider_Name, 
Procedure_Code, 
Revenue_Code
from Frank_data.dbo.tblCC360_sinceFY14_w_WCHO CC
where Service_From_Date >= '1/1/15'
--and Case_no = 10442
and Originator_Plan_Name_Type is null

and Revenue_Code is null
order by --Service_From_Date, 
 Revenue_Code, Procedure_Code

-- 2015-10-06 , 2016-01-12

UM 350000
vs non UM  28000

