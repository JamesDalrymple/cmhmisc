

select Case_No, MIN(statusDate) as first_status_date into #tmp1
from tblE2_Consumer_Diagnosis_HIT_w_ICD10 diag
where diag.sequence = 1 
group by Case_No


-- 3366, 1556, 1066
select distinct CMH.team, CMH.Case_No, CMH_EFFDT, first_status_date, datediff( dd, CMH.CMH_EFFDT, first_status_date) as num_days_missed, 
Case when Enc.Case_no is not null then 'Y' else 'N' end as Encounters_btw_dates

from  tblE2_CMH_Open_Consumers CMH
left join #tmp1 diag on diag.Case_No = CMH.Case_No 
left join tblE2_Encounter_Data_MH ENC on Enc.Case_no = CMH.Case_No and ENc.CL_FRMDT between CMH.CMH_EFFDT and cast(first_status_date as datetime) -1
--and CMH.CMH_EFFDT >= first_status_date
where CMH.county = 'washtenaw'
--and first_status_date is null -- 10
and ( CMH.CMH_EFFDT < first_status_date or first_status_date is null )
--and CMH_EFFDT >= '10/1/14'
order by 1,2 


declare @county varchar(10) = 'washtenaw', @startdate datetime = '10/1/2014', @enddate datetime = '10/30/16'
select distinct  *
from E2_fn_Contracted_Providers ( @county, @startdate, @enddate )
where Provider = 'Avalon Housing'
and CP_CODE = 'H2015'
order by 1, 2
go


select distinct Member_ID_Type, Related_Plan_Name_type, Related_Plan_Name, Originator_Plan_Name_Type, Originator_Plan_Name, *
from Frank_data.dbo.tblCC360_sinceFY14_w_WCHO CC
where Service_From_Date between '10/1/15' and '9/30/16'
and Team is not null
and Originator_Plan_Name_Type= 'CMH'
--and Member_ID_Type = 'Community Mental Health (CMH)' --1 yr 783,082 MH services
--and Originator_Plan_Name_Type = 'PHP'
and Related_Plan_Name = 'CMH Partnership of SE MI' --1 yr, 742991  MH services, 
and Billing_Provider_Name is null
and Originator_Plan_Name = 'Washtenaw Com Hlth Org'
order by Service_From_Date desc
--and Procedure_Code like 'J%'
--and Invoice_Type = 'Professional'
--where Procedure_Code in ( 'H0018') -- 'H0036' 
go

-- tblCC360_FY14_15_w_WCHO


select distinct Member_ID_Type, Related_Plan_Name_type, Related_Plan_Name, Originator_Plan_Name_Type, Originator_Plan_Name, *
from Frank_data.dbo.tblCC360_FY14_15_w_WCHO CC
where Service_From_Date between '10/1/14' and '9/30/15'
and Team is not null
and CC.Case_no = 10032
and Originator_Plan_Name_Type= 'CMH'
--and Member_ID_Type = 'Community Mental Health (CMH)' --1 yr 783,082 MH services
--and Originator_Plan_Name_Type = 'PHP'
and Related_Plan_Name = 'CMH Partnership of SE MI' --1 yr, 742991  MH services, 
--and Billing_Provider_Name is null
and Originator_Plan_Name = 'Washtenaw Com Hlth Org'
and Procedure_Code = 'H2015'
order by Service_From_Date 
go



select *
from tblE2_Encounter_Data_MH
where Case_no = 10032
and CL_FRMDT >= '9/1/16'
and primaryProcedure = 'H2015'
--where Encounter_Type = 'SAL' -- Diagnosis_Pointer = '0' --
ORDER BY	CL_FRMDT
go
