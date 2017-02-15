
select distinct primaryProcedure, MOD1, --MedicaidProviderType, 
--BillingProvider,
case when LEFT( primaryProcedure, 2) = '01' then dbo.E2_Fn_county_AllowAmounts_byCode_byFile ('FY15 WASHTENAW CONTRACT RATES', '0100', MedicaidProviderType, CL_FRMDT)
  else dbo.E2_Fn_county_AllowAmounts_byCode_byFile ('FY15 WASHTENAW CONTRACT RATES', primaryProcedure, MOD1, CL_FRMDT) end as unit_rate,
Paid_units,  Paid_amt


--select distinct MedicaidProviderType
from tblE2_Encounter_Data_MH
where county = 'washtenaw'
and CL_FRMDT between '10/1/14' and '9/30/16'
and Encounter_Type = 'Claim' -- Diagnosis_Pointer = '0' --
--and LEFT( primaryProcedure, 1) = '0'
order by 1
union
--
from tblE2_Encounter_Data_MH
where county = 'washtenaw'
and CL_FRMDT between '10/1/14' and '9/30/16'
and Encounter_Type = 'SAL' -- Diagnosis_Pointer = '0' --
--and LEFT( primaryProcedure, 1) = '0'
go




select distinct primaryProcedure, MOD1
from tblE2_Encounter_Data_MH
where county = 'washtenaw'
and CL_FRMDT between '10/1/14' and '9/30/16'
and Encounter_Type = 'Claim' -- Diagnosis_Pointer = '0' --
and LEFT( primaryProcedure, 1) = '0'
order by 1, 2

--
0114 	NULL
0114 	ST
0114 	CD
0124 	ST
0124 	NULL
0124 	CD
0134 	NULL
0901 	NULL
0912 	NULL


 -- '22', '68', '73'
 select dbo.E2_Fn_county_AllowAmounts_byCode_byFile ('FY15 WASHTENAW CONTRACT RATES', '0100', '22', '10/1/15')
  select dbo.E2_Fn_county_AllowAmounts_byCode_byFile ('FY15 WASHTENAW CONTRACT RATES', '0100', '68', '10/1/15')
   select dbo.E2_Fn_county_AllowAmounts_byCode_byFile ('FY15 WASHTENAW CONTRACT RATES', '0100', '73', '10/1/15')
 
  select dbo.E2_Fn_county_AllowAmounts_byCode_byFile ('FY15 WASHTENAW CONTRACT RATES', '0100', null, '10/1/15')
 
 
 declare @county varchar(20) = 'Washtenaw'
declare @startdate datetime = '10/1/15', @enddate datetime = '9/30/16'

