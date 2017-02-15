
create view E2_HH_Nurse as 
 -- 9/13/16
select distinct 
	case_no, team_effdt, team_expdt, 
	case when staff_eff < team_effdt then team_effdt else staff_eff end as staff_start, staff_exp as staff_end,
	Isnull(staff_exp, GETDATE()) as staff_end2, 
	assigned_staff as hh_nurse, adm.supervisor
--into #hh_nurse
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as adm
join encompass.dbo.tblE2_Staff_Credentials as staff on staff.ST_RCDID = adm.E2_ST_RCDID
where adm.county = 'Washtenaw' and adm.team2 = 'WSH - Health Home'
	and Staff_Type = 'SAMHSA Staff' and 
	(staff.staffType = 'Registered Nurse')
	and ( staff_exp is null or staff_exp >= team_effdt)
go

  
  
CREATE function [dbo].[E2_Fn_Health_Home_Consumers_w_Nurse]( @startdate datetime, @enddate datetime )
returns table
as
return ( --9/13/16,  
select distinct
	adm.case_no, adm.team_effdt as hh_start, adm.team_expdt as hh_end, ISNULL(adm.team_expdt, getdate()) as hh_end2, 
hh_nurse.staff_start, hh_nurse.staff_end, Staff_end2, hh_nurse.Supervisor as hh_nurse_supervisor, 
MAIDNo, 
CMH_Team, Primary_Staff,  
Reg_Nurse,
CSM
from encompass.dbo.E2_Fn_Health_Home_Consumers(@startdate, @enddate) as adm
left join E2_HH_Nurse as hh_nurse on hh_nurse.Case_No = adm.case_no and adm.team_effdt = hh_nurse.team_effdt
)
go


