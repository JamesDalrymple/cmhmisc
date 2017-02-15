/*
Pool: health home consumers active at least one day during the date range
fields needed: 
	case_no, er start date, er end date
*/

/* Notes (9/12/2016):
1. V_CC360_ERs: service_to_date >= '10/1/13' and County <> 'CMHPSM-SUD', 
			 Team is Primary team (and not team at service date)
2. Staff credentials much more accurate that staff title.
3. ER visits prior to 10/1/2014 are not extremely reliable!
4. 193 consumers were admitted during 7/1/14 to 10/30/14.
	select distinct case_no from
	encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
	where County = 'Washtenaw' and Team2 = 'WSH - Health Home' and team_effdt between '7/1/2014' and '9/30/2014'
5. Consumers admitted to HH prior to 10/1/14 can have at most 9 months of 'prior'/'pre' ER data.
*/

IF OBJECT_ID('tempdb.dbo.#hh_nurse', 'U') IS NOT NULL
  DROP TABLE #hh_nurse 
select distinct 
	case_no, team_effdt, team_expdt, 
	case when staff_eff < team_effdt then team_effdt else 
		staff_eff end as staff_start, staff_exp as staff_end,
	Isnull(staff_exp, GETDATE()) as staff_end2, 
	assigned_staff as hh_nurse, adm.supervisor, adm.staff_type, staff.StaffType
into #hh_nurse
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as adm
join encompass.dbo.tblE2_Staff_Credentials as staff on staff.ST_RCDID = adm.E2_ST_RCDID
where adm.county = 'Washtenaw' and adm.team2 = 'WSH - Health Home'
	and Staff_Type = 'SAMHSA Staff' and 
	(staff.staffType = 'Registered Nurse')
	and (staff_exp is null or staff_exp >= team_effdt)
go
-- select distinct hh_nurse from #hh_nurse
--select * from tblE2_CMH_Adm_Consumers_w_OBRA 

-- select HH population of interest (HH at least one day since 7/14/2014 right now)
IF OBJECT_ID('tempdb.dbo.#hh_adm', 'U') IS NOT NULL
  DROP TABLE #hh_adm
select distinct
	adm.case_no, adm.team_effdt as hh_start, adm.team_expdt as hh_end,
hh_nurse.staff_start, hh_nurse.staff_end,
Staff_end2
into #hh_adm
from encompass.dbo.E2_Fn_Health_Home_Consumers('7/1/14', GETDATE()) as adm
left join #hh_nurse as hh_nurse on 
	hh_nurse.case_no = adm.case_no and adm.team_effdt = hh_nurse.team_effdt
-- select * from #hh_adm

/* pre: up to 1 year. post: at least six months from the start of team_eff.
 multiple hh_start and hh_end will require further work to make sure all pre
 contain no HH active consumers.
*/ 
IF OBJECT_ID('tempdb.dbo.#hh_er', 'U') IS NOT NULL
  DROP TABLE #hh_er
select distinct 
	er.case_no, er.service_from_date, hh_adm.hh_start, hh_adm.hh_end,  
	max(case when er.service_from_date between 
		hh_adm.hh_start and isNull(hh_adm.hh_end, getdate()) then 1 else 0 end) as hh_at_svc,
	max(case when er.service_from_date between 
		hh_adm.staff_start and Staff_end2 then 1 else 0 end) as hh_rn_at_svc
into #hh_er
from Frank_data.dbo.V_CC360_ERs as er
join #hh_adm hh_adm on er.case_no = hh_adm.case_no
where er.county = 'Washtenaw' and 
	er.service_from_date >= dateadd( yy, -1, cast(hh_adm.hh_start as datetime)) + 1 
group by er.case_no, er.service_from_date, hh_adm.hh_start, hh_adm.hh_end
go
-- select distinct * from #hh_er 