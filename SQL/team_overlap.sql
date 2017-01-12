use convenience_jd
go

/* Goal: create a 'historical' secondary team function that James can use to satisfy data
customers without going to R. The historical team function will correctly 
assign a team on any given date for any given consumer, based on the following
priority:
-- priority list (teams listed in order of priority) --
Utilization Management (usually these consumers are in the state hospital)
Level 5: residential DD (optional since only for Kelly Bellus)
Level 5: residential MI (optional since only for Kelly Bellus)
DD Adult
ACT
MI Adult
ATO (re-assigned to 'MI Adult')
Access/Engagement and/or WCCMH are re-assigned to 'Access'
*/

IF OBJECT_ID('convenience_jd.dbo.tbl_jd_team_priority') IS NOT NULL
	DROP TABLE convenience_jd.dbo.tbl_jd_team_priority
create table convenience_jd.dbo.tbl_jd_team_priority -- 1/3/2017 James
(org_sec_team nvarchar(64), -- original secondary team
 adj_sec_team nvarchar(32), -- adjusted team
 sec_team_priority int -- team priority
)
go
INSERT INTO convenience_jd.dbo.tbl_jd_team_priority
  (org_sec_team, adj_sec_team, sec_team_priority)
select distinct cmh.team2 as org_sec_team, 
	case when cmh.team2 = 'WSH - Utilization Management' then 'UM'
		when  cmh.team2 = 'WSH - DD Adult' then 'DD'
		when  cmh.team2 = 'WSH - ACT' then 'ACT'
		when cmh.team2 in ('WSH - MI - Adult', 'WSH - ATO') then 'MI'
		when cmh.team2 = 'WSH - Children''s Services - Home Based' then 'Child HB'
		when cmh.team2 = 'WSH - Children''s Services' then 'Child'
		when cmh.team2 in ('WSH - Access/Engagement', 
		'Washtenaw County Community Mental Health',
		'Crisis Residential Services') then 'Access'
		else 'NEW: '+ cmh.team2 end as adj_sec_team,
  	case when cmh.team2 = 'WSH - Utilization Management' then 1
		when  cmh.team2 = 'WSH - DD Adult' then 2
		when  cmh.team2 = 'WSH - ACT' then 3
		when cmh.team2 = 'WSH - MI - Adult' then 4
		when cmh.team2 = 'WSH - ATO' then 5
		when cmh.team2 = 'WSH - Children''s Services - Home Based' then 6
		when cmh.team2 = 'WSH - Children''s Services' then 7
		when cmh.team2 = 'WSH - Access/Engagement' then 8 
		when cmh.team2 = 'Washtenaw County Community Mental Health' then 9
		when cmh.team2 = 'Crisis Residential Services' then 10
		else 99 end as sec_team_priority
	/* other_secondary_teams: only other one I want is Health Home and that one 
	is subject to change so not sure we should make a global function when
	it is only applicable in one area PLUS may go away soon.
	*/
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as cmh
where cmh.County = 'Washtenaw'
	and Isnull(cmh.CMH_expdt, getdate()) >= '10/1/11' 
	and ((SUBSTRING(cmh.team2, 1, 1) = 'W' and
cmh.team2 not in ('WSH - ICSS team', 'WSH - InSHAPE', 'WSH - OBRA',
	'WSH - PATH/PORT', 'WSH - Pilot Disease Management', 'WSH - SAMHSA PBHCI',
	'WSH - MH Court', 'WSH - Sobriety Court', 'WSH - Health Home')) or 
	cmh.team2 = 'Crisis Residential Services')  
go
-- select * from tbl_jd_team_priority

-- contains all the admission records for a consumer active for a single date
IF OBJECT_ID ('dbo.fn_jd_adm_at_date') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_adm_at_date
go
create function dbo.fn_jd_adm_at_date(@case_no int, @date datetime)
returns table
as return( -- 1/5/17, 1/3/17 James
	select distinct 
		cmh.case_no, cmh.county, cmh.team as primary_team, 
		t2.adj_sec_team as adj_prim_team, cmh.cmh_effdt, cmh.cmh_expdt, 
		cmh.team2 as sec_team, t.adj_sec_team, t.sec_team_priority,
		cmh.team_effdt, cmh.team_expdt,	
		case when cmh.primary_staff_or_not = 'Y' 
			then cmh.assigned_staff else null end as primary_staff,
		case when cmh.primary_staff_or_not = 'Y' 
			then cmh.staff_type else null end as primary_staff_type,
		case when cmh.primary_staff_or_not = 'Y' 
			then cmh.supervisor else null end as primary_supervisor,
		cmh.gender, cmh.dob,
		cmh.assigned_staff as staff, cmh.supervisor
	from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA cmh
	join convenience_jd.dbo.tbl_jd_team_priority as t 
		on t.org_sec_team = cmh.team2
	join convenience_jd.dbo.tbl_jd_team_priority as t2 on t2.org_sec_team = cmh.team
	where cmh.case_no = @case_no and @date between cmh.team_effdt and 
		Isnull(cmh.team_expdt, GETDATE()) and cmh.county = 'Washtenaw'
) 
go
/* Examples:
select * from dbo.fn_jd_adm_at_date(10009, '2010-12-22')

-- checking
select *
from tblE2_CMH_Adm_Consumers_w_OBRA
where Case_No = 10009 and '2012-05-30' between team_effdt and isnull(team_expdt, getdate())
order by case_no, CMH_effdt, team_effdt
*/

/* contains one row per consumer with primary team and single secondary 
	team active on date based on priority */
IF OBJECT_ID ('dbo.fn_jd_team_at_date') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_team_at_date
go
create function dbo.fn_jd_team_at_date(@case_no int, @date datetime)
returns table
as return( -- 1/5/17, 1/3/17 James
	select cmh2.county, cmh2.case_no, cmh2.primary_team, cmh2.adj_prim_team,
		cmh2.CMH_effdt, cmh2.CMH_expdt, t.org_sec_team, t.adj_sec_team,
		min(cmh3.team_effdt) as sec_start, max(cmh3.team_expdt) as sec_end
	from (select cmh.county, cmh.case_no, cmh.primary_team, cmh.adj_prim_team,
		cmh.CMH_effdt, cmh.CMH_expdt, 
		min(cmh.sec_team_priority) as min_sec_team
	from dbo.fn_jd_adm_at_date(@case_no, @date) as cmh
	group by cmh.county, cmh.case_no, cmh.primary_team, cmh.adj_prim_team,
		cmh.CMH_effdt, cmh.CMH_expdt) as cmh2
	join dbo.tbl_jd_team_priority as t on t.sec_team_priority = cmh2.min_sec_team
	join dbo.fn_jd_adm_at_date(@case_no, @date) as cmh3 on t.adj_sec_team = cmh3.adj_sec_team
	group by cmh2.county, cmh2.case_no, cmh2.primary_team, cmh2.adj_prim_team,
		cmh2.CMH_effdt, cmh2.CMH_expdt, t.org_sec_team, t.adj_sec_team
)
go
/* Examples:
select * from dbo.fn_jd_team_at_date(10009, '2010-12-22')

select distinct
	h.case_no, h.hosp, h.auth_eff, 
	h.team_at_admit as team_at_adm
from encompass.dbo.tblE2_Hosp h
where h.auth_eff between '10/1/2010' and '12/31/2016' and 
	h.county = 'Washtenaw'
-- 4609, 1/3/2017

select distinct
	h.case_no, h.hosp, h.auth_eff, 
	h.team_at_admit as team_at_adm, -- Snow's function (active_between team)
	t.primary_team, t.adj_prim_team, 
	t.CMH_effdt, t.CMH_expdt,
	t.org_sec_team, t.adj_sec_team, t.sec_start, t.sec_end,
	t.county
from encompass.dbo.tblE2_Hosp h
outer apply dbo.fn_jd_team_at_date(h.case_no, h.auth_eff) as t
where h.auth_eff between '10/1/2010' and '12/31/2016' and h.county = 'Washtenaw'
	and h.Team_at_admit <> t.org_sec_team -- 250 records differing teams
	-- and t.org_sec_team= 'Crisis Residential Services'
order by case_no, auth_eff, hosp
-- 4609, 1/3/2017

-- select * from dbo.fn_jd_adm_at_date(13022, '2014-10-22')
*/


/* Crisis Team: no longer in use, but code is there for historical reasons ----
select *
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as cmh
where team2 = 'Crisis Residential Services' and cmh.CMH_expdt is null

select *
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
where team2 like '%Crisis%' and county = 'Washtenaw'
-- 3, 1/3/2017

select * from dbo.fn_jd_team_at_date(12822, '2015-06-19')
select * from dbo.fn_jd_team_at_date(164646, '2016-06-30')
select * from dbo.fn_jd_team_at_date(1139968, '2013-04-03')
-----------------------------------------------------------------------------*/

/* examples of a team incorrectly assigned ------------------------------------
select * from dbo.fn_jd_team_at_date(1125664, '2010-12-08')

select * 
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
where case_no = 1125664 and '2010-12-08' between cmh_effdt and cmh_expdt
and '2010-12-08' between team_effdt and team_expdt
-----------------------------------------------------------------------------*/

/* make sure cmh_effdt does not EVER overlap - verified 12/28/2016 ------------
	(this must ALWAYS be true)
select distinct
	a1.*, a2.cmh_effdt as cmh_start2, 
	a2.cmh_expdt as cmh_end2, a2.case_no as case_no2, a2.team as team2,
	a2.team_effdt as team_start2, a2.team_expdt as team_end2 
from tmp_jd_adm_priority as a1
join tmp_jd_adm_priority as a2 on (a1.cmh_effdt between a2.cmh_effdt and a2.cmh_expdt or 
a1.cmh_expdt between a2.cmh_effdt and a2.cmh_expdt) and a1.case_no = a2.case_no
and (a1.cmh_effdt <> a2.cmh_effdt and a1.cmh_expdt <> a2.cmh_expdt)
-----------------------------------------------------------------------------*/

/* make sure team start/end dates do not exceed cmh start/end dates -----------
-- this I think is not possible by design
select *
from tmp_jd_adm_priority
where team_effdt < cmh_effdt or team_expdt > cmh_expdt
-----------------------------------------------------------------------------*/

-- only works in SQL 2012+
-- https://stewashton.wordpress.com/2014/03/11/sql-for-date-ranges-gaps-and-overlaps/
-- https://stewashton.wordpress.com/2014/03/22/overlapping-ranges-with-priority/
