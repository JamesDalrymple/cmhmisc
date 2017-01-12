use encompass
go

/* Goal: create a 'historical' team function that James can use to satisfy data
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

/* testing --------------------------------------------------------------------
select top 10 *
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
where county = 'Washtenaw'

select distinct team2
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
where county = 'Washtenaw'

-- regular check? no one should be on this team any more
select *
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as cmh
where team2 = 'Crisis Residential Services' and cmh.CMH_expdt is null
-----------------------------------------------------------------------------*/

IF OBJECT_ID('tempdb..#team_priority') IS NOT NULL
	DROP TABLE #team_priority
create table #team_priority
(org_team nvarchar(64), -- original team
 adj_team nvarchar(8), -- adjusted team
 team_priority int -- team priority
)
go
INSERT INTO #team_priority
  (org_team, adj_team, team_priority)
VALUES
('WSH - Utilization Management', 'UM', 1),
('WSH - DD Adult', 'DD', 2),
('WSH - ACT', 'ACT', 3),
('WSH - MI - Adult', 'MI', 4),
('WSH - ATO', 'MI', 4),
('WSH - Children''s Services - Home Based', 'Child HB', 5),
('WSH - Children''s Services', 'Child', 6),
('WSH - Access/Engagement', 'Access', 7),
('Washtenaw County Community Mental Health', 'Access', 7),
('Crisis Residential Services', 'Access', 7)
go
-- select * from #team_priority

-- initial data
IF OBJECT_ID('tempdb..#adm0') IS NOT NULL
	DROP TABLE #adm0
select distinct cmh.case_no, cast(cmh.cmh_effdt as datetime) as cmh_effdt, 
	cast(cmh.cmh_expdt as datetime) as cmh_expdt, -- cmh.team2, 
	cast(cmh.team_effdt as datetime) as team_effdt, 
	cast(cmh.team_expdt as datetime) as team_expdt, 
	-- cmh.gender, cmh.dob, cmh.age, 
	t.adj_team as team, t.team_priority,
	DENSE_RANK() over (order by cmh.case_no, cmh.cmh_effdt, cmh.cmh_expdt, cmh.team_effdt, 
	cmh.team_expdt, t.adj_team) as adm_index,
	DENSE_RANK() over (order by cmh.case_no, cmh.cmh_effdt) as cmh_index,
	-- DENSE_RANK() over (order by cmh.case_no, cmh.cmh_effdt, t.adj_team) as team_index,
	DENSE_RANK() over (Partition by cmh.case_no, cmh.cmh_effdt, t.adj_team order by cmh.team_effdt) as within_team
into #adm0
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as cmh
join #team_priority as t on cmh.team2 = t.org_team
where cmh.county = 'Washtenaw' -- and case_no = 10424
order by cmh_index, within_team
-- select * from #adm0
-- 14039 rows

IF OBJECT_ID('tempdb..#adm1') IS NOT NULL
	DROP TABLE #adm1
select distinct a1.*, -- a1.adm_index as adm_index1, a2.adm_index as adm_index2, a1.case_no, a2.team, a1.team_effdt, a1.team_expdt
	case when a1.adm_index = 1 then 1
		 when a1.adm_index = a2.adm_index + 1 and a1.cmh_index = a2.cmh_index
		 and a1.within_team = a2.within_team + 1 then 0
		 else 1 end as keep_with_next
into #adm1
from #adm0 as a1
left join #adm0 as a2 on a1.adm_index = a2.adm_index + 1 and
	a1.cmh_index = a2.cmh_index and a1.within_team = a2.within_team + 1


/* make sure cmh_effdt does not EVER overlap - verified 12/28/2016 ------------
	(this must ALWAYS be true)
select distinct
	a1.*, a2.cmh_effdt as cmh_start2, 
	a2.cmh_expdt as cmh_end2, a2.case_no as case_no2, a2.team as team2,
	a2.team_effdt as team_start2, a2.team_expdt as team_end2 
from #adm0 as a1
join #adm0 as a2 on (a1.cmh_effdt between a2.cmh_effdt and a2.cmh_expdt or 
a1.cmh_expdt between a2.cmh_effdt and a2.cmh_expdt) and a1.case_no = a2.case_no
and (a1.cmh_effdt <> a2.cmh_effdt and a1.cmh_expdt <> a2.cmh_expdt)
-----------------------------------------------------------------------------*/


/* make sure team start/end dates do not exceed cmh start/end dates -----------
-- this I think is not possible by design
select *
from #adm0
where team_effdt < cmh_effdt or team_expdt > cmh_expdt
-----------------------------------------------------------------------------*/

/* cumulative sum in SQL 2008 R2 the slow way ---------------------------------
SELECT
    T1.adm_index, T1.keep_with_next, SUM(coalesce(T2.keep_with_next, 0)) + 
	T1.keep_with_next AS CumSum
FROM 
    #adm1 T1
left JOIN #adm1 T2 ON T1.adm_index > T2.adm_index
GROUP BY
    T1.adm_index, T1.keep_with_next
order by t1.adm_index
-----------------------------------------------------------------------------*/

IF OBJECT_ID('tempdb..#adm2') IS NOT NULL
	DROP TABLE #adm2
select distinct
	a1.*, a2.team_grp
into #adm2
FROM     #adm1 a1 
         CROSS APPLY ( 
         SELECT   SUM(a2.keep_with_next) AS team_grp 
         FROM     #adm1 a2 
         WHERE    a1.adm_index >= a2.adm_index
         ) a2
order by a1.adm_index
-- select * from #adm2

-- overlapping same team fixed
IF OBJECT_ID('tempdb..#adm3') IS NOT NULL
	DROP TABLE #adm3
select 
	a.case_no, a.cmh_effdt, a.cmh_expdt, min(a.team_effdt) as team_effdt, 
	max(a.team_expdt) as team_expdt, a.team, a.team_priority
into #adm3
from #adm2 as a
group by a.case_no, a.cmh_effdt, a.cmh_expdt, a.team, a.team_priority
-- select * from #adm3

IF OBJECT_ID('tempdb..#adm4') IS NOT NULL
	DROP TABLE #adm4
select a.*,
	DENSE_RANK() over (order by a.case_no, a.cmh_effdt, a.cmh_expdt, 
	a.team_effdt, a.team_expdt, a.team) as adm_index,
	DENSE_RANK() over (order by a.case_no, a.cmh_effdt) as cmh_index,
	DENSE_RANK() over (Partition by a.case_no, a.cmh_effdt 
		order by a.team_effdt, team) as within_cmh
into #adm4
from #adm3 as a
order by adm_index
-- select * from #adm4

IF OBJECT_ID('tempdb..#adm5') IS NOT NULL
	DROP TABLE #adm5
select distinct a1.*, -- a1.adm_index as adm_index1, a2.adm_index as adm_index2, a1.case_no, a2.team, a1.team_effdt, a1.team_expdt
	case when a1.adm_index = 1 then 1
		 when a1.adm_index = a2.adm_index + 1 and a1.cmh_index = a2.cmh_index
		 and a1.within_cmh = a2.within_cmh + 1 then 0
		 else 1 end as keep_with_next
into #adm5
from #adm4 as a1
left join #adm4 as a2 on a1.adm_index = a2.adm_index + 1 and
	a1.cmh_index = a2.cmh_index and a1.within_cmh = a2.within_cmh + 1

IF OBJECT_ID('tempdb..#adm6') IS NOT NULL
	DROP TABLE #adm6
select distinct
	a1.*, a2.team_grp
into #adm6
FROM     #adm5 a1 
         CROSS APPLY ( 
         SELECT   SUM(a2.keep_with_next) AS team_grp 
         FROM     #adm5 a2 
         WHERE    a1.adm_index >= a2.adm_index
         ) a2
order by a1.adm_index
-- select * from #adm6


select * 
from #adm6