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
	cmh.gender, cmh.dob, cmh.age, t.adj_team as team, t.team_priority,
	DENSE_RANK() over (order by cmh.case_no, cmh.cmh_effdt, cmh.cmh_expdt, cmh.team_effdt, 
	cmh.team_expdt, t.adj_team) as adm_index
into #adm0
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA as cmh
join #team_priority as t on cmh.team2 = t.org_team
where cmh.county = 'Washtenaw'
-- select * from #adm0 order by adm_index
-- 14039 rows

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


/* only works in SQL 2012+
-- https://stewashton.wordpress.com/2014/03/11/sql-for-date-ranges-gaps-and-overlaps/
-- https://stewashton.wordpress.com/2014/03/22/overlapping-ranges-with-priority/

select distinct case_no, cmh_effdt, cmh_expdt, team_effdt, team, team_priority, adm_index
from #adm0 
union
select distinct case_no, cmh_effdt, cmh_expdt, team_expdt, team, team_priority, adm_index
from #adm0 


-- steps 1) and 2)
with base_ranges as (
  select distinct adm.team, i ba, -- sku, i ba,
  lead(i) over(partition by team order by i, col) as bb
  from #adm0 as adm
  unpivot(i for col in(adm.team_effdt, adm.team_expdt))
)
select * from base_ranges




-- steps 3) and 4)
, strongest_factored_ranges as (
  select bi.sku,
  bi.ba a, bi.bb b, 
  min(i.price) keep (dense_rank first order by i.prio) price
  from ranges i, base_ranges bi
  where i.sku = bi.sku
  and (i.a <= bi.ba
  and (bi.bb <= i.b or bi.bb is null and i.b is null))
  group by bi.sku, bi.ba, bi.bb
)
-- step 5)
select * from strongest_factored_ranges
match_recognize(
  partition by sku order by a, b
  measures chr(first(a)) "START", chr(last(b)) "END",
    first(price) price
  pattern(A B*)
  define B as (a, price) = ((prev(b), a.price))
)