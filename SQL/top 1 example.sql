IF OBJECT_ID('tempdb..#hosp') IS NOT NULL
	DROP TABLE #hosp
select distinct 
	h.case_no, 
	dense_rank() over (order by h.case_no, h.hosp, h.auth_eff) as h_start_key, 
	h.hosp, h.auth_eff, h.auth_exp, h.hosp_disc, 
	h.auth_units, h.unitsused, h.paid_amt, h.funding_source,
	h.adult, t.adj_prim_team, t.cmh_effdt, t.cmh_expdt, t.adj_sec_team, 
	t.sec_start, t.sec_end, h.unit_rate
into #hosp
from encompass.dbo.E2_fn_Hosp_Inpatients_Auth_vs_Paid_w_Services(
	'Washtenaw', '10/1/2014', '9/30/2016') as h
outer apply jd_utility.dbo.fn_jd_team_at_date(h.case_no, h.auth_eff) as t
/* testing #hosp --------------------------------------------------------------
select distinct * --auh_ID, count(*) as num
from encompass.dbo.E2_fn_Hosp_Inpatients_Auth_vs_Paid_w_Services(
	'Washtenaw', '10/1/2014', '9/30/2016')
	where auh_Id in ( 1111906,
1117552,
1119512,
1124801,
1126711,
1140777,
1148673 )
	group by auh_ID 
	having count(*) >1
	
	

select count(*) as num_records, count(distinct h_start_key) as top1_count
from #hosp

num_records	top1_count
1484		1477
-----------------------------------------------------------------------------*/

-- correct, but relies on dense_rank()
select distinct result.*
from #hosp d
cross apply (select top 1 * from #hosp as h_inner where h_inner.h_start_key = d.h_start_key  ) result

-- correct, and does not rely on dense_rank(). Should be faster than dense rank too.
select distinct result.*
from #hosp as original
cross apply (
	select top 1 * from #hosp as h_inner 
	where h_inner.case_no = original.case_no and
		h_inner.hosp = original.hosp and h_inner.auth_eff = original.auth_eff) as result


