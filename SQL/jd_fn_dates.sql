/* Deprecated functions -------------------------------------------------------
dbo.Fn_All_Quarters (Procedures: E2_CMH_active_Count_by_Quarter,
	E2_CMH_Comm_Hosp_adms_sum_by_Quarter,
	E2_CMH_program_active_Count_by_Quarter)
fn_FiscalYear (functions: Fn_All_FY)
Fn_All_FY (Procedures: E2_CMH_active_Count_by_FY, 
	E2_CMH_active_Count_by_FY, E2_CMH_program_active_Count_by_FY, 
	E2_CNA_Homesless_Served_by_County_Row7A)
	(functions: E2_Fn_CNA_Homeless_by_OddFY_all)
*/

/*  DROP ALL JD functions -----------------------------------------------------

SELECT name, definition, type_desc 
  FROM jd_utility.sys.sql_modules m 
INNER JOIN jd_utility.sys.objects o 
        ON m.object_id=o.object_id
WHERE type_desc like '%function%' and
name like 'fn_jd%'

DROP FUNCTION dbo.fn_jd_get_fy_aux_char
DROP FUNCTION dbo.fn_jd_get_fy_aux_num
DROP FUNCTION dbo.fn_jd_get_quarter_aux0
DROP FUNCTION dbo.fn_jd_get_month_aux0
DROP FUNCTION dbo.fn_jd_get_fys
DROP FUNCTION dbo.fn_jd_get_quarters
DROP FUNCTION dbo.fn_jd_get_months
DROP FUNCTION dbo.fn_jd_cat_time
-----------------------------------------------------------------------------*/

-- create database jd_utility -- 1/12/2017

use jd_utility
go

IF OBJECT_ID ('dbo.fn_jd_get_fy_aux_char') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_get_fy_aux_char
go
create FUNCTION dbo.fn_jd_get_fy_aux_char(@start_date date)
 /* 1/4/17, 12/22/16, 1/7/14, 1/6/15 James
	Helper function for fn_jd_get_fys, old name: dbo.fn_FiscalYear*/
RETURNS varchar(8)
AS
BEGIN
	DECLARE @Answer varchar(8)
	IF (MONTH(@start_date) < 10)
		SET @Answer = 'FY' + substring(cast(YEAR(@start_date) 
		as varchar(8)), 3, 2)
	ELSE
		SET @Answer = 'FY' + substring(cast(YEAR(@start_date) + 1 
		as varchar(8)), 3, 2)
	RETURN @Answer
END
GO
/* example --------------------------------------------------------------------
select dbo.fn_jd_get_fy_aux_char('10/1/2015'), dbo.fn_jd_get_fy_aux_char('9/30/2016')
select dbo.fn_jd_get_fy_aux_char('10/1/2016'), dbo.fn_jd_get_fy_aux_char('9/30/2017')
select dbo.fn_jd_get_fy_aux_char('10/1/2017'), dbo.fn_jd_get_fy_aux_char('9/30/2018')
select dbo.fn_jd_get_fy_aux_char('10/1/2018'), dbo.fn_jd_get_fy_aux_char('9/30/2019')
*/

IF OBJECT_ID ('dbo.fn_jd_get_fy_aux_num') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_get_fy_aux_num
go
create FUNCTION dbo.fn_jd_get_fy_aux_num(@start_date date)
/* 1/4/17, 12/22/16, 1/7/14, 1/6/15 James
	Helper function for fn_jd_get_fys, old name: dbo.fn_FiscalYear*/
RETURNS int
AS
BEGIN
	DECLARE @Answer varchar(8)
	IF (MONTH(@start_date) < 10)
		SET @Answer = YEAR(@start_date)
	ELSE
		SET @Answer = YEAR(@start_date)+1
	RETURN @Answer
END
GO
/* example --------------------------------------------------------------------
select dbo.fn_jd_get_fy_aux_num('10/1/2015'), dbo.fn_jd_get_fy_aux_num('9/30/2016')
select dbo.fn_jd_get_fy_aux_num('10/1/2016'), dbo.fn_jd_get_fy_aux_num('9/30/2017')
select dbo.fn_jd_get_fy_aux_num('10/1/2017'), dbo.fn_jd_get_fy_aux_num('9/30/2018')
*/

IF OBJECT_ID ('dbo.fn_jd_get_quarter_aux0') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_get_quarter_aux0
go
create FUNCTION dbo.fn_jd_get_quarter_aux0(@start_date DATEtime)
RETURNS varchar(9)
AS  /* 1/4/17, 12/22/16, 1/7/15, 1/6/15, James
	helper function for dbo.fn_jd_get_quarters, old name: fn_jd_get_Quarter */
BEGIN
	DECLARE @Answer varchar(9)
	IF (MONTH(@start_date) between 10 and 12)
		SET @Answer = 'FY' + SUBSTRING( cast(YEAR(@start_date) + 
		1 as nvarchar(4)), 3, 4) + ' Q1'
	IF (MONTH(@start_date) between 1 and 3)
		SET @Answer = 'FY' +  SUBSTRING( cast(YEAR(@start_date) 
		as nvarchar(4)), 3, 4) + ' Q2'
	IF (MONTH(@start_date) between 4 and 6)
		SET @Answer = 'FY' + SUBSTRING( cast(YEAR(@start_date) 
		as nvarchar(4)), 3, 4) + ' Q3'
	IF (MONTH(@start_date) between 7 and 9)
		SET @Answer = 'FY' + SUBSTRING( cast(YEAR(@start_date) 
		as nvarchar(4)), 3, 4) + ' Q4'	
	RETURN @Answer
END
GO
/* example --------------------------------------------------------------------
select 
	dbo.fn_jd_get_quarter_aux0('10/1/2015'), 
	dbo.fn_jd_get_quarter_aux0('1/1/2016'),
	dbo.fn_jd_get_quarter_aux0('4/1/2016'), 
	dbo.fn_jd_get_quarter_aux0('7/1/2016')
select 
	dbo.fn_jd_get_quarter_aux0('10/1/2016'), 
	dbo.fn_jd_get_quarter_aux0('1/1/2017'),
	dbo.fn_jd_get_quarter_aux0('4/1/2017'), 
	dbo.fn_jd_get_quarter_aux0('7/1/2017')
-----------------------------------------------------------------------------*/

IF OBJECT_ID ('dbo.fn_jd_get_month_aux0') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_get_month_aux0
go
CREATE FUNCTION dbo.fn_jd_get_month_aux0(@start_date date)
RETURNS varchar(8)
AS /* 1/4/17,  2/26/15 James */
BEGIN
	DECLARE @Answer varchar(8)
		set @Answer = substring(datename(month, @start_date), 1, 3) + 
		' ' + cast(datepart(year, @start_date) as nvarchar(4))
	RETURN @Answer
END
GO
/* example --------------------------------------------------------------------
select 
	dbo.fn_jd_get_month_aux0('10/1/2015'), 
	dbo.fn_jd_get_month_aux0('11/1/2015'),
	dbo.fn_jd_get_month_aux0('12/1/2015'), 
	dbo.fn_jd_get_month_aux0('1/1/2016'),
	dbo.fn_jd_get_month_aux0('2/1/2016'), 
	dbo.fn_jd_get_month_aux0('3/1/2016'),
	dbo.fn_jd_get_month_aux0('4/1/2016'), 
	dbo.fn_jd_get_month_aux0('5/1/2016'),
	dbo.fn_jd_get_month_aux0('6/1/2016'), 
	dbo.fn_jd_get_month_aux0('7/1/2016'),
	dbo.fn_jd_get_month_aux0('8/1/2016'), 
	dbo.fn_jd_get_month_aux0('9/1/2016'),
	dbo.fn_jd_get_month_aux0('10/1/2016'), 
	dbo.fn_jd_get_month_aux0('11/1/2016'),
	dbo.fn_jd_get_month_aux0('12/1/2016'), 
	dbo.fn_jd_get_month_aux0('1/1/2017')
-----------------------------------------------------------------------------*/

IF OBJECT_ID ('dbo.fn_jd_get_fys') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_get_fys
go
create function dbo.fn_jd_get_fys(@startdate datetime, @enddate datetime)
returns table
as return /* 1/4/17, 12/23/16, 2/23/2015, 2/19/2015, James 
			old name: Fn_All_FY */
with fy_data as
(
    select
		dbo.fn_jd_get_fy_aux_char(@startdate) as FY_char,
		dbo.fn_jd_get_fy_aux_num(@startdate) as FY_num,
		cast('10/1/' + cast(dbo.fn_jd_get_fy_aux_num(@startdate)-1 as nvarchar(4)) as datetime) as t_start, -- need this if start date is not 1st day in the fy
		cast('9/30/' + cast(dbo.fn_jd_get_fy_aux_num(@startdate) as nvarchar(4)) as datetime) as t_end,
		@startdate as startdate,
		@enddate as enddate
    UNION ALL
    SELECT
		dbo.fn_jd_get_fy_aux_char(dateadd(year, 1, t_start)) as FY_char,
		FY_num+1 as FY_num,
		dateadd(year, 1, t_start) as t_start,
		dateadd(year, 1, t_end) as t_end,
		@startdate as startdate,
		@enddate as enddate
    FROM fy_data
    WHERE dateadd(year, 1, t_start) <= @enddate
)
select FY_char, FY_num, t_start, t_end,
	case when startdate < t_end and startdate > t_start 
	then startdate else t_start end as start_adj,
	case when enddate < t_end and enddate > t_start
	then enddate else t_end end as end_adj
	from fy_data
go
/* example --------------------------------------------------------------------
select * from dbo.fn_jd_get_fys('10/1/2010', '12/21/2017')
select * from dbo.fn_jd_get_fys('7/1/2015', '12/1/2016')
*/

IF OBJECT_ID ('dbo.fn_jd_get_quarters') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_get_quarters
go
CREATE function dbo.fn_jd_get_quarters(@startdate datetime, @enddate datetime)
returns table
as return /* James 1/11/17, 1/4/17, 12/23/16			
			old name: Fn_All_Quarters -- 6/22/15, Snow, 2/23/2015, 2/19/2015, 
			2/18/2015, James */
WITH qtr_data as
(   select
		dateadd(qq, datediff(qq, 0, @startdate), 0) as t_start,
		dateadd(dd, -1, dateadd(qq, datediff(qq, 0, @startdate) +1, 0)) 
		as t_end, @startdate as startdate, @enddate as enddate
    UNION ALL
    SELECT
		dateadd(month, 3, t_start) as t_start,
		dateadd(month, 6, t_start)-1 as t_end,
		@startdate as startdate, @enddate as enddate
    FROM qtr_data
    WHERE dateadd(month, 3, t_start) <= @enddate
)
select
	dbo.fn_jd_get_fy_aux_char(t_start) as fy_char,
	dbo.fn_jd_get_fy_aux_num(t_start) as FY_num,
	substring(dbo.fn_jd_get_quarter_aux0(t_start), 6, 7) as qtr,
	dbo.fn_jd_get_quarter_aux0(t_start) as fy_qtr,
	t_start, t_end,
	case when startdate < t_end and startdate > t_start 
	then startdate else t_start end as start_adj,
	case when enddate < t_end and enddate > t_start
	then enddate else t_end end as end_adj 
from qtr_data
go
/* example --------------------------------------------------------------------
select * from dbo.fn_jd_get_quarters('10/1/2010', '12/21/2017')
select * from dbo.fn_jd_get_quarters('7/14/2015', '12/12/2016')
-----------------------------------------------------------------------------*/

IF object_id('dbo.fn_jd_get_months') IS NOT NULL 
		DROP FUNCTION dbo.fn_jd_get_months
go
create function dbo.fn_jd_get_months(@startdate datetime, @enddate datetime)
returns table as return /* 1/11/17, 12/23/16 James */
WITH mon_data AS
(    SELECT
		dateadd(mm, datediff(mm, 0, @startdate), 0) as t_start, 
		dateadd(dd, -1, dateadd(mm, datediff(mm, 0, @startdate) +1, 0)) as t_end,
		@startdate as startdate, @enddate as enddate		
    UNION ALL
    SELECT
		DATEADD(MONTH, 1, t_start) as t_start,
		dateadd(month, 2, t_start)-1 as t_end,
		@startdate as startdate, @enddate as enddate
    FROM mon_data
   WHERE DATEADD(MONTH, 1, t_start) <= @enddate
)
select
	dbo.fn_jd_get_fy_aux_char(t_start) as fy_char,
	dbo.fn_jd_get_fy_aux_num(t_start) as FY_num,
	substring(dbo.fn_jd_get_quarter_aux0(t_start), 6, 7) as qtr,
	dbo.fn_jd_get_quarter_aux0(t_start) as fy_qtr,
	datename(month, t_start) as month_name,
	dbo.fn_jd_get_month_aux0(t_start) as calendar_month,
	t_start, t_end,
	case when startdate < t_end and startdate > t_start 
	then startdate else t_start end as start_adj,
	case when enddate < t_end and enddate > t_start
	then enddate else t_end end as end_adj
from mon_data
go
/* example ---------------------------
select * from dbo.fn_jd_get_months('7/15/2010', '9/15/2018')
select * from dbo.fn_jd_get_months('7/15/2015', '9/15/2016')
-------------------------------------*/

IF object_id('dbo.fn_jd_cat_time') IS NOT NULL 
		DROP FUNCTION dbo.fn_jd_cat_time
go
create function dbo.fn_jd_cat_time(@startdate datetime, @enddate datetime)
returns table
as return /* 1/11/17, 1/4/17, 12/23/2016, James 
		  utility fn combining calendar_mon, qtr, fy 'get' functions. */
WITH cat_data as
(
	select fy.FY_num as fy_num, fy.FY_char as time_cat, fy.t_start, fy.t_end, 
		fy.start_adj, fy.end_adj, 'fy' as category 
	from dbo.fn_jd_get_fys(@startdate, @enddate) as fy
	union
	select qtr.FY_num as fy_num, qtr.fy_qtr as time_cat, qtr.t_start, qtr.t_end, 
		qtr.start_adj, qtr.end_adj, 'qtr' as category
	from dbo.fn_jd_get_quarters(@startdate, @enddate) as qtr
	union
	select mon.FY_num as fy_num, mon.calendar_month as cat_time, mon.t_start, 
		mon.t_end, mon.start_adj, mon.end_adj, 'calendar_month' as category
	from dbo.fn_jd_get_months(@startdate, @enddate) as mon
)
select fy_num, time_cat, category, t_start, t_end, start_adj, end_adj from cat_data
go
/* example ---------------------------------------------------
select * from dbo.fn_jd_cat_time('10/1/2010', '9/30/2018')
order by category, t_start
------------------------------------------------------------*/
