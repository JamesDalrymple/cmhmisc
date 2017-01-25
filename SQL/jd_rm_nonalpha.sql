/*
IF OBJECT_ID ('dbo.fn_jd_rm_nonalpha') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_rm_nonalpha
go
create function dbo.fn_jd_rm_nonalpha(@Temp nvarchar(64))
	Returns nvarchar(64)
	AS
	Begin
		Declare @KeepValues as nvarchar(32)
		set @KeepValues = '%[^a-z() -]%'
		While PatIndex(@KeepValues, @Temp) > 0
			Set @Temp = Stuff(@Temp, PatIndex(@KeepValues, @Temp), 1, '')
		Return @Temp
End
go

IF OBJECT_ID ('dbo.fn_jd_keep_numeric') IS NOT NULL  
    DROP FUNCTION dbo.fn_jd_keep_numeric
go
create function dbo.fn_jd_keep_numeric(@Temp nvarchar(64))
	Returns nvarchar(64)
	AS
	Begin

		Declare @KeepValues as nvarchar(32)
		set @KeepValues = '%[^0-9.]%'
		While PatIndex(@KeepValues, @Temp) > 0
			Set @Temp = Stuff(@Temp, PatIndex(@KeepValues, @Temp), 1, '')
		Return @Temp
End
go
*/

/* EXAMPLES -----------------------------------------------------------------*/
select dbo.fn_jd_rm_nonalpha('WCCMH [2.0] (pre-CSTS)')
select dbo.fn_jd_rm_nonalpha('Skin and subcutaneous tissue infections [197.]')
select dbo.fn_jd_keep_numeric('WCCMH [2.0]')
