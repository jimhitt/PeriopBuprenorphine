-- USE DATABASE_NAME

DECLARE @jh_start	char(10) = '2010-01-01'
DECLARE @jh_end		char(10) = '2020-12-31'
-- range of preop rx (datediff call does not require negative numbers)
DECLARE @rx_start	int = 0
DECLARE @rx_end		int = 35
DECLARE @oud_dx		int = 365
-------------------------------------------------------
DECLARE @jh_tmp1	tinyint = 0		-- exclude the patients in jh_bup_surgery2 from VASQIP query
DECLARE @jh_tmp2	tinyint = 0		-- find outpatient dx OUD
DECLARE @jh_tmp3	tinyint = 0		-- find inpatient dx OUD
DECLARE @jh_tmp4	tinyint = 0		-- join tables and exclude those patients
									-- saves full control table (jh_vasqip_control)
DECLARE @jh_tmp5	tinyint = 0		-- clear tmp tables
-------------------------------------------------------
DECLARE @jh_tmp6	tinyint = 1		-- take control list from R and populate all needed VASQIP columns
									-- saves jh_vasqip_control

-- exclude patients from control if they are in jh_bup_surgery2
-- and limit on date range
IF @jh_tmp1 = 1 BEGIN
	DROP TABLE IF EXISTS #jh_tmp1
	SELECT v.scrssn, v.oprymd
	INTO #jh_tmp1
	FROM Src.VASQIP_nso_noncardiac v
	WHERE v.scrssn NOT IN (SELECT d.scrssn FROM jh_bup_surgery2 d) AND
	(	v.oprymd > @jh_start AND
		v.oprymd < @jh_end
	) AND
	v.race1 IS NOT NULL
END

IF @jh_tmp2 = 1 BEGIN
	-- find outpat OUD dx +/- 1 year of surgery date in vasqip
	DROP TABLE IF EXISTS #jh_op_oud
	SELECT pts.*
	INTO #jh_op_oud
	FROM #jh_tmp1 pts
	JOIN Src.SPatient_SPatient sp
		ON pts.scrssn = sp.ScrSSN
	JOIN Src.Outpat_VDiagnosis opdx
		ON opdx.PatientSID = sp.PatientSID
	JOIN CDWWork.Dim.ICD9 icd9
		ON icd9.ICD9SID = opdx.ICD9SID
	JOIN CDWWork.Dim.ICD10 icd10
		ON icd10.ICD10SID = opdx.ICD10SID

	WHERE (datediff(day, pts.oprymd, opdx.VDiagnosisDateTime) BETWEEN -1 * @oud_dx AND 0
		) AND (
			-- OUD codes
			icd9.ICD9Code IN ('304.00','304.000','304.001','304.002','304.003','304.009','304.01','304.010',
			'304.011','304.012','304.013','304.019','304.02','304.020','304.021','304.022','304.023','304.029',
			'304.03','304.030','304.031','304.032','304.033','304.039','304.09','304.639','304.70','304.71','304.72',
			'304.73','304.80','304.81','304.82','304.83','304.90','304.909','304.91','305.50','305.500','305.501',
			'305.502','305.503','305.509','305.51','305.510','305.511','305.512','305.513','305.519','305.52','305.520',
			'305.521','305.522','305.523','305.529','305.53','305.530','305.531','305.532','305.533','305.539','305.59','305.909',
			'305.91','305.919','305.92','305.929','305.939','305.99')
			OR
			icd10.ICD10Code IN ('F11.10','F11.11','F11.120','F11.121','F11.122','F11.129','F11.13','F11.14','F11.150','F11.151',
'F11.159','F11.181','F11.182','F11.188','F11.19','F11.20','F11.21','F11.220','F11.221','F11.222','F11.229','F11.23','F11.24','F11.250',
'F11.251','F11.259','F11.281','F11.282','F11.288','F11.29','F11.90','F11.920','F11.921','F11.922','F11.929','F11.93','F11.94','F11.950',
'F11.951','F11.959','F11.981','F11.982','F11.988','F11.99','T40.2X1A','T40.2X1D','T40.2X1S','T40.2X2A','T40.2X2D','T40.2X2S','T40.2X3A',
'T40.2X3D','T40.2X3S','T40.2X4A','T40.2X4D','T40.2X4S','T40.2X5A','T40.2X5D','T40.2X5S','T40.2X6A','T40.2X6D','T40.2X6S'
			)
		)
END

-- find inpatient OUD
IF @jh_tmp3 = 1 BEGIN
	DROP TABLE IF EXISTS #jh_ip_oud
	
	SELECT pts.*
	INTO #jh_ip_oud
	FROM #jh_tmp1 pts
	JOIN Src.SPatient_SPatient sp
		ON pts.scrssn = sp.ScrSSN
	JOIN Src.Inpat_Inpatient inpat
		ON inpat.PatientSID = sp.PatientSID
	JOIN Src.Inpat_InpatientDiagnosis ipdx
		ON inpat.InpatientSID = ipdx.InpatientSID
	JOIN CDWWork.Dim.ICD9 icd9
		ON icd9.ICD9SID = ipdx.ICD9SID
	JOIN CDWWork.Dim.ICD10 icd10
		ON icd10.ICD10SID = ipdx.ICD10SID

	WHERE (datediff(day, pts.oprymd, ipdx.DischargeDateTime) BETWEEN -1 * @oud_dx AND 0
		) AND (
			-- OUD codes
			icd9.ICD9Code IN ('304.00','304.000','304.001','304.002','304.003','304.009','304.01','304.010',
			'304.011','304.012','304.013','304.019','304.02','304.020','304.021','304.022','304.023','304.029',
			'304.03','304.030','304.031','304.032','304.033','304.039','304.09','304.639','304.70','304.71','304.72',
			'304.73','304.80','304.81','304.82','304.83','304.90','304.909','304.91','305.50','305.500','305.501',
			'305.502','305.503','305.509','305.51','305.510','305.511','305.512','305.513','305.519','305.52','305.520',
			'305.521','305.522','305.523','305.529','305.53','305.530','305.531','305.532','305.533','305.539','305.59','305.909',
			'305.91','305.919','305.92','305.929','305.939','305.99')
			OR
			icd10.ICD10Code IN ('F11.10','F11.11','F11.120','F11.121','F11.122','F11.129','F11.13','F11.14','F11.150','F11.151',
'F11.159','F11.181','F11.182','F11.188','F11.19','F11.20','F11.21','F11.220','F11.221','F11.222','F11.229','F11.23','F11.24','F11.250',
'F11.251','F11.259','F11.281','F11.282','F11.288','F11.29','F11.90','F11.920','F11.921','F11.922','F11.929','F11.93','F11.94','F11.950',
'F11.951','F11.959','F11.981','F11.982','F11.988','F11.99','T40.2X1A','T40.2X1D','T40.2X1S','T40.2X2A','T40.2X2D','T40.2X2S','T40.2X3A',
'T40.2X3D','T40.2X3S','T40.2X4A','T40.2X4D','T40.2X4S','T40.2X5A','T40.2X5D','T40.2X5S','T40.2X6A','T40.2X6D','T40.2X6S'
			)
		)
END

-- join inpatient and outpatient OUD tables for controls
IF @jh_tmp4 = 1 BEGIN
	
	-- #jh_tmp2 is union of ip and op OUD tables
	DROP TABLE IF EXISTS #jh_tmp2
	SELECT d.oprymd, d.scrssn
	INTO #jh_tmp2
	FROM #jh_tmp1 d
	WHERE d.scrssn NOT IN  (
			SELECT a.scrssn FROM #jh_ip_oud a
			UNION
			SELECT b.scrssn FROM #jh_op_oud b
			UNION
			SELECT c.scrssn FROM jh_bup_surgery2 c
		) 
	GROUP BY d.oprymd, d.scrssn

	-- join #jh_tmp2 on VASQIP
	--	  saves jh_vasqip_control
	DROP TABLE IF EXISTS jh_vasqip_control
	SELECT pts.oprymd, pts.scrssn,
		d.sex, d.asaclas, d.race1, d.age, d.PRNCPTX, d.smoke
	INTO jh_vasqip_control
	FROM #jh_tmp2 pts
	JOIN Src.VASQIP_nso_noncardiac d
		ON d.oprymd = pts.oprymd AND pts.scrssn = d.scrssn
END

IF @jh_tmp5 = 1 BEGIN
	DROP TABLE IF EXISTS #jh_ip_oud
	DROP TABLE IF EXISTS #jh_op_oud
	DROP TABLE IF EXISTS #jh_tmp1
	DROP TABLE IF EXISTS #jh_tmp2
END

-- takes jh_control_list (uploaded from R)
--    populates the table with VASQIP variables
IF @jh_tmp6 = 1 BEGIN
	DROP TABLE IF EXISTS #jh_tmp3
	SELECT pts.scrssn, pts.oprymd, sp.PatientICN
	INTO #jh_tmp3
	FROM jh_control_list pts
	JOIN Src.SPatient_SPatient sp
		ON sp.ScrSSN = pts.scrssn
	GROUP BY pts.scrssn, pts.oprymd, sp.PatientICN

	DROP TABLE IF EXISTS jh_vasqip_control
	SELECT d.scrssn, pts.PatientICN,
		d.ethnicity, d.race1, d.sex, d.etoh, d.smoke, d.bmi, d.age,
		d.oprymd, d.opdt, d.PRNCPTX, d.speccode, d.optime, d.PODIAG, 
		d.asaclas, d.ASAINDEX,
		d.TOTHLOS, d.TOTSLOS, d.HOSPADM, d.inout, d.POSTCODE, d.tpatin, d.tpatout, d.pacu,
		d.returnor, d.cdarrest, d.cdmi, d.cnscva, d.cnscva_n, d.failwean, d.pulembol, d.oupneumo, d.MECHVENT, d.reintub, d.renainsf,
		d.urninfec, d.dehis, d.orgspcssi, d.supinfec, d.wndinfd
	INTO jh_vasqip_control
	FROM Src.VASQIP_nso_noncardiac d
	JOIN #jh_tmp3 pts	
		ON pts.oprymd = d.oprymd AND pts.scrssn = d.scrssn
	;

	DROP TABLE IF EXISTS #jh_tmp3
END