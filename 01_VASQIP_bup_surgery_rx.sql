-- USE DATABASE_NAME

DECLARE @jh_start	char(10) = '2010-01-01'
DECLARE @jh_end		char(10) = '2020-12-31'
-- range of preop rx (datediff call does not require negative numbers)
DECLARE @rx_start	int = 0
DECLARE @rx_end		int = 35
DECLARE @oud_dx		int = 365
-------------------------------------------------------
DECLARE @jh_tmp1	tinyint = 1		-- finds surgery in patients on buprenorphine
									-- outputs only ScrSSN and OPRYMD
DECLARE @jh_tmp2	tinyint = 1		-- collects and saves the VASQIP table for those patients
									-- saves jh_bup_surgery
DECLARE @jh_tmp3	tinyint = 1		-- find patients with OUD diagnosis w/in @oud_dx
									-- saves jh_bup_surgery2
DECLARE @jh_tmp4	tinyint = 1		-- clear temp tables

IF @jh_tmp1 = 1 BEGIN
	/*	Pull VASQIP ScrSSN and OPRYMD (op date) for patients with recent buprenorphine (not Butrans/Belbuca) rx
		Group by ScrSSN and OPRYMD to avoid redundancy
		Create temp table #jh_tmp1
		Extended the buprenorphine date range to 0 to 60 days to include recently prescribed.
		Can adjust the inclusion criteria later
	*/
	DROP TABLE IF EXISTS #jh_tmp1
	SELECT v.scrssn, v.OPRYMD
	INTO #jh_tmp1
	FROM Src.VASQIP_nso_noncardiac v
	JOIN Src.SPatient_SPatient sp
		ON sp.ScrSSN = v.scrssn
	JOIN Src.RxOut_RxOutpatFill rx
		ON rx.PatientSID = sp.PatientSID
	JOIN CDWWork.Dim.LocalDrug ld
		ON ld.LocalDrugSID = rx.LocalDrugSID
	WHERE ld.LocalDrugNameWithDose LIKE '%buprenorphine%'
		AND
		(	rx.IssueDate > @jh_start AND
			rx.IssueDate < @jh_end
		) AND (
			v.opdt > @jh_start AND
			v.opdt < @jh_end
		)
		AND ld.LocalDrugNameWithDose NOT LIKE '%patch%'
		AND ld.LocalDrugNameWithDose NOT LIKE '%mcg%'
		AND datediff(day, rx.DispensedDate, v.opdt) BETWEEN @rx_start AND @rx_end
	GROUP BY v.scrssn, v.OPRYMD
	ORDER BY 1
END


IF @jh_tmp3 = 1 BEGIN
	DROP TABLE IF EXISTS #jh_tmp2
	SELECT d.scrssn, d.oprymd
	INTO #jh_tmp2
	FROM #jh_tmp1 d

	-- Find patients with inpatient dx
	DROP TABLE IF EXISTS #jh_tmp_ip
	SELECT sp.PatientICN, pts.scrssn, pts.oprymd
	INTO #jh_tmp_ip
	FROM #jh_tmp2 pts
	JOIN Src.VASQIP_nso_noncardiac d
		ON d.scrssn = pts.scrssn AND d.oprymd = pts.oprymd
	JOIN Src.SPatient_SPatient sp
		ON pts.scrssn = sp.ScrSSN
	JOIN Src.Inpat_Inpatient inpat
		ON inpat.PatientSID = sp.PatientSID
	JOIN Src.Inpat_InpatientDiagnosis ipdx
		ON inpat.InpatientSID = ipdx.InpatientSID
	--JOIN Src.Outpat_VDiagnosis opdx
	--	ON opdx.PatientSID = sp.PatientSID
	JOIN CDWWork.Dim.ICD9 icd9
		ON icd9.ICD9SID = ipdx.ICD9SID
	JOIN CDWWork.Dim.ICD10 icd10
		ON icd10.ICD10SID = ipdx.ICD10SID

	WHERE (--	datediff(day, pts.oprymd, opdx.VDiagnosisDateTime) BETWEEN -1 * @oud_dx AND @oud_dx	OR
			datediff(day, pts.oprymd, ipdx.DischargeDateTime) BETWEEN -1 * @oud_dx AND 0
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
	GROUP BY sp.PatientICN, pts.scrssn, pts.oprymd

	-- find patients with outpatient DX
	DROP TABLE IF EXISTS #jh_tmp_op
	SELECT sp.PatientICN, pts.scrssn, pts.oprymd
	INTO #jh_tmp_op
	FROM #jh_tmp2 pts
	JOIN Src.VASQIP_nso_noncardiac d
		ON d.scrssn = pts.scrssn AND d.oprymd = pts.oprymd
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
	GROUP BY sp.PatientICN, pts.scrssn, pts.oprymd

	-- put them together and group to avoid duplicates
	DROP TABLE IF EXISTS #jh_tmp3
	SELECT pts.oprymd, pts.scrssn, pts.PatientICN
	INTO #jh_tmp3
	FROM (
			SELECT *
			FROM #jh_tmp_ip
		UNION
			SELECT *
			FROM #jh_tmp_op
		) pts
	GROUP BY pts.oprymd, pts.scrssn, pts.PatientICN

	-- pull the VASQIP columns
	DROP TABLE IF EXISTS jh_bup_surgery2
	SELECT d.scrssn, pts.PatientICN,
		d.ethnicity, d.race1, d.sex, d.etoh, d.smoke, d.bmi, d.age,
		d.oprymd, d.opdt, d.PRNCPTX, d.speccode, d.optime, d.PODIAG, 
		d.asaclas, d.ASAINDEX,
		d.TOTHLOS, d.TOTSLOS, d.HOSPADM, d.inout, d.POSTCODE, d.tpatin, d.tpatout, d.pacu,
		d.returnor, d.cdarrest, d.cdmi, d.cnscva, d.cnscva_n, d.failwean, d.pulembol, d.oupneumo, d.MECHVENT, d.reintub, d.renainsf,
		d.urninfec, d.dehis, d.orgspcssi, d.supinfec, d.wndinfd

	INTO jh_bup_surgery2
	FROM #jh_tmp3 pts
	JOIN Src.VASQIP_nso_noncardiac d
		ON d.scrssn = pts.scrssn AND d.OPRYMD = pts.OPRYMD
END

IF @jh_tmp4 = 1 BEGIN
	DROP TABLE IF EXISTS #jh_tmp_ip
	DROP TABLE IF EXISTS #jh_tmp_op
	DROP TABLE IF EXISTS #jh_tmp1
	DROP TABLE IF EXISTS #jh_tmp2
	DROP TABLE IF EXISTS #jh_tmp3
END