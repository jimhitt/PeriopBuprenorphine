-- USE DATABASE_NAME

DECLARE @jh_tmp1	INT = 0		-- create perm table of case/control pts w/ ICN and SID
									-- jh_vasqip_all_pts
DECLARE @jh_tmp2	INT = 1		-- finds inpatient dx codes
DECLARE @jh_tmp3	INT = 1		-- finds outpatient dx codes
DECLARE @jh_tmp4	INT = 1		-- create jh_vasqip_all_dx from union of inpat/outpat
------------------------------
DECLARE @jh_start	INT = -365
DECLARE @jh_end		INT = 365

IF @jh_tmp1 = 1 BEGIN
/*	ONLY NEEDS TO BE RUN ONCE
	This creates a permenant table of patients (control and case)
	with PatientICN, PatientSID, scrssn, and oprymd
*/
	DROP TABLE IF EXISTS jh_vasqip_all_pts
	SELECT pts.scrssn, pts.OPRYMD, pts.type, sp.PatientICN, sp.PatientSID
	INTO jh_vasqip_all_pts
	FROM -- first collect case and control patients
		(SELECT a.scrssn, a.OPRYMD, 'case' "type"
		FROM jh_bup_surgery2 a
		UNION
		SELECT b.scrssn, b.OPRYMD, 'control' "type"
		FROM jh_control_list b) 
			pts
	-- join on Spatient to get SID and ICN
	JOIN Src.SPatient_SPatient sp
		ON sp.ScrSSN = pts.scrssn
END



IF @jh_tmp2 = 1 BEGIN
-- pulls inpatient dx and puts in 
-- PERM TABLE jh_vasqip_ip_dx
	DROP TABLE IF EXISTS jh_vasqip_ip_dx

	SELECT pts.PatientICN, pts.scrssn, pts.oprymd, pts.type,
		inpt.AdmitDateTime "CodeDateTime", inpt.InpatientSID "EncounterSID", 
		icd9.ICD9Code, icd10.ICD10Code
	INTO jh_vasqip_ip_dx
	FROM jh_vasqip_all_pts pts

	-- join on inpatient
	JOIN Src.Inpat_Inpatient									inpt
		ON inpt.PatientSID = pts.PatientSID
	JOIN Src.Inpat_InpatientDiagnosis							ipdx
		ON ipdx.InpatientSID = inpt.InpatientSID
	JOIN CDWWork.Dim.ICD9										icd9
		ON icd9.ICD9SID = ipdx.ICD9SID
	JOIN CDWWork.Dim.ICD10										icd10
		ON icd10.ICD10SID = ipdx.ICD10SID
	WHERE datediff(day, pts.OPRYMD, inpt.AdmitDateTime) BETWEEN @jh_start AND @jh_end
END

IF @jh_tmp3 = 1 BEGIN
	DROP TABLE IF EXISTS jh_vasqip_op_dx

	SELECT pts.PatientICN, pts.scrssn, pts.oprymd, pts.type,
		vdiag.VisitDateTime "CodeDateTime", vdiag.VisitSID "EncounterSID",
		icd9.ICD9Code, icd10.ICD10Code
	INTO jh_vasqip_op_dx

	FROM jh_vasqip_all_pts pts

	JOIN Src.Outpat_VDiagnosis							vdiag
		ON vdiag.PatientSID = pts.PatientSID
	JOIN CDWWork.Dim.ICD9								icd9
		ON icd9.ICD9SID = vdiag.ICD9SID
	JOIN CDWWork.Dim.ICD10								icd10
		ON icd10.ICD10SID = vdiag.ICD10SID

	WHERE datediff(day, pts.OPRYMD, vdiag.VisitDateTime) BETWEEN @jh_start AND @jh_end
END

/*	Combine jh_vasqip_op_dx and jh_vasqip_ip_dx
	using union
	save jh_vasqip_all_dx
	remove other tables
*/
IF @jh_tmp4 = 1 BEGIN
	DROP TABLE IF EXISTS jh_vasqip_all_dx
	
	SELECT *
	INTO jh_vasqip_all_dx
	FROM
		(SELECT inpt.*, 'inpt' "code_type"
		FROM jh_vasqip_ip_dx inpt

		UNION

		SELECT outpat.*, 'outpt' "code_type"
		FROM jh_vasqip_op_dx outpat) d
	
	DROP TABLE IF EXISTS jh_vasqip_op_dx
	DROP TABLE IF EXISTS jh_vasqip_ip_dx
END
