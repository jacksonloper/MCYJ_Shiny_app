# DATA.xlsx - Data Dictionary

This document describes the structure and contents of the `DATA.xlsx` file, which contains data about Special Investigation Reports (SIRs) for child care facilities in Michigan.

## Overview

The `DATA.xlsx` file contains three sheets:
1. **INFORMATION** - Facility and investigation metadata
2. **ALLEGATIONS** - Details about alleged violations
3. **RULES** - Rule codes and their compliance status

---

## Sheet 1: INFORMATION

Contains metadata about facilities and their special investigation reports.

**Dimensions:** 1,809 rows × 22 columns

### Columns

| Column | Name | Type | Description | Sample Values |
|--------|------|------|-------------|---------------|
| 1 | File Name | Text | Name of the PDF file containing the report | `2024C0001132SI.pdf`, `2024SIC0001505.pdf` |
| 2 | License # | Text | Facility license number | `CM390412729`, `CI820200982` |
| 3 | Investigation # | Text | Unique investigation identifier | `2024C0001132SI`, `2024SIC0001505` |
| 4 | Final Report Date | DateTime | Date the investigation report was finalized | `2024-03-22`, `2024-06-27` |
| 5 | Administrator | Text | Name of the facility administrator | `Lauryn Ritchie`, `Tajuanda Carey` |
| 6 | Capacity | Integer | Maximum number of residents the facility can accommodate | `6`, `10` |
| 7 | Complaint Receipt Date | DateTime | Date the complaint was received | `2024-02-09`, `2024-06-11` |
| 8 | Investigation Initiation Date | DateTime | Date the investigation began | `2024-02-09`, `2024-06-11` |
| 9 | Effective Date | DateTime | License effective date | (mostly empty in sample data) |
| 10 | Expiration Date | DateTime | License expiration date | `2024-12-02`, `2026-08-14` |
| 11 | Facility Address | Text | Physical address of the facility | (contains full addresses) |
| 12 | Facility Telephone # | Text | Facility contact phone number | (contains phone numbers) |
| 13 | License Status | Text | Current status of the facility license | `regular`, `1st` |
| 14 | Licensee Address | Text | Address of the license holder | (contains full addresses) |
| 15 | Licensee Designee | Text | Designated representative for the licensee | `Tonya Moore`, `Stacie Bowens` |
| 16 | Licensee Name | Text | Name of the license holder (individual or organization) | `Thorpe, Kimberly`, `Spectrum Child & Family Services` |
| 17 | Licensee Telephone # | Text | Licensee contact phone number | (contains phone numbers) |
| 18 | Facility Name | Text | Official name of the facility | `LTAI IV DD/CI`, `Beverly House` |
| 19 | Original Issuance Date | DateTime | Date the license was originally issued | (mostly empty in sample data) |
| 20 | Program Type | Text | Type of child care program | Values processed in code:<br>- `CHILD CARING INSTITUTION, GOVERNMENT - NON-FIA`<br>- `CHILD CARING INSTITUTION, FIA`<br>- `CHILD CARING INSTITUTION, PRIVATE`<br>- `CHILD PLACING AGENCY, FIA`<br>- `CHILD PLACING AGENCY, PRIVATE`<br>- `CHILD PLACING AGENCY`<br>- `CHILD THERAPEUTIC GROUP HOME`<br>- `COURT OPERATED RESIDENTIAL CARE FACILITY`<br>- `OTHER` |
| 21 | Recommendation | Text | Final recommendation from the investigation | Examples:<br>- "Based on investigative findings the facility is not in compliance with all applicable licensing stat..."<br>- "Based on investigative findings the facility is in compliance with all applicable licensing statutes" |
| 22 | Number of Allegations | Integer | Total number of allegations in the investigation | `1`, `3` |

### Usage Notes

- The `Program Type` field is processed during data loading (see `global.R` lines 36-74) to normalize various formats into standard categories
- Date fields are stored in Excel datetime format
- This sheet is joined with the ALLEGATIONS and RULES sheets using the `File Name` column as the key

---

## Sheet 2: ALLEGATIONS

Contains detailed information about each allegation investigated in the special investigation reports.

**Dimensions:** 3,139 rows × 328 columns (only first 6 columns contain data)

### Active Columns

| Column | Name | Type | Description | Sample Values |
|--------|------|------|-------------|---------------|
| 1 | File Name | Text | Links to the corresponding investigation (foreign key to INFORMATION sheet) | `2024C0001132SI.pdf`, `2024SIC0001505.pdf` |
| 2 | Allegation | Text | Description of the alleged violation | "For the past three months, three staff members have been sleeping on the couch during their night sh..."<br>"Documentation is falsified to reflect the observation checks are occurring." |
| 3 | Investigation | Text | Summary of the investigation process and findings | "INTERVIEWS: Complainant/Staff 1 was interviewed via phone on 2/13/24. Staff 1 reported she no longer..." |
| 4 | Analysis | Text | Detailed analysis of the evidence and determination | "Based on interviews conducted, video footage reviewed, and documentation reviewed, this violation is..." |
| 5 | Conclusion | Text | Final determination of the allegation | `Violation Established`, `Violation Not Established` |
| 6 | Violation Established | Text | Boolean indicator (Yes/No) | `Yes`, `No` |
| 7-328 | (Empty) | N/A | These columns exist in the Excel structure but contain no data | N/A |

### Usage Notes

- Only columns A through F (1-6) are read by the application (see `global.R` line 75)
- The `Violation Established` column is used for filtering and analysis in violation statistics
- Each row represents one allegation, and investigations can have multiple allegations
- The `File Name` column is used to join with the INFORMATION sheet for additional context

---

## Sheet 3: RULES

Contains information about specific rule violations cited in investigations.

**Dimensions:** 3,080 rows × 5 columns

### Columns

| Column | Name | Type | Description | Sample Values |
|--------|------|------|-------------|---------------|
| 1 | File Name | Text | Links to the corresponding investigation (foreign key to INFORMATION sheet) | `2024C0001132SI.pdf`, `2024SIC0001505.pdf` |
| 2 | Rule | Text | Rule code number identifying the specific regulation | `400.4127`, `116 - 722.120`, `400.413` |
| 3 | Description | Text | Full text description of the rule and its requirements | "Rule Code & CCI Rule 400.4127 Staff to resident ratio Section (4) When residents are asleep or other..."<br>"Rule Code & ACT 116 - 722.120 Investigation, inspection, and examination Section of conditions, book..." |
| 4 | Conclusion | Text | Final determination for this specific rule | `Violation Established`, `Violation Not Established` |
| 5 | Violation Established | Text | Boolean indicator (Yes/No) | `Yes`, `No` |

### Usage Notes

- All 5 columns are read as text type (see `global.R` line 77)
- Multiple rules can be cited for a single investigation
- The `Rule` column is used to generate statistics about the most frequently violated rules
- This data is used in the "Top 10 Rule Codes" visualization in the dashboard

---

## Data Relationships

The three sheets are related as follows:

```
INFORMATION (1,809 investigations)
    ├── File Name (Primary Key)
    │
    ├─→ ALLEGATIONS (3,139 allegations)
    │       └── File Name (Foreign Key)
    │
    └─→ RULES (3,080 rule citations)
            └── File Name (Foreign Key)
```

One investigation (INFORMATION) can have:
- Multiple allegations (ALLEGATIONS)
- Multiple rule citations (RULES)

---

## Data Loading

The data is loaded in `global.R` using the `readData()` function:

```r
data_path = "sources/DATA.xlsx"

data <- list(
  info = read_excel(data_path, "INFORMATION"),
  violations = read_excel(data_path, "ALLEGATIONS", range = cell_cols("A:F")),
  rules = read_excel(data_path, "RULES", col_types = rep("text", 5))
)
```

---

## Data Quality Notes

1. **Empty Columns**: The ALLEGATIONS sheet has 322 empty columns (7-328) that are artifacts of the data extraction process
2. **Date Fields**: Some date fields (Effective Date, Original Issuance Date) have limited data coverage
3. **Text Truncation**: Long text fields (Allegation, Investigation, Analysis, Description) may be truncated in samples but contain full text in the actual data
4. **Program Type Normalization**: The Program Type field requires normalization during loading due to inconsistent formatting in source data

---

## Update History

- Data contains Special Investigation Reports from Michigan Child & Youth Justice facilities
- Reports span multiple years (earliest visible: 2017+)
- Last data extraction: As indicated by most recent Final Report Dates in the data

---

## Contact & Questions

For questions about the data structure or content, refer to:
- The Michigan Department of Health and Human Services licensing division
- The Special Investigation Report archive
