# DATA_SOURCES.md - Quick Reference Guide

## 🔴 CRITICAL: Information You MUST Fill In

### Highest Priority (Do This First!)

#### 1. Reclamation Data (Section 3.5)
**Why Critical:** This is your main predictor variable. Reviewers will ask detailed questions.

**Must Answer:**
- [ ] What is the original data source? (satellite imagery? government data? published dataset?)
- [ ] How did you create/obtain this raster?
- [ ] What years of reclamation does it include?
- [ ] How was it validated?
- [ ] Can other researchers access it?

**Example Answer:**
```markdown
**Source:** Custom compilation from Landsat imagery analysis

**Methodology:**
1. Data Sources:
   - Landsat 5/7/8 imagery (1985-2020) from Google Earth Engine
   - Validated against government reclamation reports from China, UAE, Singapore
   
2. Spatial Processing:
   - Supervised classification of coastal land cover change
   - Manual digitization of reclamation boundaries
   - Aggregated to 2.5 arcmin by summing reclaimed area per pixel
   
3. Temporal Aggregation:
   - Cumulative total of all reclamation from 1985-2020
   
4. Validation:
   - Compared with published reclamation statistics (R² = 0.89)
   - Ground-truthed for 20 major cities
   - Overall accuracy: 87%
```

#### 2. Anthropogenic Data Sources (Sections 3.1-3.4)

**Built-up Area (Section 3.1):**
- [ ] Source name (likely: ESA WorldCover, GHSL, or similar)
- [ ] Why year 2025? (typo or projection?)
- [ ] Original resolution

**GDP (Section 3.2):**
- [ ] Source (Kummu et al.? World Bank?)
- [ ] Currency and base year
- [ ] How GDP was gridded

**Population (Section 3.3):**
- [ ] Source (WorldPop? GPWv4? LandScan? GHS-POP?)
- [ ] Estimation method
- [ ] Original resolution

**VIIRS Night-lights (Section 3.4):**
- [ ] Exact product version (VNL v2? v2.1?)
- [ ] Annual composite or monthly?
- [ ] Exact year/month of 2023 data

### Medium Priority

#### 3. GBIF Data (Section 1.1)
- [ ] Date you downloaded the data
- [ ] GBIF download DOI (get this from GBIF after download)
- [ ] Total number of records
- [ ] Date range of records

#### 4. GloNAF Data (Section 1.2)
- [ ] Date you accessed the database
- [ ] Version number
- [ ] How you obtained access

#### 5. Download Dates
Go through each section and fill in:
- [ ] WorldClim download date
- [ ] SoilGrids download date
- [ ] Bio-ORACLE download date
- [ ] All other download dates

---

## 📝 How to Update When Adding New Data

### Step 1: Add to Quick Reference Table
At the top of DATA_SOURCES.md, add a row:

```markdown
| [Data Type] | [Source] | [Resolution] | [Temporal] | [Access] | ✅/⚠️/🔴 |
```

### Step 2: Create New Section
Copy the template from Section 7 and fill in:

```markdown
### X.X [Your Data Source Name]

**Full Name:** [Full official name]
**Website:** [URL where others can find it]

**Citation:**
[Exact citation - copy from the data provider's website]

**Access Information:**
- **License:** [Check the license! CC-BY? CC0? Proprietary?]
- **Resolution:** [Original resolution] → resampled to 2.5 arcmin
- **Temporal Coverage:** [What years/period?]
- **Download Date:** [When YOU downloaded it]
- **File Name:** [Exact filename in your project]

**Description:**
[1-2 sentences: What is this data and why are you using it?]

**Processing:**
[What did you do to this data? Which script?]
```

### Step 3: Update Change Log
Add a row to the table in Section 7:

```markdown
| 2026-01-XX | Added [Data Source Name] | Binyue Kang |
```

### Step 4: Update Acknowledgments
Add the data provider to Section 10 if they should be acknowledged.

---

## ✅ Pre-Submission Checklist

Before submitting your manuscript, go through Section 8 and check every box.

**Most Important:**
- [ ] Reclamation data fully documented
- [ ] All [FILL IN] placeholders removed
- [ ] All data sources have proper citations
- [ ] Data availability statement complete

---

## 🎯 Common Data Sources (For Your Reference)

### Population Data Options:
1. **WorldPop** - https://www.worldpop.org
   - High resolution, well-validated
   - Free, CC-BY 4.0
   
2. **GPWv4** - https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
   - UN-adjusted, widely used
   - Free, CC-BY 4.0
   
3. **GHS-POP** - https://ghsl.jrc.ec.europa.eu
   - From European Commission
   - Free, CC-BY 4.0

### GDP Data Options:
1. **Kummu et al. (2018)** - https://doi.org/10.1038/sdata.2018.4
   - Gridded GDP at 5 arcmin
   - Widely cited
   
2. **G-Econ** - http://gecon.yale.edu
   - Economic activity data
   - Free for research

### Built-up Area Options:
1. **ESA WorldCover** - https://worldcover2021.esa.int
   - 10m resolution, 2021
   - Free, CC-BY 4.0
   
2. **GHSL Built-up** - https://ghsl.jrc.ec.europa.eu
   - Multiple years available
   - Free, CC-BY 4.0

---

## 💡 Tips for Writing Data Descriptions

### Good Example:
```markdown
**Description:**
Population density data from WorldPop 2020, representing estimated 
number of people per km² at 100m resolution. Used as a proxy for 
human disturbance and propagule pressure in urban areas. Resampled 
to 2.5 arcmin using sum aggregation to preserve total population counts.
```

### Bad Example:
```markdown
**Description:**
Population data.
```

### Key Elements:
1. **What:** What does the data represent?
2. **Why:** Why are you using it in your study?
3. **How:** How did you process it?

---

## 📧 Getting Help

If you're unsure about:
- **Citation format:** Check the data provider's website - they usually provide a recommended citation
- **License:** Look for "Terms of Use" or "Data Policy" on the website
- **Methodology:** Read the associated paper (usually linked on the data website)
- **Access:** Check if there's a DOI, or if data requires registration

---

## 🔄 Version Control

Every time you update DATA_SOURCES.md:
1. Update the "Last Updated" date at the top
2. Add an entry to the Change Log (Section 7)
3. Commit to git with a clear message: "Updated DATA_SOURCES: added [source name]"

---

**Remember:** Better to over-document than under-document. Reviewers appreciate thoroughness!
