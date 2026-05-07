# Import all required libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import timedelta
import warnings
warnings.filterwarnings('ignore')

# For advanced visualizations
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots

import subprocess
import sys

subprocess.check_call([sys.executable, "-m", "pip", "install", "lifelines"])
# For survival analysis

try:
    from lifelines import KaplanMeierFitter, CoxPHFitter
    from lifelines.statistics import logrank_test
    LIFELINES_AVAILABLE = True
except ImportError:
    LIFELINES_AVAILABLE = False
    print("Warning: lifelines package not installed.")
    print("Run: pip install lifelines")
    print("Continuing with basic analysis...")


# For network/sankey diagrams
#~!pip install networkx -q  # Uncomment if needed
import networkx as nx

# Set styles
plt.style.use('seaborn-v0_8-darkgrid')
sns.set_palette("husl")
import os
print("Current working directory:", os.getcwd())
print("Files in this directory:", os.listdir())
print("hey")
# Load and prepare data
# Set your working directory
working_dir = r"c:/Your/Path"

# Change to that directory
os.chdir(working_dir)
df = pd.read_csv("patient_admissions.csv")

# Convert dates
df['admission_date'] = pd.to_datetime(df['admission_date'], format='%d/%m/%Y')
df['discharge_date'] = pd.to_datetime(df['discharge_date'], format='%d/%m/%Y')
df['los'] = (df['discharge_date'] - df['admission_date']).dt.days

# Create cleaned dataset
df_clean = df[
    (df['los'] > 0) & 
    (df['los'].notna()) & 
    (df['age'] > 0) & 
    (df['age'] < 120)
].copy()

# Add derived variables
df_clean['age_group'] = pd.cut(df_clean['age'], bins=[0, 40, 65, 80, 200], 
                                labels=['18-40', '41-65', '66-80', '80+'])
df_clean['month'] = df_clean['admission_date'].dt.month
df_clean['year'] = df_clean['admission_date'].dt.year

print(f"Dataset loaded: {len(df_clean)} clean records")
print(f"Date range: {df_clean['admission_date'].min()} to {df_clean['admission_date'].max()}")

# Exercise 1: Analyzing Bed Occupancy and Identifying Capacity Pressure Points

def calculate_occupancy(data, total_beds=500):
    """
    Calculate daily bed occupancy and identify pressure points
    
    Parameters:
    - data: DataFrame with admission_date and discharge_date
    - total_beds: Total available beds in the Trust
    
    Returns:
    - occupancy_df: Daily occupancy rates
    - pressure_dates: Dates when occupancy exceeded 95%
    - diagnosis_breakdown: Top diagnosis groups during high occupancy
    """
    
    # Create date range for the entire period
    date_range = pd.date_range(
        start=data['admission_date'].min(),
        end=data['discharge_date'].max(),
        freq='D'
    )
    
    # Calculate occupied beds each day
    occupancy = []
    for date in date_range:
        # Count patients in hospital on this date
        occupied = sum(
            (data['admission_date'] <= date) & 
            (data['discharge_date'] >= date)
        )
        occupancy.append(occupied)
    
    # Create occupancy dataframe
    occupancy_df = pd.DataFrame({
        'date': date_range,
        'occupied_beds': occupancy,
        'occupancy_rate': (np.array(occupancy) / total_beds * 100).round(1)
    })
    
    # Identify pressure dates (>95% occupancy)
    pressure_dates = occupancy_df[occupancy_df['occupancy_rate'] > 95].copy()
    
    # Find diagnosis breakdown during high pressure periods
    high_pressure_periods = pressure_dates['date'].tolist()
    
    if high_pressure_periods:
        # Get admissions during high pressure periods (7-day windows)
        pressure_admissions = []
        for pressure_date in high_pressure_periods[:10]:  # Limit to first 10
            window_start = pressure_date - timedelta(days=7)
            window_end = pressure_date + timedelta(days=7)
            window_admissions = data[
                (data['admission_date'] >= window_start) & 
                (data['admission_date'] <= window_end)
            ]
            pressure_admissions.append(window_admissions)
        
        if pressure_admissions:
            all_pressure_admissions = pd.concat(pressure_admissions)
            diagnosis_breakdown = all_pressure_admissions['primary_diagnosis'].value_counts().head(5)
        else:
            diagnosis_breakdown = pd.Series()
    else:
        diagnosis_breakdown = pd.Series()
    
    return occupancy_df, pressure_dates, diagnosis_breakdown

# Run the analysis
occupancy_df, pressure_dates, diagnosis_breakdown = calculate_occupancy(df_clean, total_beds=500)

# Display results
print("="*60)
print("BED OCCUPANCY ANALYSIS")
print("="*60)
print(f"\nAverage occupancy rate: {occupancy_df['occupancy_rate'].mean():.1f}%")
print(f"Maximum occupancy: {occupancy_df['occupancy_rate'].max():.1f}%")
print(f"Days with >95% occupancy: {len(pressure_dates)}")

if len(pressure_dates) > 0:
    print(f"\nFirst 5 pressure dates:")
    print(pressure_dates[['date', 'occupancy_rate']].head())

print(f"\nTop diagnoses during high occupancy periods:")
if len(diagnosis_breakdown) > 0:
    for dx, count in diagnosis_breakdown.items():
        print(f"  {dx}: {count} admissions")
else:
    print("  No high occupancy periods detected")

# Visualization 1: Occupancy timeline
fig, ax = plt.subplots(figsize=(14, 6))
ax.plot(occupancy_df['date'], occupancy_df['occupancy_rate'], 
        linewidth=1, color='steelblue', alpha=0.7)
ax.axhline(y=85, color='orange', linestyle='--', label='85% Warning Threshold', alpha=0.7)
ax.axhline(y=95, color='red', linestyle='--', label='95% Critical Threshold', alpha=0.7)
ax.fill_between(occupancy_df['date'], occupancy_df['occupancy_rate'], 95,
                where=(occupancy_df['occupancy_rate'] > 95), 
                color='red', alpha=0.3, label='Critical Capacity')
ax.set_xlabel('Date', fontsize=12)
ax.set_ylabel('Occupancy Rate (%)', fontsize=12)
ax.set_title('Daily Bed Occupancy Rate - NHS Trust\nRed areas indicate critical capacity (>95%)', 
             fontsize=14, fontweight='bold')
ax.legend()
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('bed_occupancy.png', dpi=300, bbox_inches='tight')
plt.show()

# Visualization 2: Occupancy distribution
fig, ax = plt.subplots(figsize=(10, 6))
ax.hist(occupancy_df['occupancy_rate'], bins=30, color='steelblue', alpha=0.7, edgecolor='black')
ax.axvline(x=85, color='orange', linestyle='--', label='85% Threshold', linewidth=2)
ax.axvline(x=95, color='red', linestyle='--', label='95% Threshold', linewidth=2)
ax.set_xlabel('Occupancy Rate (%)', fontsize=12)
ax.set_ylabel('Number of Days', fontsize=12)
ax.set_title('Distribution of Bed Occupancy Rates', fontsize=14, fontweight='bold')
ax.legend()
plt.tight_layout()
plt.savefig('occupancy_distribution.png', dpi=300, bbox_inches='tight')
plt.show()

print("\n✓ Exercise 1 Complete: Identified capacity pressure points and high-demand diagnoses")

# Exercise 2: Mapping Patient Pathways and Identifying Care Home Discharge Patterns

def analyze_patient_flow(data):
    """
    Analyze patient flow through discharge destinations and readmissions
    
    Returns:
    - transition_matrix: Probability of moving between discharge destinations
    - care_home_diagnoses: Diagnoses most likely to be discharged to care homes
    - sankey_data: Data ready for sankey visualization
    """
    
    # 1. Calculate transition probabilities between discharge destinations
    # For patients with multiple admissions, track their discharge pathway
    patients_with_multiple = data.groupby('patient_id').size()
    multiple_admission_patients = patients_with_multiple[patients_with_multiple > 1].index
    
    transitions = []
    for patient in multiple_admission_patients:
        patient_data = data[data['patient_id'] == patient].sort_values('admission_date')
        destinations = patient_data['discharge_destination'].tolist()
        
        # Create transitions between consecutive discharges
        for i in range(len(destinations) - 1):
            transitions.append((destinations[i], destinations[i+1]))
    
    # Build transition matrix
    transition_df = pd.DataFrame(transitions, columns=['from', 'to'])
    transition_matrix = pd.crosstab(transition_df['from'], transition_df['to'], normalize='index')
    
    # 2. Diagnoses most often discharged to care homes
    care_home_patients = data[data['discharge_destination'] == 'Care home']
    care_home_diagnoses = care_home_patients['primary_diagnosis'].value_counts().head(10)
    care_home_percentages = (care_home_patients['primary_diagnosis'].value_counts() / 
                              data['primary_diagnosis'].value_counts() * 100).round(1)
    
    care_home_summary = pd.DataFrame({
        'diagnosis': care_home_diagnoses.index,
        'count': care_home_diagnoses.values,
        'percentage_of_diagnosis': [care_home_percentages[dx] for dx in care_home_diagnoses.index]
    }).head(5)
    
    # 3. Prepare data for sankey diagram
    # Nodes: Admission -> Discharge Destination -> Readmission status
    flow_data = data.groupby(['discharge_destination', 'readmitted_30d']).size().reset_index()
    flow_data.columns = ['destination', 'readmitted', 'count']
    
    # Create sankey-ready format
    sankey_data = {
        'source': [],
        'target': [],
        'value': [],
        'labels': ['Admission'] + list(flow_data['destination'].unique()) + ['No Readmission', 'Readmitted']
    }
    
    # Map names to indices
    label_to_index = {label: i for i, label in enumerate(sankey_data['labels'])}
    
    # Add flows: Admission -> Destination
    dest_counts = data['discharge_destination'].value_counts()
    for dest, count in dest_counts.items():
        sankey_data['source'].append(label_to_index['Admission'])
        sankey_data['target'].append(label_to_index[dest])
        sankey_data['value'].append(count)
    
    # Add flows: Destination -> Readmission status
    for _, row in flow_data.iterrows():
        dest = row['destination']
        readmit_status = 'Readmitted' if row['readmitted'] == 1 else 'No Readmission'
        sankey_data['source'].append(label_to_index[dest])
        sankey_data['target'].append(label_to_index[readmit_status])
        sankey_data['value'].append(row['count'])
    
    return transition_matrix, care_home_summary, sankey_data

# Run the analysis
transition_matrix, care_home_summary, sankey_data = analyze_patient_flow(df_clean)

# Display results
print("="*60)
print("PATIENT FLOW ANALYSIS")
print("="*60)

print("\n1. Transition Matrix (probability of moving between discharge destinations):")
print(transition_matrix.round(2))

print("\n2. Top 5 Diagnoses Discharged to Care Homes:")
print(care_home_summary.to_string(index=False))

# Visualization 1: Sankey Diagram using Plotly
fig = go.Figure(data=[go.Sankey(
    node=dict(
        pad=15,
        thickness=20,
        line=dict(color="black", width=0.5),
        label=sankey_data['labels'],
        color="blue"
    ),
    link=dict(
        source=sankey_data['source'],
        target=sankey_data['target'],
        value=sankey_data['value']
    )
)])

fig.update_layout(
    title_text="Patient Flow: Admission → Discharge → Readmission",
    font_size=12,
    width=1000,
    height=600
)
fig.write_html("patient_flow_sankey.html")
fig.show()

# Visualization 2: Care home discharge patterns
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

# Bar chart of top diagnoses to care homes
axes[0].barh(care_home_summary['diagnosis'], care_home_summary['count'], color='coral')
axes[0].set_xlabel('Number of Patients', fontsize=12)
axes[0].set_title('Top Diagnoses Discharged to Care Homes', fontweight='bold')
axes[0].invert_yaxis()

# Percentage of each diagnosis going to care homes
care_home_by_dx = df_clean.groupby('primary_diagnosis').apply(
    lambda x: (x['discharge_destination'] == 'Care home').mean() * 100
).sort_values(ascending=False).head(10)

axes[1].barh(range(len(care_home_by_dx)), care_home_by_dx.values, color='skyblue')
axes[1].set_yticks(range(len(care_home_by_dx)))
axes[1].set_yticklabels(care_home_by_dx.index)
axes[1].set_xlabel('Percentage (%)', fontsize=12)
axes[1].set_title('Percentage of Diagnoses Sent to Care Homes', fontweight='bold')
axes[1].invert_yaxis()

plt.suptitle('Care Home Discharge Patterns', fontsize=14, fontweight='bold')
plt.tight_layout()
plt.savefig('care_home_patterns.png', dpi=300, bbox_inches='tight')
plt.show()

# Visualization 3: Network graph of patient transfers
if len(transition_matrix) > 0:
    G = nx.DiGraph()
    
    # Add edges with weights
    for from_dest in transition_matrix.index:
        for to_dest in transition_matrix.columns:
            weight = transition_matrix.loc[from_dest, to_dest]
            if weight > 0 and not pd.isna(weight):
                G.add_edge(from_dest, to_dest, weight=weight)
    
    fig, ax = plt.subplots(figsize=(10, 8))
    pos = nx.spring_layout(G, k=2, seed=42)
    
    # Draw nodes
    nx.draw_networkx_nodes(G, pos, node_size=3000, node_color='lightblue', alpha=0.7)
    
    # Draw edges with width proportional to weight
    edges = G.edges()
    weights = [G[u][v]['weight'] * 3 for u, v in edges]
    nx.draw_networkx_edges(G, pos, width=weights, edge_color='gray', alpha=0.5, 
                          connectionstyle="arc3,rad=0.1", arrowsize=20)
    
    # Draw labels
    nx.draw_networkx_labels(G, pos, font_size=10, font_weight='bold')
    
    ax.set_title('Patient Transfer Network Between Discharge Destinations\nEdge width = transition probability', 
                fontsize=14, fontweight='bold')
    ax.axis('off')
    plt.tight_layout()
    plt.savefig('transfer_network.png', dpi=300, bbox_inches='tight')
    plt.show()

print("\n✓ Exercise 2 Complete: Mapped patient pathways and identified care home patterns")

# Exercise 3: Validating Clinical Coding and Identifying Potential Errors

def flag_suspicious_records(data):
    """
    Identify potentially erroneous or suspicious clinical coding
    
    Returns:
    - suspicious_flags: DataFrame with flagged records and reasons
    - validation_summary: Summary counts by flag type
    """
    
    df_flagged = data.copy()
    df_flagged['flag_reason'] = ''
    df_flagged['flag_type'] = ''
    
    # Flag 1: Long LOS for syncope (normally short stay)
    syncope_mask = (df_flagged['primary_diagnosis'] == 'R55') & (df_flagged['los'] > 14)
    df_flagged.loc[syncope_mask, 'flag_reason'] = 'Unusually long LOS for syncope (>14 days)'
    df_flagged.loc[syncope_mask, 'flag_type'] = 'Coding_error_suspected'
    
    # Flag 2: Young patients discharged to care home
    young_carehome_mask = (df_flagged['age'] < 65) & (df_flagged['discharge_destination'] == 'Care home')
    df_flagged.loc[young_carehome_mask, 'flag_reason'] = 'Patient under 65 discharged to care home'
    df_flagged.loc[young_carehome_mask, 'flag_type'] = 'Unusual_pathway'
    
    # Flag 3: Multiple admissions within 7 days (possible coding duplication)
    # For each patient, identify admissions within 7 days of each other
    duplicate_admissions = []
    for patient in df_flagged['patient_id'].unique():
        patient_data = df_flagged[df_flagged['patient_id'] == patient].sort_values('admission_date')
        
        for i in range(len(patient_data) - 1):
            days_between = (patient_data.iloc[i + 1]['admission_date'] - 
                          patient_data.iloc[i]['discharge_date']).days
            
            if 0 <= days_between <= 7:
                duplicate_admissions.append(patient_data.iloc[i + 1].name)  # Index of second admission
    
    df_flagged.loc[df_flagged.index.isin(duplicate_admissions), 'flag_reason'] = (
        df_flagged.loc[df_flagged.index.isin(duplicate_admissions), 'flag_reason'] + 
        '; Multiple admissions within 7 days (possible duplicate)'
    ).str.strip('; ')
    df_flagged.loc[df_flagged.index.isin(duplicate_admissions), 'flag_type'] = 'Potential_duplicate'
    
    # Flag 4: Impossible LOS (already cleaned, but check for edge cases)
    impossible_los_mask = (df_flagged['los'] > 60) | (df_flagged['los'] < 1)
    df_flagged.loc[impossible_los_mask, 'flag_reason'] = 'Impossible length of stay'
    df_flagged.loc[impossible_los_mask, 'flag_type'] = 'Data_quality'
    
    # Flag 5: Missing critical data
    missing_data_mask = df_flagged['num_comorbidities'].isna() | df_flagged['discharge_destination'].isna()
    df_flagged.loc[missing_data_mask, 'flag_reason'] = 'Missing critical clinical data'
    df_flagged.loc[missing_data_mask, 'flag_type'] = 'Missing_data'
    
    # Flag 6: Extreme age with unlikely diagnosis (screening)
    extreme_age_mask = (df_flagged['age'] > 100) & (df_flagged['primary_diagnosis'] == 'S06')
    df_flagged.loc[extreme_age_mask, 'flag_reason'] = 'Extreme age with head injury - verify coding'
    df_flagged.loc[extreme_age_mask, 'flag_type'] = 'Clinical_review'
    
    # Filter to flagged records only
    flagged_records = df_flagged[df_flagged['flag_reason'] != ''].copy()
    
    # Summary by flag type
    validation_summary = flagged_records['flag_type'].value_counts()
    
    return flagged_records, validation_summary

# Run validation
flagged_records, validation_summary = flag_suspicious_records(df_clean)

print("="*60)
print("CLINICAL CODING VALIDATION REPORT")
print("="*60)
print(f"\nTotal records reviewed: {len(df_clean):,}")
print(f"Records flagged for review: {len(flagged_records):,} ({len(flagged_records)/len(df_clean)*100:.1f}%)")
print(f"\nFlag types found:")
for flag_type, count in validation_summary.items():
    print(f"  {flag_type}: {count} records")

print(f"\nFirst 10 flagged records:")
print(flagged_records[['patient_id', 'age', 'primary_diagnosis', 'los', 
                       'discharge_destination', 'flag_type', 'flag_reason']].head(10))

# Visualization 1: Flag types distribution
fig, ax = plt.subplots(figsize=(10, 6))
validation_summary.plot(kind='bar', color=['red', 'orange', 'gold', 'coral', 'salmon'], ax=ax)
ax.set_xlabel('Flag Type', fontsize=12)
ax.set_ylabel('Number of Records', fontsize=12)
ax.set_title('Clinical Coding Issues Detected', fontsize=14, fontweight='bold')
ax.set_xticklabels(ax.get_xticklabels(), rotation=45, ha='right')
for i, v in enumerate(validation_summary.values):
    ax.text(i, v + 5, str(v), ha='center', fontweight='bold')
plt.tight_layout()
plt.savefig('coding_issues.png', dpi=300, bbox_inches='tight')
plt.show()

# Visualization 2: Age distribution of care home discharges
fig, axes = plt.subplots(1, 2, figsize=(14, 5))

# Age distribution for care home vs home discharge
care_home_ages = df_clean[df_clean['discharge_destination'] == 'Care home']['age']
home_ages = df_clean[df_clean['discharge_destination'] == 'Home']['age']

axes[0].hist(care_home_ages, bins=20, alpha=0.7, label='Care home', color='red')
axes[0].hist(home_ages, bins=20, alpha=0.7, label='Home', color='green')
axes[0].set_xlabel('Age', fontsize=12)
axes[0].set_ylabel('Frequency', fontsize=12)
axes[0].set_title('Age Distribution by Discharge Destination', fontweight='bold')
axes[0].legend()
axes[0].axvline(x=65, color='blue', linestyle='--', label='Age 65 threshold')

# Young care home patients (flagged)
young_carehome = flagged_records[flagged_records['flag_type'] == 'Unusual_pathway']
if len(young_carehome) > 0:
    axes[1].scatter(young_carehome['age'], young_carehome['los'], 
                   alpha=0.7, s=100, c='red', label='Flagged: Under 65 to care home')
    axes[1].scatter(df_clean[df_clean['discharge_destination'] == 'Care home']['age'],
                   df_clean[df_clean['discharge_destination'] == 'Care home']['los'],
                   alpha=0.3, s=20, c='gray', label='Normal care home patients')
    axes[1].set_xlabel('Age', fontsize=12)
    axes[1].set_ylabel('Length of Stay (days)', fontsize=12)
    axes[1].set_title('Unusual Care Home Discharges (Under 65)', fontweight='bold')
    axes[1].legend()

plt.tight_layout()
plt.savefig('care_home_age_analysis.png', dpi=300, bbox_inches='tight')
plt.show()

# Export flagged records for clinical review
flagged_records.to_csv('flagged_coding_issues.csv', index=False)
print("\n✓ Flagged records exported to 'flagged_coding_issues.csv'")
print("✓ Exercise 3 Complete: Identified potential coding errors and unusual patterns")


# Exercise 4: Building Survival Models to Understand Time-to-Readmission Patterns

