import pandas as pd

#Question A
df1 = pd.read_excel('201011.xlsx', header=1)
df1['year'] = 2010

df2 = pd.read_excel('201112.xlsx', header=1)
df2['year'] = 2011

df3 = pd.read_excel('201213.xlsx', header=1)
df3['year'] = 2012

df4 = pd.read_excel('201314.xlsx', header=1)
df4['year'] = 2013

df5 = pd.read_excel('201415.xlsx', header=1)
df5['year'] = 2014

df6 = pd.read_excel('201516.xlsx', header=1)
df6['year'] = 2015
financial_all = pd.concat([df1, df2, df3, df4, df5, df6], ignore_index=True)
print(financial_all.head())
print(financial_all.columns)

dff1 = pd.read_excel('hd2010.xlsx', header=1)
dff1['year'] = 2010

dff2 = pd.read_excel('hd2011.xlsx', header=1)
dff2['year'] = 2011

dff3 = pd.read_excel('hd2012.xlsx', header=1)
dff3['year'] = 2012

dff4 = pd.read_excel('hd2013.xlsx', header=1)
dff4['year'] = 2013

dff5 = pd.read_excel('hd2014.xlsx', header=1)
dff5['year'] = 2014

dff6 = pd.read_excel('hd2015.xlsx', header=1)
dff6['year'] = 2015
institutional_all = pd.concat([dff1, dff2, dff3, dff4, dff5, dff6], ignore_index=True)
print(institutional_all.head())
print(institutional_all.columns)


merged_df = pd.merge(financial_all, institutional_all, on=['UNITID', 'year'], how='inner')


print(merged_df.head())
print(merged_df.columns)

merged_df['degree_bach'] = (
    (merged_df['UGOFFER'] == 1) & (~merged_df['HLOFFER'].isin([1, 2, 3, 4]))
).astype(int)


merged_df['public'] = (merged_df['CONTROL'] == 1).astype(int)
merged_df['enroll_ftug'] = merged_df['SCUGFFN']
merged_df['grant_federal'] = merged_df['FGRNT_T']
merged_df = merged_df[merged_df['UGOFFER'] == 1]
print('UGOFFER' in merged_df.columns)

merged_df = merged_df[['UNITID', 'STABBR', 'year', 'degree_bach', 'public', 'enroll_ftug', 'grant_federal']].copy()
merged_df = merged_df.rename(columns={'UNITID': 'ID_IPEDS', 'STABBR': 'stabbr'})



print(merged_df.head())

merged_df = merged_df.dropna()

year_counts = merged_df.groupby('ID_IPEDS')['year'].nunique()


valid_unitids = year_counts[year_counts == 6].index


balanced_df = merged_df[merged_df['ID_IPEDS'].isin(valid_unitids)]

exclude_states = ["DC", "FM", "MH", "MP", "PR", "PW", "VI", "GU", "AS"]
filtered_df = balanced_df[~balanced_df["stabbr"].isin(exclude_states)]




print(filtered_df.head())


print(filtered_df.shape)


print(filtered_df['stabbr'].unique())




print(filtered_df.groupby('ID_IPEDS')['year'].nunique().value_counts())



#B. Questions: Enrollment

subset_df = filtered_df[(filtered_df['public'] == 1) & (filtered_df['degree_bach'] == 0)]
enroll_by_year = subset_df.groupby('year')['enroll_ftug'].sum().reset_index()


import matplotlib.pyplot as plt

plt.plot(enroll_by_year['year'], enroll_by_year['enroll_ftug'], marker='o')
plt.title('Total Enrollment in Public Two-Year Colleges (2010–2015)')
plt.xlabel('Year')
plt.ylabel('Total Enrollment')
plt.grid(True)
plt.tight_layout()
plt.show()

#C. Questions: Financial Aid
df_2015 = filtered_df[filtered_df['year'] == 2015]
vt_df = df_2015[df_2015['stabbr'] == 'VT']
ny_df = df_2015[df_2015['stabbr'] == 'NY']
vt_total_grant = vt_df['grant_federal'].sum()
vt_total_students = vt_df['enroll_ftug'].sum()
vt_per_student_grant = vt_total_grant / vt_total_students

ny_total_grant = ny_df['grant_federal'].sum()
ny_total_students = ny_df['enroll_ftug'].sum()
ny_per_student_grant = ny_total_grant / ny_total_students

print(f"Vermont: ${vt_per_student_grant:.2f} per student")
print(f"New York: ${ny_per_student_grant:.2f} per student")

df_2015 = filtered_df[filtered_df['year'] == 2015].copy()
df_2015['grant_per_student'] = df_2015['grant_federal'] / df_2015['enroll_ftug']
state_avg = df_2015.groupby('stabbr')['grant_per_student'].mean()
state_avg.describe()
print(state_avg.describe())
import matplotlib.pyplot as plt
state_avg.sort_values().plot(kind='bar', figsize=(12, 5), title='Avg. Federal Grant per Student by State')
plt.ylabel('Grant per Student ($)')
plt.xlabel('State')
plt.tight_layout()
plt.show()


df_2015['new_grant_federal'] = 1750 * df_2015['enroll_ftug'] + 0.15 * (df_2015['enroll_ftug'] ** 2)


df_2015['new_grant_per_student'] = df_2015['new_grant_federal'] / df_2015['enroll_ftug']


state_avg_new = df_2015.groupby('stabbr')['new_grant_per_student'].mean()

print(state_avg_new.describe())


import matplotlib.pyplot as plt
state_avg_new.sort_values().plot(kind='bar', figsize=(12, 5), title='New Avg. Federal Grant per Student by State')
plt.ylabel('New Grant per Student ($)')
plt.xlabel('State')
plt.tight_layout()
plt.show()

filtered_df.to_excel("filtered_output.xlsx", index=False)





