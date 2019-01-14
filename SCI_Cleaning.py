
# coding: utf-8

# In[110]:


import pandas as pd


# In[111]:


Periodicity = pd.read_csv('/Periodicity_Timeliness.csv')
Source = pd.read_csv('/Source_Data.csv')
Methodology = pd.read_csv('/Statistical_Methodology.csv')

#Same file as Periodicity - will replace value with Average Score
Average = pd.read_csv('/Periodicity_Timeliness.csv')


# In[112]:


Periodicity = Periodicity.drop(columns=['Unnamed: 0'])
Source = Source.drop(columns=['Unnamed: 0'])
Methodology = Methodology.drop(columns=['Unnamed: 0'])
Average = Average.drop(columns=['Unnamed: 0'])


# In[113]:


Methodology.head(1)


# In[114]:


#Melting so Year is presented as one columns
Periodicity = pd.melt(Periodicity, id_vars=['NAME','Country Code','Indicator Name','Indicator Code'])
Source = pd.melt(Source, id_vars=['NAME','Country Code','Indicator Name','Indicator Code'])
Methodology = pd.melt(Methodology, id_vars=['NAME','Country Code','Indicator Name','Indicator Code'])
Average = pd.melt(Average, id_vars=['NAME','Country Code','Indicator Name','Indicator Code'])


# ### 1. Dataframe for Leaflet Choropleth Map (SCI_Melted.csv)

# In[116]:


Average['Indicator Name'] = 'Average'

#will not use this column
Average['Indicator Code'] = 'Average'


# In[117]:


#Creating Average Score for Average DF
Average['value'] = round((Periodicity['value'] + Methodology['value'] + Source['value'])/3)


# In[118]:


#concactenated into one dataframe
SCI = pd.concat([Periodicity,Source,Methodology, Average], ignore_index=True)


# In[119]:


#round value column
SCI['Value'] = SCI['value'].round(2)


# In[120]:


#Shortening Indicator Names

SCI['Indicator Name'] = SCI['Indicator Name'].map({'Periodicity and timeliness assessment of statistical capacity (scale 0 - 100)': 'Periodicity',
                               'Methodology assessment of statistical capacity (scale 0 - 100)': 'Methodology',
                              'Source data assessment of statistical capacity (scale 0 - 100)': 'Source'})


# In[121]:


SCI=SCI.rename(columns={'variable':'Year','Value':'Percentage'})
SCI =SCI.drop(columns=['value'])


# In[122]:


SCI['Indicator Name'] = SCI['Indicator Name'].fillna('Average')


# In[123]:


SCI = SCI.rename(columns={'Country Code':'Country_code','Indicator Name':'Indicator_Name','Indicator Code':'Indicator_Code'})


# In[ ]:


SCI.to_csv('SCI_Melted.csv', sep=',')


# ### Dataframe for Table (SCI_Melted_Table.csv)

# In[125]:


Periodicity['Source'] = Source['value']


# In[126]:


Periodicity['Methodology'] = Methodology['value']

Periodicity = Periodicity.rename(columns={'value':'Periodicity','variable':'Year'})

Periodicity['Average'] = (Periodicity['Periodicity']+Periodicity['Source']+Periodicity['Methodology'])/3

Periodicity=Periodicity.drop(columns=['Indicator Name','Indicator Code'])

Periodicity[['Periodicity','Average']] = Periodicity[['Periodicity','Average']].round()


Periodicity[['Periodicity','Average']] = Periodicity[['Periodicity','Average']].astype(int)


# In[127]:


Periodicity.to_csv('SCI_Melted_Table.csv', sep=',')

