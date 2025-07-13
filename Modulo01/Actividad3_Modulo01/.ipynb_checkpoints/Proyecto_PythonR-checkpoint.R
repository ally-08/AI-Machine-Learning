library(reticulate)
use_python("C:/Users/lesly/AppData/Local/Programs/Python/Python313/python.exe", required=TRUE)

py_run_string("
import pandas as pd

df = pd.read_excel('Enhanced_pizza.xlsx')
df.columns = df.columns.str.strip()

df = df.drop_duplicates()
df['Delivery Duration (min)'] = df['Delivery Duration (min)'].fillna(df['Delivery Duration (min)'].median())
df['Order Time'] = pd.to_datetime(df['Order Time'])
df['Delivery Time'] = pd.to_datetime(df['Delivery Time'])

df['Order_Date'] = df['Order Time'].dt.date
df['Order Time'] = df['Order Time'].dt.strftime('%H:%M:%S')
df['Delivery Time'] = df['Delivery Time'].dt.strftime('%Y-%m-%d')

columnas_a_redondear = [
    'Delivery Duration (min)', 
    'Distance (km)', 
    'Delivery Efficiency (min/km)', 
    'Topping Density',
    'Estimated Duration (min)', 
    'Delay (min)', 
    'Restaurant Avg Time'
]

df[columnas_a_redondear] = df[columnas_a_redondear].round(2)
")

# Traer el dataframe a R
df_r <- py$df

# Mostrarlo con DT para scroll y formato
library(DT)
datatable(df_r, options = list(scrollX = TRUE, scrollY = '500px', paging = FALSE))
