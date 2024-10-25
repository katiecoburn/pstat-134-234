# Data contains information about books from the British library
import pandas as pd
import numpy as np

pd.options.display.max_columns = 10
pd.options.display.max_rows = 20

data = pd.read_csv('lecture4-data-cleaning-example/BL-Flickr-Images-Book.csv', 
  delimiter=',', encoding='utf-8')
print(data.head(10))

# Some columns have information useful to the library but not very
# descriptive of the books themselves. We can drop those columns:

to_drop = ['Edition Statement', 'Corporate Author',
  'Corporate Contributors', 'Former owner', 'Engraver',
  'Contributors', 'Issuance type', 'Shelfmarks']

data.drop(to_drop, inplace=True, axis=1)
# That means we want the changes to be made directly in our object
# and it should look for the values to be dropped in the columns

data.head()

# The index is currently 0, 1, 2, etc., but it might be helpful to
# use the book's identifier as its index, as the identifier is
# a unique id:
data['Identifier'].is_unique

data = data.set_index('Identifier')
data.head()

data.loc[206]

# Let's clean date and place of publication
data.dtypes.value_counts()
# everything is an object (string)
# but it might make more sense for date to be a number
data.loc[1905:, 'Date of Publication'].head(20)
# A book should only have one date of publication, so we need to:
# remove extra dates in square brackets, if present:
# convert date ranges to "start date":
# completely remove uncertain dates like [1897?]
# convert string nan to NaN
# We can write a single regular expression to extract year:

regex = r'^(\d{4})'
# find any four digits at the beginning of a string: \d any digit,
# {4} four times; ^ = start of string and () = capturing group

extr = data['Date of Publication'].str.extract(r'^(\d{4})', expand=False)
extr.head()
data['Date of Publication'] = pd.to_numeric(extr)
data['Date of Publication'].dtype
data['Date of Publication'].isnull().sum() / len(data)
# above gets the percentage of missing values

# Now we'll clean place of publication
data['Place of Publication'].head(10)
# for some books, the place has unnecessary information
# this is only for rows where they were published in London or Oxford
# we can look at two specific entries:
data.loc[4157862]
data.loc[4159587]
# same place, but one has hyphens and the other doesn't

pub = data['Place of Publication']
london = pub.str.contains('London')
london[:5]
oxford = pub.str.contains('Oxford')

# If the string contains London we replace with London, same for Oxford
# and we replace dashes with spaces
data['Place of Publication'] = np.where(london, 'London',
  np.where(oxford, 'Oxford',
    pub.str.replace('-', ' ')))
    
data['Place of Publication'].head()
