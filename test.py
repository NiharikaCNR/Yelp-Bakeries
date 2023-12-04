import json

# Open the JSON file for reading
with open('/Users/Patron/Documents/Yelp/yelp_Fall2023/review.json', 'r') as file:
    data = json.load(file)

# Now you can work with the 'data' dictionary

import pandas as pd

# Convert the JSON data to a DataFrame
df = pd.DataFrame(data)

# Now you have a DataFrame 'df' containing your JSON data
print(df.head())

df.to_csv('/Users/Patron/Documents/Yelp/yelp_Fall2023/review.csv', index=False)

