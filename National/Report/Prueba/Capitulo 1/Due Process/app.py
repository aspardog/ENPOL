from flask import Flask, render_template
import pandas as pd
import json
import re

app = Flask(__name__)

def is_json(myjson):
    try:
        json_object = json.loads(myjson)
    except (ValueError, TypeError):
        return False
    return True


@app.route('/')
def report():
    df = pd.read_excel('/Users/santiagopardo/Library/CloudStorage/OneDrive-WorldJusticeProject/ENPOL/Data/National/Report/Due Process/Data.xlsx', sheet_name='Sheet1')  # Update with the correct sheet name if necessary

    pagenumber = df.set_index('section_id')['pagenumber'].dropna().to_dict()

    parsed_section_data = {}
    for index, row in df.iterrows():
        row_dict = row.to_dict()
        for key, value in row_dict.items():
            if isinstance(value, str) and is_json(value):
                row_dict[key] = json.loads(value)
        parsed_section_data[row['section']] = row_dict

    # Print the data whenever you need to debug
    #print(json.dumps(parsed_section_data, indent=4))

    index = render_template('report.html', section_data=parsed_section_data, pagenumber = pagenumber)
    
    with open('index.html', 'w') as f:         
        f.write(index)          
    return index

if __name__ == '__main__':
    app.run(debug=True)

