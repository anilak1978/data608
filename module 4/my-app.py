import pandas as pd
import numpy as np
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.graph_objs as go

#referencing the module4 sample code, using soql to query health, steward, spc_common(species)
soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,count(tree_id)' +\
        '&$group=spc_common,boroname,health,steward').replace(' ', '%20')


part_1 = pd.read_json(soql_url)

#part_1.shape() based on Socrata row limit, we can only get 1000 rows at a time. 
# we pull 5000 rows in parts and combine them
part_2 = pd.read_json(soql_url + '&$offset=1000')
part_3 = pd.read_json(soql_url + '&$offset=2000')
part_4 = pd.read_json(soql_url + '&$offset=3000')
part_5 = pd.read_json(soql_url + '&$offset=4000')

tree_counts = pd.concat([part_1,part_2,part_3,part_4,part_5])
#tree_counts.isnull().sum() check for missing values

tree_counts = tree_counts.dropna() # drop the missing values

my_app = dash.Dash() # create the app (review Chris Parmer's video https://www.youtube.com/watch?v=5BAthiN0htc)

my_app.layout = html.Div(children=[
    html.H1(children = 'Health of Trees in NYC'),
    html.P('Two interactive Visualizations That Displays Health Distribution for Boroughs and Stewardship'),
    html.P('Please Select a Borough '),
    dcc.Dropdown(
        id='dropdown-a',
        options=[{'label': i, 'value': i} for i in ['Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island']],
        value='Queens', multi=False, placeholder='Filter By Borough'
    ),
    html.Div(id='output-a'),
    html.P('Number of Stewards Per Tree'),
    html.P("0 = Poor Health; 1 = Fair Health, 2 = Good Health"),
    dcc.Dropdown(
        id='dropdown-b',
        options=[{'label': i, 'value': i} for i in tree_counts['steward'].unique()],
        value='None', multi=False, placeholder='Filter by Borough'
    ),
    html.Div(id='output-b'),
    html.P("0 = Poor Health; 1 = Fair Health, 2 = Good Health")
    ])

@my_app.callback(
        Output(component_id='output-a', component_property='children'),
        [Input(component_id='dropdown-a', component_property='value')]
        )

def boro_graph(input_data):
    df = tree_counts[tree_counts.boroname == input_data]
    
    return dcc.Graph(
            id='Health by Borough',
            figure={
                    'data':[
              {'x':df['health'], 'type': 'histogram','name': 'Health by Borough'}
          ],
          'layout':{
              'title':"Health by Borough"
                  }
          }
              )

@my_app.callback(
        Output(component_id='output-b', component_property='children'),
        [Input(component_id='dropdown-b', component_property='value')]
        )

def steward_graph(input_data):
    df = tree_counts[tree_counts.steward == input_data]
    
    return dcc.Graph(
            id='Health by Steward',
            figure={
                    'data':[
              {'x':df['health'], 'type': 'histogram','name': 'Health by Stewardship'}
          ],
          'layout':{
              'title':"Health by Stewardship"
                  }
          }
              )


if __name__ == '__main__':
    my_app.run_server(debug=False)