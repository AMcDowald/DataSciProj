import dash
from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html
import plotly.graph_objs as go
import pandas as pd


#CUSTOM INPUT
path = '/home/amcdowald/Downloads/Wholesale customers data.csv'


#INIT_xFUNC
def m_dataframe():
    MASTER = pd.DataFrame.from_csv(path)
    return MASTER

#INPUT DATA
MASTER_DATAFRAME=m_dataframe()
list_of_columns_excluding_region=MASTER_DATAFRAME.columns.get_values()[1:]

# #START APP
app = dash.Dash()
app.title='HDI App'

app.layout = html.Div([
    html.Div([html.H1('Plotly Wholesale')],style={'text-align':'center','color':'green'}),
    html.Div([
        html.Div([
            html.P('Menu'),
            dcc.Dropdown(
                id='my-dropdown',
                options=[ {'label': '%s'%x, 'value': '%s'%x} for x in list_of_columns_excluding_region],
                multi=True,
                value=list_of_columns_excluding_region[1:3]
            ),html.P('Channel'),
            dcc.RadioItems(
                id='my-channel_checklist',
                options=[
                    {'label': '%s'%x, 'value': '%s'%x} for x in sorted(MASTER_DATAFRAME.index.unique())
                ],
                value='1'
            ),html.P('Region'),
            dcc.RadioItems(
                id='my-region_checklist',
                options=[
                    {'label': '%s'%x, 'value': '%s'%x} for x in sorted(MASTER_DATAFRAME.Region.unique())
                ],
                value='1'
            ),
            html.P('Stat'),
            dcc.RadioItems(
                id='stat',
                options=[
                    {'label': '%s' % x, 'value': '%s' % x} for x in ['mean','sum','max']
                ],
                value='mean'
            ),
        ],
        style={'float': 'left','box-sizing':'border-box', 'max-height':'20px','flex': '1','width':'20%'}),
        html.Div([
            html.Div([
                html.H1('Line Graph',style={'text-align':'center'}),
                dcc.Graph(id='my-graph3')
            ],
            style={'margin':'15px','flex':'1','padding':'10px'})
        ],style={'width':'80%','overflow':'auto'})#,style={'float': 'left','box-sizing':'border-box','display': 'flex','width':'50%'})
    ],style={'margin': 'auto','width':'50%','height':'600px','border':'13px solid green','padding':'10px','display': 'flex'})

])

@app.callback(
    dash.dependencies.Output('my-graph3', 'figure'),
    [dash.dependencies.Input('my-dropdown', 'value'),
     dash.dependencies.Input('my-channel_checklist', 'value'),
     dash.dependencies.Input('my-region_checklist', 'value'),
     dash.dependencies.Input('stat', 'value')])
def update_graph(selected_dropdown_value,channels,regions,stat):
    if stat=='mean':
        new_df=MASTER_DATAFRAME.groupby(['Region', 'Channel'])[selected_dropdown_value].mean()
    elif stat=='sum':
        new_df = MASTER_DATAFRAME.groupby(['Region', 'Channel'])[selected_dropdown_value].sum()
    elif stat=='max':
        new_df = MASTER_DATAFRAME.groupby(['Region', 'Channel'])[selected_dropdown_value].max()
    print(selected_dropdown_value)
    return {
            'data' : [go.Scatter(x=new_df.columns.get_values(), y=[x for x in new_df.loc[(int(regions),int(channels)),:]],
            mode='lines+markers')
        ], 'layout' : {'title' : 'Mean Line graph of\n %s'% ', '.join([str(x) for x in selected_dropdown_value])}
       }


if __name__ == '__main__':
    app.run_server(host='0.0.0.0', port=9999, debug=True)


#https://github.com/plotly/dash-core-components/pull/73