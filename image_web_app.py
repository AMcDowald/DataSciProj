import dash
from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html
import plotly.graph_objs as go
import pandas as pd
import image_web_app_lib as lib
import base64
import io
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
app.scripts.config.serve_locally = True



app.layout = html.Div([
    html.Div([html.H1('Plotly Wholesale')],style={'text-align':'center','color':'green'}),
    html.Div([
        html.Div([
            html.Div(id='output-image-upload'),
            dcc.Upload(
                id='upload-image',
                children=html.Div([
                    'Drag and Drop or ',
                    html.A('Select Files')
                ]),
                style={
                    'width': '100%',
                    'height': '60px',
                    'lineHeight': '60px',
                    'borderWidth': '1px',
                    'borderStyle': 'dashed',
                    'borderRadius': '5px',
                    'textAlign': 'center',
                    'margin': '10px'
                },
                # Allow multiple files to be uploaded
                multiple=True
            )

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

def parse_contents(contents, filename, date):
    return html.Div([
        html.Br(),
        html.H5(filename,style={'text-align':'center'}),
        html.Div([html.Img(src=contents,style={'margin': 'auto','width':'100%','border':'3px solid green','padding-left':'1px','text-align':'center'})],style={'padding':'10px'})
    ])

@app.callback(Output('output-image-upload', 'children'),
              [Input('upload-image', 'contents'),
               Input('upload-image', 'filename'),
               Input('upload-image', 'last_modified')])
def update_output(list_of_contents, list_of_names, list_of_dates):
    if list_of_contents is not None:
        children = [
            parse_contents(c, n, d) for c, n, d in
            zip(list_of_contents, list_of_names, list_of_dates)]
        print(children)
        return children



@app.callback(dash.dependencies.Output('my-graph3', 'figure'),
              [dash.dependencies.Input('upload-image', 'contents')])
def update_graph(contents):
    contents=contents[0]
    content_type, content_string = contents.split(',')
    decoded = base64.b64decode(content_string)
    x_axis=lib.whatNumIsThis(io.BytesIO(decoded))[0]
    y_axis=lib.whatNumIsThis(io.BytesIO(decoded))[1]
    data = [go.Bar(
        x=x_axis,
        y=y_axis,
        marker={'color': '#235ebc'},
        opacity=0.8,
        name="Graph")]
    layout = go.Layout(
        xaxis={'type': 'Mean', 'title': "Image Number Guess"},
        yaxis={'type': 'linear', 'title': "Value",'range':[400,500]},
        margin={'l': 60, 'b': 40, 'r': 10, 't': 10},
        hovermode="False"
    )
    return {'data': data, 'layout': layout}



if __name__ == '__main__':
    app.run_server(host='0.0.0.0', port=9999, debug=True)