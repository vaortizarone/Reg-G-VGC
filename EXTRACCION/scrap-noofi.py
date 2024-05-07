import requests
import pandas as pd
import re
import json


# %%
def obtener_datos_lista(torneo_id):
    url = f"https://play.limitlesstcg.com/api/tournaments/{torneo_id}/standings"
    api_key = "1e7f7868e9d5277dcfcac6c15fbd0ec2"
    headers = {"X-Access-Key": api_key}

    response = requests.get(url, headers=headers)
    json_data = response.json()

    datos_player = pd.DataFrame(json_data)
    part1 = datos_player[['player', 'decklist', 'record']]

    a = pd.concat([part1, pd.json_normalize(part1['record'])[['wins', 'losses']]], axis=1).drop('record', axis=1)

    return a


# %%

url = "https://play.limitlesstcg.com/api/tournaments"
api_key = "1e7f7868e9d5277dcfcac6c15fbd0ec2"

# Incluye la clave de acceso como encabezado HTTP
headers = {
    "X-Access-Key": api_key
}

# Parámetros de consulta opcionales (por ejemplo, limit para especificar la cantidad de resultados)
params = {
    "limit": 50,
    "game": "VGC"
}

# Realizar una solicitud GET a la API con encabezados y parámetros de consulta
response = requests.get(url, headers=headers, params=params)

data = response.json()

tours = pd.DataFrame(data)[pd.DataFrame(data)['players'] > 30][['id', 'name']]
tours


# %%

def combinar_datos_torneos(tours):
    # Crear un DataFrame vacío para almacenar los resultados
    resultados = pd.DataFrame()
    for index, row in tours.iterrows():
        torneo_id = row['id']
        nombre_torneo = row['name']

        # Obtener datos de la lista y las rondas para el torneo actual
        df_lista = obtener_datos_lista(torneo_id)

        # Agregar las columnas adicionales
        df_lista.insert(0, 'Torneo', nombre_torneo)
        df_lista.insert(1, 'Tipo_Torneo', 'No-Oficial')
        df_lista.rename(columns={'wins': 'Wins', 'losses': 'Losses', 'decklist': 'Equipo', 'player': 'Player'},
                        inplace=True)
        df_lista.insert(3, 'Rondas', f'https://play.limitlesstcg.com/api/tournaments/{torneo_id}/pairings')
        # Combina los DataFrames y agrega los resultados al DataFrame principal
        resultados = pd.concat([resultados, df_lista], ignore_index=True)

    return resultados


fact_run_noofi = combinar_datos_torneos(tours)
fact_run_noofi = pd.DataFrame(fact_run_noofi)

#%%
fact_run_noofi = fact_run_noofi.dropna(subset=['Equipo'])
fact_run_noofi.insert(0, 'ID', range(20001, 20001 + len(fact_run_noofi)))
fact_run_noofi.reset_index(drop=True, inplace=True)


#%%
res = pd.DataFrame()
for i in range(0, len(fact_run_noofi)):
    d = pd.DataFrame(fact_run_noofi['Equipo'][i])
    d['ID'] = fact_run_noofi['ID'][i]
    d = d[['ID', 'name', 'item', 'ability', 'tera', 'attacks']]
    res = pd.concat([res, d], ignore_index=False)
#%%
res[['Mov1', 'Mov2', 'Mov3', 'Mov4']] = res['attacks'].apply(pd.Series)
res = res[['ID', 'name', 'item', 'ability', 'tera', 'Mov1', 'Mov2', 'Mov3', 'Mov4']]
team_ots_noofi = res.rename(columns={'name': 'Pokemon', 'item': 'Objeto', 'ability': 'Habilidad', 'tera': 'Teratipo'})

# %%
api_key = "1e7f7868e9d5277dcfcac6c15fbd0ec2"
headers = {"X-Access-Key": api_key}
uni_torn = fact_run_noofi['Rondas'].unique()

import requests
import pandas as pd


def rondas_torneo(url_torneo):
    try:
        response = requests.get(url_torneo, headers=headers)
        json_data = response.json()
        datos_rondas = pd.DataFrame(json_data)

        # Intentar seleccionar las columnas necesarias
        df2 = datos_rondas[['round', 'winner', 'player1', 'player2']]

        # Intentar obtener la columna 'match'; si no está presente, asignar None a esa columna
        try:
            df2['match'] = datos_rondas['match']
        except KeyError:
            df2['match'] = None

        # Si 'match' no está presente, asignar 'round' a esos valores
        df2.loc[~df2['match'].isna(), 'round'] = df2['match']

        return df2

    except Exception as e:
        print(f"Error al procesar la URL {url_torneo}: {str(e)}")
        return None


#%%
dicc_torneos = {}

for i in range(len(uni_torn)):
    df = rondas_torneo(uni_torn[i])
    dicc_torneos[uni_torn[i]] = df

# %%
rounds_noofi = pd.DataFrame()

def process_round(i):
    url_torneo = fact_run_noofi['Rondas'][i]
    datos_rondas = dicc_torneos.get(url_torneo)
    df2 = datos_rondas[['round', 'winner', 'player1', 'player2', 'match']]
    df2.loc[~df2['match'].isna(), 'round'] = df2['match']
    df3 = df2[(df2['player1'] == fact_run_noofi['Player'][i]) | (df2['player2'] == fact_run_noofi['Player'][i])]
    df3['Rival'] = df3.apply(lambda row: row['player1'] if row['player2'] == fact_run_noofi['Player'][i] else row['player2'],
                             axis=1)
    df3['Resultado'] = df3.apply(lambda row: 'Win' if row['winner'] == fact_run_noofi['Player'][i] else 'Loss', axis=1)
    df3['ID'] = fact_run_noofi['ID'][i]
    df4 = df3[['ID', 'round', 'Rival', 'Resultado']]
    df4.rename(columns={'round': 'Ronda'}, inplace=True)
    df4.loc[df4['Rival'].isna(), ['Rival', 'Resultado', 'Ronda']] = 'DROP'
    return df4


for i in range(0, len(fact_run_noofi)):
    df = process_round(i)
    rounds_noofi = pd.concat([rounds_noofi, df], ignore_index=True)
# %%

fact_run_noofi.to_csv('fact_run_noofi.csv', index=False)
team_ots_noofi.to_csv('team_list_noofi.csv', index=False)
rounds_noofi.to_csv('round_list_noofi.csv', index=False)