import pandas as pd
from nba_api.stats.endpoints import leaguedashteamstats
from collections import defaultdict
import time

customHeaders = {
    'Host': 'stats.nba.com',
    'Connection': 'keep-alive',
    'Cache-Control': 'max-age=0',
    'Upgrade-Insecure-Requests': '1',
    'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3',
    'Accept-Encoding': 'gzip, deflate, br',
    'Accept-Language': 'en-US,en;q=0.9',
    'Referer': 'https://stats.nba.com/',
    'x-nba-stats-origin': 'stats',
    'x-nba-stats-token': 'true',
}
# Create a list of the seasons to analyze
seasons = ['2000-01', '2001-02', '2002-03', '2003-04', '2004-05', '2005-06', '2006-07', '2007-08', '2008-09', '2009-10', '2010-11',
           '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18', '2018-19', '2019-20', '2020-21']


# Scrapes stats.nba.com to get season average statistics using nba-api,
# creates Pandas dataframe from statistics, and concatenates each year's dataframe with the prior year
def get_data(season_list):
    time.sleep(1)
    stats = pd.DataFrame()

    for season in season_list:
        # Get Teams Traditional Info
        allTeamsTraditionalInfo = leaguedashteamstats.LeagueDashTeamStats(per_mode_detailed='PerGame',
                                                                          season=season,
                                                                          measure_type_detailed_defense='Base',
                                                                          headers=customHeaders,
                                                                          timeout=120)
        allTeamsTraditionalDict = allTeamsTraditionalInfo.get_normalized_dict()
        allTeamsTraditionalList = allTeamsTraditionalDict['LeagueDashTeamStats']

        # Get Teams Advanced Info
        allTeamsAdvancedInfo = leaguedashteamstats.LeagueDashTeamStats(per_mode_detailed='PerGame',
                                                                       season=season,
                                                                       measure_type_detailed_defense='Advanced',
                                                                       headers=customHeaders,
                                                                       timeout=120)
        allTeamsAdvancedDict = allTeamsAdvancedInfo.get_normalized_dict()
        allTeamsAdvancedList = allTeamsAdvancedDict['LeagueDashTeamStats']

        # Merge the two lists of dicts (Traditional Info and Advanced Info)
        d = defaultdict(dict)
        for l in (allTeamsTraditionalList, allTeamsAdvancedList):
            for elem in l:
                d[elem['TEAM_ID']].update(elem)
        result = d.values()

        # Add the merged dict to the DataFrame
        df = pd.DataFrame(result)

        # Add season information
        df.insert(0, 'SEASON', season)

        # Concat with previous years stats
        stats = pd.concat([stats, df], ignore_index=True)

    return stats


df = get_data(seasons)
print(df)

# Find the columns that do not appear on the NBA official website
indexes_to_drop = []
i = 0
for column in df.columns:
    if "_RANK" in column:
        indexes_to_drop.append(i)

    if column == "TEAM_ID" or column == "CFPARAMS" or column == "CFID" or column == "PACE_PER40":
        indexes_to_drop.append(i)

    if column.startswith("E_"):
        indexes_to_drop.append(i)

    i += 1

# Drop the columns
df.drop(df.columns[indexes_to_drop], axis=1, inplace=True)

# Save dataframe in order to perform R analysis
df.to_csv('tabella.csv', sep=";", index=False)