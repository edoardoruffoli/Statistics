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

# Scrapes stats.nba.com to get season average statistics using Selenium, creates Pandas dataframe from statistics, and concatenates each year's dataframe with the prior year
def get_data():
    time.sleep(1)

    # Get Teams Traditional Info
    allTeamsTraditionalInfo = leaguedashteamstats.LeagueDashTeamStats(per_mode_detailed='PerGame',
                                                           season='2020-21',
                                                           measure_type_detailed_defense='Base',
                                                           headers=customHeaders,
                                                           timeout=120)
    allTeamsTraditionalDict = allTeamsTraditionalInfo.get_normalized_dict()
    allTeamsTraditionalList = allTeamsTraditionalDict['LeagueDashTeamStats']

    # Get Teams Advanced Info
    allTeamsAdvancedInfo = leaguedashteamstats.LeagueDashTeamStats(per_mode_detailed='PerGame',
                                                           season='2020-21',
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

    return pd.DataFrame(result)


statistics_df = get_data()

# Save dataframe in order to perform R analysis
statistics_df.to_csv('nba_complete_team_statistics.csv', sep=";", index=False)