Dokument for å beskrive hvordan filnavn skal være for de forskjellige filene. 


Input 

Filnavn						Type  		Mappe																							Forklaring 

forecast_point_GW + "X"		xlsx		master_thesis\input\dynamic_data\forecasting_method\ + NAVN PÅ FORECASTING METODE + \			fil som inneholder forecasted poeng hvor X = hvilken gameweek gjeldende ergo 0, 1, 2, ... 37 
player_cost_GW + "X"		xlsx		master_thesis\input\dynamic_data\cost\															fil som inneholder reell cost hvor X = hvilken gameweek gjeldende ergo 0, 1, 2, ... 37 
		
player_club 				xlsx		master_thesis\input\static_data\																fil som inneholder hvilken klubb hver spiller er i. 
player_list 				xlsx		master_thesis\input\static_data\ 																fil som inneholder indeks til hver spiller. inneholder også fornavn, etternavn og posisjon. 
EFPL_data					txt			master_thesis\input\static_data\ 																fil som innehilder hvor mange players, keepers, defenders, midfielders og attackers i spillet.


Output 

Filnavn 										Type		Mappe 																						Forklaring
		
NAVN PÅ FORECASTING METODE + _output_GW + "X" 	txt			master_thesis\input\dynamic_data\forecasting_method\ + NAVN PÅ FORECASTING METODE + \		Fil som inneholder output fra en kjøring i Mosel, hvor X = hvilken gameweek gjeldende ergo 0, 1, 2, ... 37


Notes: 
- N/A = -10000 forecasted point. 