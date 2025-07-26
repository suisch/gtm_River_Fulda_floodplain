# Groundwater ecosystem services - quantifying for the River Fulda floodplain
This repository allows to run process models of a three level (carbon in the form of dissolved organic carbon as a breakdown product from detritus;  microorganisms; fauna) food web breaking down carbon and thus providing the ecosystem service of cleaning up the groundwater. 

Extension of this model to other questions and use cases is planned - please stay tuned.

Original meteorological data (produkt_nieder_tag_19490101_20171231_01526.txt, produkt_tu_termin_18850101_20231231_01526.txt) were treated with script Fulda_gw_temperature_extrapolation_fp.R and the product of that, Fulda_daily_temp_joh_long, is used in the actual model.

groundwater_trophic_model.R is the main work horse and calls other functions, variables, and parameters available from, and called from, this repository.

