p_load(writexl)

# LANCIARE CIASCUNA SEZIONE DOPO AVER LANCIATO IL _DA CORRISPONDENTE
# RUN EACH SECTION AFTER SOURCING THE CORRESPONDING _DA FILE


# Abitare
write_xlsx(abPlotZon, 'pressTables//abPlotZon.xlsx') # Indice di soddisfazione con la condizione abitativa - Scomposizione per tipo di insediamento
write_xlsx(abPlotDem, 'pressTables//abPlotDem.xlsx') # Indice di soddisfazione con la condizione abitativa - Scomposizione per tipo di insediamento (Per occupazione)

# Impegnarsi
write_xlsx(svil, 'pressTables//svilInd.xlsx') # Servizi, rappresentanza e cultura al centro\ndelle esigenze dei nostri iscritti
write_xlsx(att, 'pressTables//attInd.xlsx') # Welfare, ambiente e diritti civili sono i temi\nche stanno più a cuore ai nostri iscritti

# Lavorare
write_xlsx(risc, 'pressTables//riscLav.xlsx') # Rischi percepiti dai nostri iscritti
Lavorare |> 
  count(aspiraz) |> 
  arrange(n) |> 
  write_xlsx('pressTables//aspiraz.xlsx') # Aspirazioni per il futuro

# Studiare
write_xlsx(studPlotEdu,'pressTables//studPlotEdu.xlsx')  # Indice di soddisfazione con il percorso di studi

# Restare
Restare |> 
  group_by(rapp, occ) |> 
  count() |> 
  write_xlsx('pressTables//attRest.xlsx') # Attitudine al restare
write_xlsx(rest, 'pressTables//motRest.xlsx') # Motivi per restare
write_xlsx(lasc, 'pressTables//motLasc.xlsx') # Motivi per lasciare
