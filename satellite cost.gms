*J_ componenti costo satelliti
*D_ funzioni di sconto prezzo satellite
*consideriamo le regioni su costo satellite? io direi di no
*inserire equazione in riga 320 qualcosa
Variables 
YJ(t)       'total global cost'
YJJ (t,n)   'total cost per region'
;
PARAMETERS
Weight                  'satellite medium weight'            /50/ 'kg'
J_launch(t)             'launch cost'                   /
J_construction(t)       'assembly cost'           /
J_bus(t)                'satellite bus cost'       /
J_mirror(t)             'satellite mirror cost'
J_fuel                  'fuel for positioning from LEO to L1 cost, and for maintaining position'/220/ '$/kg'
J_o&m                   'operation and maintenance cost'/1.1/
J_development           'development cost'              /1.0001/  'r&d 1/10000 dei costi totali, forse un po alto ma va svbiluppato anche il sistema di controllo'
D_launch                'discount rate launch'          /0.95/
D_construction          'discount rate assembly'        /0.95/
D_bus                   'discount rate satellite bus'   /0.95/
D_mirror                'discount rate satellite mirror' /0.95/
D_fuel                  'discount rate satellite fuel'  /0.95/
Cost_YoY                'cost year over year'
;
J_launch(tfirst)=5000  ' $/kg'
J_bus(tfirst)=4000000
J_mirror(tfirst)=70000  'supponendo 1/100 del peso in oro e assumendo solo quello come costo'
equation
eq_satellite_cost(t)
eq_J_construction(t)
eq_J_bus(t)
eq_J_mirror(t)
eq_YJ_perregion(t,n)$(reg(n))
eq_cost_YoY(t,n)$(reg(n))
;


eq_J_launch(t)..        J_launch(t+1)         =E=     J_launch(t)*D_launch;
eq_J_construction(t)..  J_construction(t)     =E=     (J_bus(t)+J_mirror(t))*J_o&m;
eq_J_bus(t)..           J_bus(t+1)            =E=     J_bus(t)*D_bus;
eq_J_mirror(t)..        J_bus(t+1)            =E=     J_mirror(t)*D_mirror;
eq_J_maintenance(t)..   J_maintenance(t+1)    =E=     J_maintenance(t)*D_maintenance;
eq_satellite_cost(t)..  YJ(t)                 =E= mass(t)*Weight*(
                                                                J_launch (t)+
                                                                J_construction(t)+
                                                                J_fuel(t))*
                                                                J_development;
eq_YJ_perregion(t,n)$(reg(n))..    YJJ(t,n)   =E=     YJ(t)*nweights(t,n)
'pop(t,n)/sum(nn$reg(nn),pop(t,nn))'
eq_cost_YoY(t,n)$(reg(n))..        Cost_YoY(t+1,n)   =E=     YJJ(t+1,n)-YJJ(t,n)

