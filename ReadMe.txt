Set up your data folder to create experiments

1. First, mount your data folder in Windows command prompt:
subst D: C:\Users\npoh\Documents\myProjects\Biogen_Tecfidera

If you run this on the server, use
subst D: \\kgxsapp100\F\Projects\Biogen_Tecfidera

TIPS: To unmount, use subst D: \d

In this folder, make sure that 'Data' and 'Results' folder are accessible.

2. Setup your local working direcotry (optional)

subst L: C:\Users\npoh\Documents\Git\projects\Biogen_Tecf2017_round5

On my server, I would use 
subst L: F:\Norman\Biogen_Tecf2017_round5



pre_comdx_lst2_diff

xgb_universe_basic/  xgb_universe_basic_diff/  xgb_universe_basic_hcp/  xgb_universe_basic_hcp_diff/

