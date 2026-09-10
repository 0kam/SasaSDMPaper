"""Build the org_/sim_ control-point table procrustes_tps_align.R expects.
org = GCP position observed in the photograph (gcp.csv u,v)
sim = the same GCP projected with the recovered camera model (alproj @ c8a3e3d)."""
import json, importlib.util, pandas as pd, numpy as np, sys
D='/Users/okamoto/NIES/SasaSDMPaper/data_from_server/ortho/data/'
ALP=sys.argv[1] if len(sys.argv)>1 else '/private/tmp/claude-501/-Users-okamoto-NIES-SasaSDMPaper--claude-worktrees-review-summary-ece-ddc69c/98ac7ecf-eafb-49c6-aa2d-9aab2998c13e/scratchpad/src/alproj_2022/src/alproj/optimize.py'
s=importlib.util.spec_from_file_location('op',ALP); op=importlib.util.module_from_spec(s); s.loader.exec_module(op)
P={k:v for k,v in json.load(open(D+'params_optim.json')).items() if k!='error'}
g=pd.read_csv(D+'gcp.csv',index_col=0)
pr=op.project(g[['x','y','z']],P)
pd.DataFrame({'org_x':g.u.values,'org_y':g.v.values,'sim_x':pr.u.values,'sim_y':pr.v.values}).to_csv(
    '/Users/okamoto/NIES/SasaSDMPaper/.claude/worktrees/review-summary-ece-ddc69c/review/restore/georect/out/tps_control_points.csv',index=False)
print('wrote', len(g), 'pairs')
