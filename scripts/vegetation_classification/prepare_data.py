from utils.utils import set_patches

set_patches("../data_source/labels", "../data_source/aligned/2012", "../data/2012_5x5/", (5,5))
#set_patches("../data_source/labels", "../data_source/aligned/2015", "../data/2015/", (1,1))
set_patches("../data_source/labels", "../data_source/aligned/2021", "../data/2021_5x5/", (5,5))

set_patches("../data_source/labels", "../data_source/normalized/2012", "../data/2012_normalized/", (1,1))
set_patches("../data_source/labels", "../data_source/normalized/2015", "../data/2015_normalized/", (1,1))
set_patches("../data_source/labels", "../data_source/normalized/2021", "../data/2021_normalized/", (1,1))

set_patches("../data_source/labels", "../data_source/aligned_composite/2012", "../data/2012_composite_5x5/", (5,5))
# set_patches("../data_source/labels", "../data_source/aligned_composite/2015", "../data/2015/", (1,1))
set_patches("../data_source/labels", "../data_source/aligned_composite/2021", "../data/2021_composite_5x5/", (5,5))