# CSC8634_CC_Project

The ipynb source file for the report is located in 'CSC8635_ML_Project/reports' folder. The run the analysis from this file, the notes for running the analysis from script (below) should be observed, omitting the individual .py script runs.

To load project from scripts:

1. Create and activate keras-friendly py env (py3.6 etc)

2. Download data file from: https://www.kaggle.com/kmader/skin-cancer-mnist-ham10000. Unzip and put in 'CSC8635_ML_Project/data' folder

3. Open 'load_transform.py' from 'CSC8635_ML_Project/munge'

4. Set 'project_dir' to working to CSC8635_ML_Project directory path

5. Set 'test_n' to arbitrary test number for new tests. To load models etc for existing test, enter '1822-05' or '1823-01' (1822-05 forms the basis of the report. 1823-01 is an interesting model for which validation metrics peformed significantly better than training metrics. Unfortunatley there wasn't enough time to examine this in the report)

6. Run 'load_transform.py'

7. Run 'CNN.py' from 'CSC8635_ML_Project/src'

8. Run 'Test_Analysis.py' from 'CSC8635_ML_Project/src'

9. Run 'ROC_Analysis.py'from 'CSC8635_ML_Project/src'

NB(1): In addition to the saved hdf5 models in 'CSC8635_ML_Project/tests', there are multiple cached objects stored in 'CSC8635_ML_Project/cache'. These objects are as follows:

* meta (dataframe of complete dataset including resized images as arrays and labels)
* hist_df (dataframe of model training history)
* results_df (dataframe of model test/validation accuracy and loss performance)
* y_score (numpy array of model prediction for ROC)

These objects have been cached because of heavy computation requirements and/or compatibility with machines incapable of running keras. The scripts can be run as is to view the saved models/analysis. For new models, the **cached objects must be refreshed**. This can be accomplished by uncommenting the respective sections of the scripts. 

NB(2): Additional files of interest:

* log.tx (git commit log)
* CSC8635_ML_Project/docs/references (contains reference literature for report)
* CNN_Search.py (automated hyperparameter search using Talos)
* CNN_TL.py (model built using transfer learning from ImageNet InceptionV3)
