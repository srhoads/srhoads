##******************************************************************************
######--------ways of importing py files/reloading them #### import functions.py file to get ur defs (works for reimporting too)--------######
# functions = (open('functions.py').read()); exec(functions.read())
## or...
# exec(open('functions.py').read())
###----or from raw github url:--------------------------------------------------------------
# import urllib.request; exec(urllib.request.urlopen('https://raw.githubusercontent.com/srhoads/srhoads/master/python/functions.py').read())
##******************************************************************************
try: # Set `IMPORT_MODULES=True` if you want to load a bunch of libraries relevant to the functions in this file... otherwise, it defaults to `IMPORT_MODULES=False`
    IMPORT_MODULES
except:
    IMPORT_MODULES = False

import os # auto-importing os bc it's so ubiquitous
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

def pipInstall(pkg):
  import os
  os.system("pip3 install " + pkg)

def pipUpgrade(pkg):
  import os
  os.system("pip3 install --upgrade " + pkg)

def pkg(package, via = "pip"):
    from pip._internal import main
    from os import system
    try: return __import__(package)
    except ImportError: 
        try:
            if via == "pip":
                try:
                    main(['install', '--user', package]) 
                except EnvironmentError:
                    system('python3 -m pip install ' + package + " / y")
            if via == "conda":
                system('conda install ' + package)
        except:
            print("Sorry kid... can't install this guy")
            
def pkg_v1(module='uszipcode', submodule=None):
    justModule = module
    if submodule is not None:
        module = module + '.' + submodule
    try: 
        import importlib 
        import re
    except Exception as e:
        print(e)
        import os
        os.system(str('pip install ' + 'importlib'))
        os.system(str('pip install ' + 're'))
        import importlib
    try: # loader = pkgutil.find_loader(module) # mod = loader.load_module(module) # importlib.abc.load_module(module) # importlib.util.module_for_loader(module)
        mod = importlib.import_module(module)
        globals().update(mod.__dict__)
    except Exception as e:
        print(e)
        import os
        try:
            os.system(str('pip install ' + justModule))
        except Exception as e:
            print(e)
            justModule = re.sub('_', '-', justModule)
            os.system(str('pip install ' + justModule))
        mod = importlib.import_module(module)
        globals().update(mod.__dict__) # except ModuleNotFoundError:#     import os#     mod = importlib.import_module(module)#     globals().update(mod.__dict__)      

def pkg2(module, submodule = None, verbose=False):
    if submodule is not None:
        import_str = "from {0} import {1}".format(module,', '.join([submodule]))
    else:
        import_str = "import {0}".format(module)
    try: # https://stackoverflow.com/questions/8718885/import-module-from-string-variable
        exec(import_str)
        submodule = '' if submodule is None else submodule
        print(module,' ', submodule, ' imported!') if verbose else None
    except Exception as e:
        print(e); import os
        try:
            os.system(str('pip install ' + module))
        except Exception:
            import re
            module = re.sub('_', '-', module)
            os.system(str('pip install ' + module))
        exec(import_str)
        if submodule is None: submodule =         print(module,' ', submodule, ' imported!')
    if type(import_str) is not str:
        return(import_str[0])
    else:
        return(import_str)

def pkgs2(MODULES = [
    ['itertools','combinations'],
    ]):
    for ITEM in MODULES:
        import_str = "from {0} import {1}".format(ITEM[0],', '.join(str(i) for i in ITEM[1:]))
        try:
            exec(import_str)
            print(ITEM[0],' ', ITEM[1:], ' imported!')
        except Exception as e:
            print(e); import os
            try:
                os.system(str('pip install ' + ITEM[0]))
            except Exception as e:
                print(e); import re
                module = re.sub('_', '-', ITEM[0])
                os.system(str('pip install ' + ITEM[0]))
            exec(import_str)
            print(ITEM[0],' ', ITEM[1:], ' imported!')
    import_strs = []
    for ITEM in MODULES:
        import_str = "from {0} import {1}".format(ITEM[0],', '.join(str(i) for i in ITEM[1:]))
        import_strs.append(import_str)
    return import_strs

def load_pkgs2(MODULES = [
    ['itertools','combinations'],
    ]):
    import_strs = []
    for ITEM in MODULES:
        import_str = "from {0} import {1}".format(ITEM[0],', '.join(str(i) for i in ITEM[1:]))
        exec(import_str)
        print(ITEM[0],' ', ITEM[1:], ' imported!')
        import_strs.append(import_str)
    return import_strs

def source_global():
    try: exec(open('global.py').read()); print("Try 1 success")
    except FileNotFoundError: 
        try: exec(open('/Users/srhoads/GitHub/aapdata/global.py').read()); print("Try 2 success")
        except FileNotFoundError:
            try: glob=open('/home/srhoads/GitHub/aapdata/global.py'); exec(glob.read()); print("Try 3 success")
            except FileNotFoundError:
                    try: glob=open('../../aapdata/global.py'); exec(glob.read()); print("Try 4 success")
                    except: print("Can't load file...")
                        
def source(file):
    try: exec(open(file).read()); print("Try 1 success")
    except FileNotFoundError: 
        try: exec(open('/Users/srhoads/GitHub/aapdata/' + file + '.py').read()); print("Try 2 success")
        except FileNotFoundError:
            try: glob=open('/home/srhoads/GitHub/aapdata/' + file + '.py'); exec(glob.read()); print("Try 3 success")
            except FileNotFoundError:
                try: glob=open('../../aapdata/' + file + '.py'); exec(glob.read()); print("Try 4 success")
                except FileNotFoundError:
                    try: glob=open('../' + file + '.py'); exec(glob.read()); print("Try 4 success")
                    except FileNotFoundError:
                        try: glob=open('../../' + file + '.py'); exec(glob.read()); print("Try 4 success")
                        except: print("Can't load file...")
                
#================================================================================================================= 
if IMPORT_MODULES:
    # IMPORTS!
    try:
        pkgImportStrings = pkgs2(MODULES = [
            ['matplotlib', '*'],
            ['sklearn', '*'],
            ['tpot', '*'],
            ['imblearn', '*'], # ['', '*']
        ])
        [exec(pkgImportString) for pkgImportString in pkgImportStrings]
    except Exception as e:
        print(e)
    try:
        import feather
    except Exception as e:
        print('WILL INSTALL: feather-format'); import os; os.system("pip3 install feather-format")
        try:
            import feather
        except Exception as e:
            print('ERROR (functions.py):', e)
    try:
        pkg('sklearn'); pkg('tpot'); pkg('imblearn'); pkg('mglearn'); pkg('patsy'); pkg('pandas'); pkg('pickle');
        # import matplotlib.pyplot as plt
        from sklearn.ensemble import RandomForestClassifier, ExtraTreesClassifier, AdaBoostClassifier, BaggingClassifier
        from sklearn.naive_bayes import GaussianNB, MultinomialNB
        from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis
        from sklearn.linear_model import LogisticRegression
        from sklearn.tree import DecisionTreeClassifier
        from sklearn.neighbors import KNeighborsClassifier
        from sklearn.svm import SVC
        from sklearn.multiclass import OneVsRestClassifier, OneVsOneClassifier
        from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer, TfidfTransformer
        from sklearn.metrics import confusion_matrix, accuracy_score
        from sklearn.feature_selection import VarianceThreshold
        from sklearn.pipeline import Pipeline
        from sklearn.linear_model import LinearRegression
        from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV, RandomizedSearchCV   # from sklearn import model_selection
        from sklearn.datasets import make_classification, load_digits
        from sklearn.decomposition import LatentDirichletAllocation
        from tpot import TPOTClassifier
        from imblearn.ensemble import EasyEnsembleClassifier
        import numpy as np
        import pandas as pd
        import patsy as ps
        import re, time
        from time import time
        import pickle, mglearn
        from scipy.stats import randint as sp_randint
    except Exception as e:
        print('ERROR (functions.py): IMPORT_MODULES is True but ERRORS importing:', e)
        try: # pkgs2([['tpot'], ['imblearn'], ['mglearn'], ['patsy'], ['pandas'], ['pickle']])
            pkg('tpot'); pkg('imblearn'); pkg('mglearn'); pkg('patsy'); pkg('pandas'); pkg('pickle');
        except Exception as e:
            print('ERROR (functions.py):', e)
    try:
        from xgboost import XGBClassifier
    except Exception as e:
        print('ERROR (functions.py):', e); os.system("brew install libomp"); os.system("pip3 install xgboost")
        try:
            from xgboost import XGBClassifier
        except Exception as e:
            print('ERROR (functions.py):', "xgboost.XGBClassifier not loading correctly, sadly :( ...", e)
    try:
        pkgImportStrings = pkgs2(MODULES = [
        ['numpy', '*'],
        ['pandas', '*'],
        ['patsy', '*'],
        ['re', '*'],
        ['time', '*'],
        ['pickle', '*'],
        ['mglearn', '*'],
        ['scipy', '*'],
        ['os', '*'],
        ['pickle', '*'],
        ])
        [exec(pkgImportString) for pkgImportString in pkgImportStrings]
    except Exception as e:
        print('ERROR (functions.py):', e)

    try:
        import psycopg2
    except Exception as e:
        print("ERROR (functions.py): can't import psycopg2", e) # import os; os.system('pip install psycopg2')
        import os; os.system('pip install psycopg2-binary')
        
    try:
        import pickle, feather
    except Exception as e:
        print("ERROR (functions.py): can't import", e) 
    # WRITING FILES #---------------------------------------------------
    # import pickle
    #pickle.dump(model, open('/Users/srhoads/Documents/GitHub/name_race_gender/model/gendermodel_97.6%.sav', 'wb'))
    # import feather
    #feather.write_dataframe(pd.DataFrame(X), "/Users/srhoads/Documents/GitHub/name_race_gender/model/X_gendermodel_97.6%.feather")
    #feather.write_dataframe(pd.DataFrame(y), "/Users/srhoads/Documents/GitHub/name_race_gender/model/y_gendermodel_97.6%.feather")
    #feather.write_dataframe(pd.DataFrame(nrg_nona['name12_first_space_last']), "/Users/srhoads/Documents/GitHub/name_race_gender/model/corpus_gendermodel_97.6%.feather")          
    # LOADING FILES #---------------------------------------------------
    # load pickle: model = pickle.load(open('/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.sav', 'rb'))
    # load feather: X = feather.read_dataframe("/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.feather")
    # (nrg4 = nrg.sample(3000)); (nrg4 = nrg)

##20220606=================================================================================================================
    def list_to_dict(lst):
        # res_dct = {lst[i]: lst[i + 1] for i in range(0, len(lst), 2)}
        # return res_dct
        it = iter(lst)
        res_dct = dict(zip(it, it))
        return res_dct

    def recode_from_dict(pdcolumn, recode_dict={"to":["from", "fromval2"], "":["NAN", "NONE", "X", "NaN", "nan", None, np.NaN, np.nan, np.NAN], "new":"oldval"}):
        # pdcolumn.replace({"^(NAN|None|NaN)$":""})
        pdcolumn = pd.Series(pdcolumn) if not 'pandas.core.series.Series' in str(type(pdcolumn)) else pdcolumn
        # pdcolumn = pd.Series(pdcolumn.unique().tolist() + ["idk", "", np.nan, "NAN", "nan", "None", None, "from"])
        newcolumn = pdcolumn.copy()
        for k, v in recode_dict.items():
            v = [v] if not "list" in str(type(v)) else v
            replace_dict = {s:k for s in v}
            newcolumn.replace(replace_dict, inplace=True)
        return(newcolumn)

    def recode_na(pdcolumn, recode_list=["None", None, "NAN", "nan", "NaN"], use_numpy_nan=False):
        if use_numpy_nan:
            from numpy import nan, NAN, NaN
            recode_list = unique(recode_list + [nan, NAN, NaN])
        recode_dict = {np.nan:recode_list}
        newcolumn = recode_from_dict(pdcolumn, recode_dict)
        return(newcolumn)

##=================================================================================================================

def var99(myarray, var=.99):
    from sklearn.feature_selection import VarianceThreshold
    sel = VarianceThreshold(threshold=(var * (1 - var)))
    X = myarray
    x_trimmed99 = sel.fit_transform(X)
    return x_trimmed99

def getfirstname(vec):
    vec = [re.sub(r'([A-Z])', "", i)for i in vec]
    vec = [re.sub('^ ', "", i)for i in vec]
    vec = [re.sub('  $| $', "", i)for i in vec]
    vec = [re.sub('  ', " ", i)for i in vec]
    return vec

def gridbest(gs):
    print("\nBEST SCORE:\n", gs.best_score_, "\n")
    print("\nBEST PARAMS:\n", gs.best_params_, "\n")
    print("\nBEST MODEL:\n", gs.best_estimator_, "\n")

def gsreports(gs):
    print(gridbest(gs))
    print("\n", report(gs.cv_results_), "\n")

def dim(x): return x.shape

def toupper(vec):
    vec = [s.upper for s in vec]
    return vec

def report_ml_model(results, n_top=5):
    for i in range(1, n_top + 1):
        candidates = np.flatnonzero(results['rank_test_score'] == i)
        for candidate in candidates:
            print("Model with rank: {0}".format(i))
            print("Mean validation score: {0:.3f} (std: {1:.3f})".format(
                  results['mean_test_score'][candidate],
                  results['std_test_score'][candidate]))
            print("Parameters: {0}".format(results['params'][candidate]))

def scores_ml_model(model, x_test, y_test, cv=10, accuracy=True, auc_cv=True, accuracy_cv=True, confmat=True, printmodel=False):
    y_pred = model.predict(x_test) # y_pred includes your predictions
    if printmodel: print(model)
    if accuracy: print("Test-Data prediction accuracy: {:.5f}".format(model.score(x_test, y_test)))
    if confmat: print("Confusion Matrix:\n", confusion_matrix(y_test, y_pred))
    if auc_cv: print(cv,"fold CV AUC", np.mean(cross_val_score(model, x_test, y_test, cv=cv, scoring = 'roc_auc')))
    if accuracy_cv: print(cv,"fold CV Accuracy", np.mean(cross_val_score(model, x_test, y_test, cv=cv, scoring = 'accuracy')))
    # return np.mean([np.mean(cross_val_score(model, x, y, cv=10, scoring = 'roc_auc')), np.mean(cross_val_score(model, X, y, cv=10, scoring = 'accuracy'))])

# head like in R
#----------------------------------------------------------
def head(x, n=5):
    result = x[0:len(x)] if len(x) < n else x[0:n]
    return(result)
        
# list files from dir path like list.files() in r
#----------------------------------------------------------
def list_files(path = "data", pattern = ".f"):
    from os import listdir
    from os.path import isfile, join
    files = [path + "/" + f for f in listdir(path) if isfile(join(path, f))]
    files = [file for file in files if pattern in file]
    return files

# read 1 feather file
#----------------------------------------------------------
def readfr(path):
    from feather import read_dataframe
    df = read_dataframe(path)
    return df

# read multiple feather files
#----------------------------------------------------------
def readfrs(x, pattern = ".f"):
    if isinstance(x, str): x = list_files(path=x, pattern=pattern)
    import pandas as pd
    data = pd.concat([readfr(file) for file in x])
    print("You get a pandas dataframe with these dimensions!!!", data.shape)
    return data

# read feather files and save them as csv files
#----------------------------------------------------------
def readfrs_writecsvs(x, pattern = ".f"):
    if isinstance(x, str): x = list_files(path=x, pattern=pattern)
    import pandas as pd
    from re import sub
    for file in x:
        newname = sub("\\.f", ".csv", file)
        readfr(file).to_csv(newname)
        print(newname)

# read feather files and save them as csv files, but first encode them nicely
#----------------------------------------------------------
def readfrs_writecsvs_encoded(x, pattern = ".f", to_numeric = 'gender'):
    if isinstance(x, str): x = list_files(path=x, pattern=pattern)
    import pandas as pd
    from re import sub
    for file in x:
        newname = sub("\\.f", "_encoded.csv", file)
        frdata = readfr(file)
        frdata[to_numeric] = as_numeric(frdata[to_numeric])
        frdata['gender'] = as_numeric(frdata['gender'])
        frdata.to_csv(newname)
        print(newname)

# turn a character variable/feature into numeric (ie: gender to binary 0/1)
#----------------------------------------------------------
def char_to_num(vec): return((vec.astype("category")).cat.codes)
def as_numeric(vec): return((vec.astype("category")).cat.codes)

# split by some # of chars, for example, split_2 would make samantha into sa ma nt ha?
#----------------------------------------------------------
def split_n(s, n=2): return [s[idx:idx + n] for idx, val in enumerate(s) if idx%n == 0]

# split n by 2 characters
#----------------------------------------------------------
def two_split(s): return split_n(s, 2)

# flatten a list--like dplyr::combine() or unlist()
def flatten(d):
     v = [[i] if not isinstance(i, list) else flatten(i) for i in d]
     return [i for b in v for i in b]

# split a string by space
#----------------------------------------------------------
def strsplit(x, by = " ", unlist=False):
    if isinstance(x, str): x = x.split(by)
    else:
        x = [s.split(by) for s in x]
        if unlist: x = flatten(x)
    return x


# make into a pandas dataframe
#----------------------------------------------------------
def pdd(m):
    import pandas as pd
    return pd.DataFrame(m)

def clean_tokenize_feature(x, front=True, end=True, firstlastonly=False, endeach = True, fronteach = True, tokenize_by=2):
    import re
    import numpy as np
    features = [] # x = vec or string
    if isinstance(x, list) or isinstance(x, np.ndarray):
        for xi in x:
            features.extend(clean_feature(xi, front=front, end=end))
    else:
        x = "".join([ c if (c.isalpha()) else " " for c in x.lower()])
        features.append(" ".join(split_n(x, n=tokenize_by)))
    if front:
        features = ["33333FRONT33333" + feature for feature in features]
    if fronteach:
        features = [re.sub("  ", "  33333FRONT33333", str(feature)) for feature in features]          
    if end:
        features = [feature + "88888END88888" for feature in features]    
    if endeach:
        features = [re.sub("  ", "88888END88888  ", str(feature)) for feature in features]          
    if firstlastonly:
        features = [feature.split(" ")[-0] + " " + feature.split(" ")[-1] for feature in features]
    features = [(re.sub("  33333FRONT3333388888END88888  ", " ", str(feature))) + " " + 
                (re.sub("33333FRONT33333|88888END88888","", " ".join([word for word in feature.split(" ") 
                if any(letter in word for letter in '33333FRONT33333|88888END88888')]))) for feature in features]
    features = [re.sub("  ", " ", str(feature)) for feature in features]
    return features

# flatten dataframe values i guess?
#----------------------------------------------------------
def dflat(x): # x = pd.DataFrame
    if not isinstance(x, list):
        return(list(x.values.flatten()))
    else:
        return(x)

# not like unlist in R (use flatten() for that) but does break stuff down somehow?
#----------------------------------------------------------
def unlist(listofvecsofstrings):
    v = [[i] if not isinstance(i, list) else flatten(i) for i in listofvecsofstrings]
    return [i for b in v for i in b]

# return just a formatted array of features
#----------------------------------------------------------
def get_my_x(x, front=True, end=True, firstlastonly=False, endeach = True, fronteach = True, matrix=False, feature_names=False, justfit=False, justvectorizer=False):
    x = dflat(x)
    corpus = clean_feature(x, front=front, end=end, firstlastonly=firstlastonly)
    from sklearn.feature_extraction.text import CountVectorizer
    vectorizer = CountVectorizer()
    X = vectorizer.fit(corpus)
    if justfit:
        return(X)
    if justvectorizer:
        return(vectorizer)
    if feature_names:
        return(X.get_feature_names())
    else:
        result = {
            'X': X.transform(corpus).toarray(),
            'transformer': lambda y: X.transform(clean_feature(y, front=front, end=end, firstlastonly=firstlastonly, endeach = endeach, fronteach = fronteach)).toarray()
        }
        if matrix:
            return X.transform(corpus).toarray()
        else:
            return result


# get x and y variables ready to go, from original dataframe
#----------------------------------------------------------
def getxy(df, x = "name", y = "gender", sample = None, dropna = True, both = True, justx = False, justy = False, feature_names=False, justfit=False, justvectorizer=False):
    if sample is not None:
        df = df.sample(sample)
    if dropna:
        df = (df[[x, y]]).dropna()
    if justx and not justy:
        return (get_my_x(df[x], feature_names=feature_names, justfit=justfit, justvectorizer=justvectorizer))["X"]
    if justy and not justx:
        return (df[y].astype("category")).cat.codes
    if justx and justy:
        result = {
            'x': (get_my_x(df[x], feature_names=feature_names, justfit=justfit, justvectorizer=justvectorizer))["X"], 
            'y': (df[y].astype("category")).cat.codes
        }
        print("getxy's x & y dict keys are:", result.keys(), "\n")
        return result
    else:
        result = {
            'x': (get_my_x(df[x], feature_names=feature_names, justfit=justfit, justvectorizer=justvectorizer))["X"], 
            'y': (df[y].astype("category")).cat.codes
        }
        print("getxy's x & y dict keys are:", result.keys())
        return result

# feather path dir or filenames input to get x and y for model
#----------------------------------------------------------
def frs_toxy(path=None, filenames=None, x = "name", y = "gender", sample = None, dropna = True, both = True, justx = False, justy = False,feature_names=False):
    if filenames is not None:
        data = readfrs(filenames)
    else:
        data = readfrs(path)
    result = getxy(data, x = x, y = y, sample = sample, dropna = dropna, both = both, justx = justx, justy = justy, feature_names=feature_names)
    return result

# read feather files and get test-train output
def frs_toxy_testtrain(path=None,filenames=None, x = "name", y = "gender", sample = None, dropna = True, both = True, justx = False, justy = False):
    xy = frs_toxy(path=path,filenames=filenames, x = x, y = y, sample = sample, dropna = dropna, both = both, justx = justx, justy = justy)
    return testtrain(xy = xy)

def source_v1(pyfile="functions.py"):
    sourcedfile = open(pyfile)
    exec(sourcedfile.read())

# test-train split from xy output of frs_toxy
#----------------------------------------------------------
def testtrain(xy = None, x = None, y = None):
    from sklearn.model_selection import train_test_split
    if x is not None and y is not None:
        x_train, x_test, y_train, y_test = train_test_split(x, y, random_state=0, stratify = y)
        result = {
            'x_train': x_train, 
            'x_test': x_test, 
            'y_train': y_train, 
            'y_test': y_test
        }
        print("test-train dict keys:", result.keys())
        return result
    else:
        x_train, x_test, y_train, y_test = train_test_split(xy["x"], xy["y"], random_state=0, stratify = xy["y"])
        result = {
            'x_train': x_train, 
            'x_test': x_test, 
            'y_train': y_train, 
            'y_test': y_test
        }
        print("test-train dict keys:", result.keys())
        return result

# get some made-up sample data in the various common forms
#----------------------------------------------------------
def getdata(getdf=False, getstring=False, getnestedlist=False, getdict=False, getarray=False, getlist=False):
    import numpy as np
    import pandas as pd
    x = np.random.randn(10, 4)
    if getarray:
        return x
    if getdf:
        return pd.DataFrame(x, columns=list('ABCD'))
    if getnestedlist:
        return x.tolist()
    if getlist:
        return flatten(x.tolist())
    if getdict:
        return pd.DataFrame(x, columns=list('ABCD')).to_dict()
    if getstring:
        return "this STRING has some spaces and 10-7 numbers"
    else:
        print("array", "dataframe", "nestedlist", "list", "dict", "string")



# print("If u see this message, CONGRATS! You imported the most updated version of the functions file WEE")



# read 1 feather file
def read_feather(path):
    from feather import read_dataframe
    df = read_dataframe(path)
    return df
    
# read 1 csv file
def read_csv(path):
    from pandas import read_csv
    df = read_csv(path)
    return df

# read multiple feather files
def read_feathers(filenames):
    import pandas as pd
    data = pd.concat([read_feather(file) for file in filenames])
    print("You get a pandas dataframe with these dimensions!!!", data.shape)
    return(data)

# read multiple feather files
def read_csvs(filenames):
    import pandas as pd
    data = pd.concat([read_csv(file) for file in filenames])
    print("You get a pandas dataframe with these dimensions!!!", data.shape)
    return(data)
    
# read feather files from a directory path
def readfrs_fromdir(dir = "data"):
    files = list_files(dir)
    data = readfrs(files)
    return data

# clean features: lowercase, alpha characters, tokenize, vectorize...
def clean_feature(x, front=True, end=True, firstlastonly=False, endeach = True, fronteach = True, tokenize_by=2):
    # from re import sub
    import re
    import numpy as np
    features = [] # x = vec or string
    if isinstance(x, list) or isinstance(x, np.ndarray):
        for xi in x:
            features.extend(clean_feature(xi, front=front, end=end))
    else:
        x = "".join([ c if (c.isalpha()) else " " for c in x.lower()])
        features.append(" ".join(split_n(x, n=tokenize_by)))
    if front:
        features = ["33333FRONT33333" + feature for feature in features]
    if fronteach:
        features = [re.sub("  ", "  33333FRONT33333", str(feature)) for feature in features]          
    if end:
        features = [feature + "88888END88888" for feature in features]    
    if endeach:
        features = [re.sub("  ", "88888END88888  ", str(feature)) for feature in features]          
    if firstlastonly:
        features = [feature.split(" ")[-0] + " " + feature.split(" ")[-1] for feature in features]
    features = [(re.sub("  33333FRONT3333388888END88888  ", " ", str(feature))) + " " + (re.sub("33333FRONT33333|88888END88888","", " ".join([word for word in feature.split(" ") if any(letter in word for letter in '33333FRONT33333|88888END88888')]))) for feature in features]
    features = [re.sub("  ", " ", str(feature)) for feature in features]
    return features


# def checkup():
#     print("working?")


#### OTHER PPL'S FXNS FROM THE INTERNET:
##******************************************************************************# 
# lemmatizing text--making consistent tenses and word forms like run and running
def lemmatize_stemming(text):
    from nltk.stem import WordNetLemmatizer, SnowballStemmer
    stemmer = nltk.stem.SnowballStemmer()
    return stemmer.stem(WordNetLemmatizer().lemmatize(text, pos='v'))

# not my own function--uses it's own thing to remove stopwords
def preprocess(text):
    from gensim.utils import simple_preprocess
    from gensim.parsing.preprocessing import STOPWORDS
    result = []
    for token in gensim.utils.simple_preprocess(text):
        if token not in gensim.parsing.preprocessing.STOPWORDS and len(token) > 3:
            result.append(lemmatize_stemming(token))
    return result


## IMPORTS + FUNCTIONS FOR IMPORTS
##******************************************************************************
# gensim: Dictionary, LdaModel, Word2Vec
def import_gensim():
    from gensim.corpora.dictionary import Dictionary
    from gensim.models.ldamodel import LdaModel
    from gensim.models import Word2Vec
    from gensim.utils import simple_preprocess
    from gensim.parsing.preprocessing import STOPWORDS
    print("Dictionary", "LdaModel", "Word2Vec", "simple_preprocess", "STOPWORDS")

# import the basics!
def import_nppd():
    import numpy as np
    import pandas as pd
    print("np", "pd")

# import nltk
def import_nltk():
    from nltk.stem import WordNetLemmatizer, SnowballStemmer
    import nltk.stem.porter as nsp
    print("WordNetLemmatizer", "SnowballStemmer", "'nltk.stem.porter as smp'")

# import stuff relevant to text processing/NLP
def import_nlp():
    print("\nnumpy; pandas as:")
    import_nppd()
    print("\nnltk:")
    import_nltk()
    print("\ngensim:")
    import_gensim()
    from sklearn.feature_extraction.text import CountVectorizer
    print("\nsklearn:","CountVectorizer")


## OLD WAYS OF IMPORTING/SOURCING FILES
##******************************************************************************
#### import:
##----------------------------------------------------------
# from functions import *

### or full path...
##----------------------------------------------------------
# import sys
# sys.path.append('/Users/srhoads/GitHub/namegender')
# from functions_test import *

#### reimport/reload:
##----------------------------------------------------------
# import importlib
# import functions
# importlib.reload(functions)
# from functions import *
##******************************************************************************




# print("If u see this message, CONGRATS! You imported the most updated version of the functions file WEE")




#======# print("below=way f-ing old ugh") #========================================================================================================================================================================================================






def space_split(s):
    splits = []
    for x in s:
        splits.append(x.split(" "))
    return splits

def no_split(s):
    return [s[idx:idx + 25] for idx, val in enumerate(s) if idx%25 == 0]
def one_split(s):
    return [s[idx:idx + 1] for idx, val in enumerate(s) if idx%1 == 0]
def three_split(s):
    return [s[idx:idx + 3] for idx, val in enumerate(s) if idx%3 == 0]
def four_split(s):
    return [s[idx:idx + 2] for idx, val in enumerate(s) if idx%2 == 0]



################################### HELPER FUNCTIONS (diversity-planning-project) (2021-04-12) ###################################

def unlist(l):
    flat_list = []
    for sublist in l:
        for item in sublist:
            flat_list.append(item)
    return flat_list

def intersection(lst1, lst2): 
    lst3 = [value for value in lst1 if value in lst2] 
    return lst3 

def drop_empty(l):
    l_copy = l.copy()
    while("" in l_copy) :
        l_copy.remove("")
    return l_copy

def unique(list1): 
    # intilize a null list 
    unique_list = [] 
    # traverse for all elements 
    for x in list1: 
        # check if exists in unique_list or not 
        if x not in unique_list: 
            unique_list.append(x) 
    return unique_list

# def list_remove(l, remove_list=['']):
#     l_copy = list(l.copy()) if type(l)==str else l.copy()
#     for remove_str in remove_list:
#         l_copy.remove(remove_str) if remove_str in l_copy else None
#     return l_copy

def list_remove(l, values=['VALUETOREMOVE']):
    l0 = l.copy()
    if isinstance(values, str):
        values = [values]
    for value in values:
        while value in l0:
            l0.remove(value)
    return l0

def list_select_matches(l, patterns=['9570', '9760'], invert=False, exact=True):
    l_copy = list(l.copy()) if type(l)==str else l.copy()
    if exact:
        if invert:
            for pattern in patterns:
                l_copy.remove(pattern) if pattern in l_copy else None
        else:
            l_copy = [pattern for pattern in patterns if pattern in l_copy]
    else:
        if invert:
            l_copy2 = []
            for s in l_copy:
                l_copy2 = l_copy2 + [s for pattern in patterns if not pattern in s]
            l_copy = l_copy2.copy()
        else:
            l_copy2 = []
            for s in l_copy:
                l_copy2 = l_copy2 + [s for pattern in patterns if pattern in s]
            l_copy = l_copy2.copy()
    return l_copy
    
def split_string_vec(string_pre_form='blah, blah', by=','):
    strings_pre = string_pre_form if type(string_pre_form)==list else [p.strip() for p in str(string_pre_form).split(by)]
    strings = []
    for string in strings_pre:
        if (string != ""):
            strings.append(string) # print(string_pre_form, " --> ", strings)  
    return(strings)

def df_select_matches(df, pattern="", ignore_case=True, invert=False):
    flags = re.IGNORECASE if ignore_case else False
    df_copy = df.copy()
    colnames = df_copy.columns[~df_copy.columns.str.contains(pattern, flags=flags)] if invert else df_copy.columns[df_copy.columns.str.contains(pattern, flags=flags)]
    return df_copy[colnames]

def df_get_preferred_column(df, patterns=["emp.*id", ".*emp.*id.*|identif|emp.*name|emp.*num|^eid$", "^id$|id.*(num|code|\\#)|ident.*(num|code|\\#)|emp.*(num|code|\\#)|\\beid\\b"], fillmissingwith='', verbose=False):
    df_copy = df.copy() # df = pd.read_csv("~/Downloads/EEO1_WORKFORCE_SAMPLE.csv")
    newcolname = []
    for pattern in patterns:
        if pattern and len(newcolname)==0:
            newcolname = df_copy.columns[df_copy.columns.str.contains(pattern, flags=re.IGNORECASE)]
    print("newcolname:", newcolname) if verbose else None
    df_copydesiredcolumn = [fillmissingwith]*len(df_copy) if len(newcolname)==0 else df_copy[newcolname[0]]
    return df_copydesiredcolumn

def df_mutate_at(df, pattern, fun, verbose=False):
    dfObj = df.copy()
    relevant_cols = dfObj.columns[dfObj.columns.str.contains(pattern)]
    print('#applying df_mutate_at() to...',fun,'relevant_cols=', relevant_cols.tolist()) if verbose else None
    modDfObj = dfObj.apply(lambda x: fun(x) if x.name in relevant_cols else x)
    return modDfObj

def df_mutate_if(df, pattern='object', fun=None, verbose=False):
    dfObj = df.copy()
    relevant_cols = dfObj.select_dtypes(include=[pattern]).columns
    print('#applying df_mutate_if() to...',fun,'relevant_cols=', relevant_cols.tolist()) if verbose else None
    modDfObj = dfObj.apply(lambda x: fun(x) if x.name in relevant_cols else x)
    return modDfObj

def rename_columns(df=None, prestrings=['', '', ''], newstrings=['', '', ''], exact=False):
    df_copy = df.copy()
    for i in range(0, len(prestrings)): # i = 1
        if not exact:
            df_copy.rename(columns={col: col.replace(prestrings[i], newstrings[i]) for col in df_copy.columns}, inplace=True) 
        else:
            df_copy.rename(columns={prestrings[i]: newstrings[i]}, inplace=True)
    return df_copy

def sort_columns(df=None, bycols=['', '', '']):
    df_copy = df.copy()
    for bycol in bycols: # i = 1
        df_copy = df_copy.sort_values(bycol) if bycol in df_copy else df_copy
    df_copy = df_copy.reset_index(drop=True)
    return df_copy

def find_nearest(array, value):
    value = float(value) if type(value)==str else value
    array = np.asarray(array)
    idx = (np.abs(array - value)).argmin()
    return array[idx]

def recode_from_dict(pdcolumn, recode_dict={"to":["from", "fromval2"], "":["NAN", "NONE", "X", "NaN", "nan", None, np.NaN, np.nan, np.NAN], "new":"oldval"}):
    pdcolumn = pd.Series(pdcolumn) if not 'pandas.core.series.Series' in str(type(pdcolumn)) else pdcolumn
    newcolumn = pdcolumn.copy()
    for k, v in recode_dict.items():
        v = [v] if not "list" in str(type(v)) else v
        replace_dict = {s:k for s in v}
        newcolumn.replace(replace_dict, inplace=True)
    return(newcolumn)

def recode_0s_pad_str(s="1234", length=4, empty_if_all_0s=True, only_numbers=False):
    s = str(s).strip()
    s = re.sub("-|[a-zA-Z]", "", s).strip() if only_numbers else s
    sfull = s.rjust(length, '0') # adding leading zero 
    sfull = sfull.replace("0"*length, "") if empty_if_all_0s else sfull
    return sfull

def recode_sex(pdcolumn):
    pdcolumn = pd.Series(pdcolumn.copy()) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    newcolumn = pdcolumn.str.replace(" ", "").str.lower().replace(
    ["female",  "f", "fem",     "male",  "m"],  
    ["2", "2", "2",     "1",    "1"]
    )
    return newcolumn

def recode_sex_to_male_female(pdcolumn):
    pdcolumn = pd.Series(pdcolumn.copy()) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    newcolumn = pdcolumn.str.replace(" ", "").str.lower().replace(
    ["2", "1", "f", "m"],  
    ["female", "male", "female", "male"]
    )
    return newcolumn

def recode_hisp(pdcolumn):
    pdcolumn = pd.Series(pdcolumn.copy()) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    newcolumn = pdcolumn.apply(str).str.replace(" ", "").str.lower().replace(
    ["hispanicorlatino","3","hispanic","hisp", "latino", "lat"],   
    ["y", "y", "y","y","y","y"]
    ).apply(lambda x: "y" if(x == 'y') else "01")
    return newcolumn

def recode_race(pdcolumn):
    pdcolumn = pd.Series(pdcolumn.copy()) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    newcolumn = pdcolumn.apply(str).str.replace(" ", "").str.lower().replace(
    # ["white","w","wh","1",     "black","blackorafricanamerican","2",     "3","3","3",     "4","4","4",     "5","americanindianorotheralaskanative","5",     "asian","6","6",     "7","twoormoreraces","nhopi",     "8","other","8",     "hispanicorlatino","9","9"],  
    # ["1", "1", "1","1",          "2","2","2",                            "9","9","9",     "6","6","6",     "5","5","5",                                     "6","6","6",         "9","9","7",                  "8","8","8",         "9","9","9"])
    ["1","2","3","4","5","6","7"], 
    ["white","black","hisp","asian","amerind","nhopi","twoplus"]
    )
    newcolumn = newcolumn.str.replace(" ", "").str.lower().replace(
        ["white","black","hisp","asian","amerind","nhopi","twoplus"],
        ["1",    "2",    "8",   "6",    "5",    "7",    "9"])
    return newcolumn

def recode_race_to_jl_num(pdcolumn):
    pdcolumn = pd.Series(pdcolumn.copy()) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    newcolumn = pdcolumn.apply(str).str.replace(" ", "").str.lower().replace(
        ["white","black","hisp","asian","amerind","nhopi","twoplus"],
        ["1",    "2",    "8",   "6",    "5",    "7",    "9"])
    return newcolumn

def pre_clean_codes(s):
    s0 = re.sub("\\\r|\\\n|\\\t|'|\"|_|;|\\(|\\)|\\=|\\<|\\>|\\!|\\?|\\[|\\](,| ){2,10}|, ,", ',', s)
    s1 = re.sub("( ){2,10}", ' ', s0)
    s2 = re.sub('(,){2,10}|, |, ,', ',', s1).strip('^(,| )|(,| )$').strip()
    return s2

def clean_occp_str(s):
    s0 = re.sub("[a-zA-Z]|\\.|:|201(0|7|8)-201(7|9)|-|&|\\/| [0-9][0-9]-[0-9][0-9] |(,){2,20}|, |\\\r|\\\n|\\\t|'|_|;|\\(|\\)|\\=|\\<|\\>|\\!|\\?|\\[|\\]", ",", s).replace(',,', ',').strip('^,|,$').strip()
    s1 = re.sub('( |\\s){2,20}', ' ', s0).strip().replace(', ', ',')
    return s1

def state_abb_to_name(pdcolumn):
    states_dict = {"AL":"Alabama","AK":"Alaska","AZ":"Arizona","AR":"Arkansas","CA":"California","CO":"Colorado","CT":"Connecticut","DC":"District of Columbia","DE":"Delaware","FL":"Florida","GA":"Georgia","HI":"Hawaii","ID":"Idaho","IL":"Illinois","IN":"Indiana","IA":"Iowa","KS":"Kansas","KY":"Kentucky","LA":"Louisiana","ME":"Maine","MD":"Maryland","MA":"Massachusetts","MI":"Michigan","MN":"Minnesota","MS":"Mississippi","MO":"Missouri","MT":"Montana","NE":"Nebraska","NV":"Nevada","NH":"New Hampshire","NJ":"New Jersey","NM":"New Mexico","NY":"New York","NC":"North Carolina","ND":"North Dakota","OH":"Ohio","OK":"Oklahoma","OR":"Oregon","PA":"Pennsylvania","RI":"Rhode Island","SC":"South Carolina","SD":"South Dakota","TN":"Tennessee","TX":"Texas","UT":"Utah","VT":"Vermont","VA":"Virginia","WA":"Washington","WV":"West Virginia","WI":"Wisconsin","WY":"Wyoming","PR":"Puerto Rico"}
    pdcolumn = pd.Series(pdcolumn) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    pdcolumn = pd.Series([s.strip() for s in pdcolumn]).str.upper()
    newcolumn = pdcolumn.replace(states_dict, regex=False)
    return newcolumn

def state_abb_to_region(pdcolumn):
    states_dict = {"AL":"South","AK":"West","AZ":"West","AR":"South","CA":"West","CO":"West","CT":"Northeast","DC":"South","DE":"South","FL":"South","GA":"South","HI":"West","ID":"West","IL":"Midwest","IN":"Midwest","IA":"Midwest","KS":"Midwest","KY":"South","LA":"South","ME":"Northeast","MD":"Northeast","MA":"Northeast","MI":"Midwest","MN":"Midwest","MS":"South","MO":"Midwest","MT":"West","NE":"Midwest","NV":"West","NH":"Northeast","NJ":"Northeast","NM":"West","NY":"Northeast","NC":"South","ND":"Midwest","OH":"Midwest","OK":"South","OR":"West","PA":"Northeast","RI":"Northeast","SC":"South","SD":"Midwest","TN":"South","TX":"South","UT":"West","VT":"Northeast","VA":"South","WA":"West","WV":"South","WI":"Midwest","WY":"West","PR":"Caribbean"}
    pdcolumn = pd.Series(pdcolumn) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    pdcolumn = pd.Series([s.strip() for s in pdcolumn]).str.upper()
    newcolumn = pdcolumn.replace(states_dict, regex=False)
    return newcolumn

def sql_list_colnames(cursor, table_name="clientmanagement_client"):
    select_column_names_str = "select * from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME='" + table_name + "'"
    cursor.execute(select_column_names_str)
    colnamesplusdata = cursor.fetchall() #[desc[0] for desc in cursor.description]
    colnames = [x[3] for x in colnamesplusdata]
    return colnames

def recode_race_to_jl_word(pdcolumn):
    pdcolumn = pd.Series(pdcolumn.copy()) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    newcolumn = pdcolumn.apply(str).str.replace(" ", "").str.lower().replace(
        ["1","2","3","4","5","6","7"],
        ["white","black","hisp","asian","amerind","nhopi","twoplus"]
        )
    return newcolumn

def recode_race_acs_to_jl(pdcolumn): # https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2014-2018.pdf
    newcolumn = pdcolumn.apply(str).str.replace(" ", "").str.lower().replace(
        ["1",     "2",     "3",       "4",       "hisp", "5",       "6",     "7",     "8",  "9"],
        ["white", "black", "amerind", "amerind", "hisp", "amerind", "asian", "nhopi", None, "twoplus"])
    return newcolumn

def recode_state(state, to_fips=False): # state="Califoria"
    state = str(state).upper()
    if len(state)>2:
        state = re.sub("NEW", "NEW ", state, flags=re.IGNORECASE)
        state = re.sub("WEST", "WEST ", state, flags=re.IGNORECASE)
        state = re.sub("NORTH", "NORTH ", state, flags=re.IGNORECASE)
        state = re.sub("SOUTH", "SOUTH ", state, flags=re.IGNORECASE)
        state = re.sub("  ", " ", state, flags=re.IGNORECASE).strip()
        try:
            from us.states import lookup
            state = lookup(state).abbr
        except:
            pass
    if to_fips:
        try:
            from us.states import lookup
            state = lookup(state).abbr
            state = lookup(state).fips
        except:
            pass
    return(state)

def df_select_matches2(df, patterns=['', ''], ignore_case=True, invert=False):
    flags = re.IGNORECASE if ignore_case else False
    df_copy = df.copy()
    if type(patterns)!=str:
        colnames = unique(unlist([df_copy.columns[~df_copy.columns.str.contains(pattern, flags=flags)] if invert else df_copy.columns[df_copy.columns.str.contains(pattern, flags=flags)] for pattern in patterns]))
    else:
        colnames = df_copy.columns[~df_copy.columns.str.contains(patterns, flags=flags)] if invert else df_copy.columns[df_copy.columns.str.contains(patterns, flags=flags)]
    return df_copy[colnames]


def printurn(x):
    print(x)
    return(x)

def recode_onehot_if(s='CHILDRENS COAT', pattern='CHILDREN'):
    result = '1' if pattern in str(s) else '0'
    return(result)

def check_color(color):
    pkg('colour'); from colour import Color
    try:
        Color(color)
        return True
    except ValueError:
        return False

def extract_color(pdcolumn):
    pdcolumn = pd.Series(pdcolumn.copy()) if type(pdcolumn)==str else pd.Series(pdcolumn) if type(pdcolumn)==list else pdcolumn
    newcolumn = pdcolumn.copy().apply(lambda s: ' '.join([i for i in str(s).split(' ') if check_color(i)]))
    return(newcolumn)
    


def round_up(number, decimals=0):
    """Function for rounding."""
    multiplier = 10 ** decimals
    return math.ceil(number * multiplier) / multiplier  if not math.isnan(number) else number

def round_down(number, decimals=0):
    """Function for rounding."""
    multiplier = 10 ** decimals
    return math.floor(number * multiplier) / multiplier if not math.isnan(number) else number

def merge_two_dicts(x_dict, y_dict):
    """Given two dicts, merge them into a new dict as a shallow copy."""
    z_dict = x_dict.copy()
    z_dict.update(y_dict)
    return z_dict

def select_dict(dictionary, pattern='(e|E)mp.*'):
    """Selecting elements from a dictionary based on a pattern"""
    re_obj = re.compile(pattern, re.IGNORECASE)
    res = dict(filter(lambda item: re_obj.match(item[0]), dictionary.items())) 
    return res

def pct_str_to_dec(s='10%'):
    """Turn a number string into a decimal"""
    s = re.sub('%', '', s)
    try:
        s = float(s)/100
    except:
        s = float(np.nan)
    return s

def str_to_float_force(s):
    try:
        s = float(s)
    except:
        s = float(np.nan)
    return s

#===================== DUBIOUS INSTRUCTIONAL STUFF ==============================================================================================================================================

# ways of importing py files/reloading them
## import:
# from functions import *
## reimport/reload:
# import importlib
# import functions
# importlib.reload(functions)
# from functions import *
## import functions.py file to get ur defs (works for reimporting too)
# functions = open('functions.py')
# exec(functions.read())

# LOADING FILES
# import pickle
# import feather
# load pickle: model = pickle.load(open('/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.sav', 'rb'))
# load feather: X = feather.read_dataframe("/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.feather")

# WRITING FILES
# import pickle
#pickle.dump(model, open('/Users/srhoads/Documents/GitHub/name_race_gender/model/gendermodel_97.6%.sav', 'wb'))
# import feather
#feather.write_dataframe(pd.DataFrame(X), "/Users/srhoads/Documents/GitHub/name_race_gender/model/X_gendermodel_97.6%.feather")
#feather.write_dataframe(pd.DataFrame(y), "/Users/srhoads/Documents/GitHub/name_race_gender/model/y_gendermodel_97.6%.feather")
#feather.write_dataframe(pd.DataFrame(nrg_nona['name12_first_space_last']), "/Users/srhoads/Documents/GitHub/name_race_gender/model/corpus_gendermodel_97.6%.feather")





#===================== UNNECESSARY/OLD/MISCELLANEOUS ==============================================================================================================================================

# # split by n # of characters (like two_split but ambiguous)
# def tokenize_by_n(s, n=2):
#     return split_n(s, n)

# import gensim
# from gensim.corpora.dictionary import Dictionary
# from gensim.models import Word2Vec
# from gensim.models.ldamodel import LdaModel
# from gensim.utils import simple_preprocess
# from gensim.parsing.preprocessing import STOPWORDS
# import collections
# from nltk.stem import WordNetLemmatizer, SnowballStemmer
# from nltk.stem.porter import *

# import sys
# sys.path.append('/Users/srhoads/GitHub/namegender')
# from functions_test import *

# try:
#     exec(pkgs2([['feather_format', '*']])[0])
# except Exception:
#     None

# https://towardsdatascience.com/how-to-customize-jupyterlab-keyboard-shortcuts-72321f73753d

# def pkg(package, via = "pip"):
#     from pip._internal import main
#     from os import system
#     try: return __import__(package)
#     except ImportError: 
#         try:
#             if via == "pip":
#                 main(['install', '--user', package]) 
#             if via == "conda":
#                 system('python3 conda install ' + package)
#         except:
#             print("Sorry kid... can't install this guy")
# pkg("yapf")

# def dim(x):
#     import pandas as pd
#     if isinstance(x, pd.DataFrame): return x.shape
#     else: return len(x)

# def getxy(df, x = "name", y = "gender", sample = None, dropna = True, both = True, justx = False, justy = False):
#     if sample is not None:
#         df = df.sample(sample)
#     if dropna:
#         df = (df[[x, y]]).dropna()
#     if justx and not justy:
#         return (get_my_x(df[x]))["X"]
#     if justy and not justx:
#         return (df[y].astype("category")).cat.codes
#     if justx and justy:
#         result = {
#             'x': (get_my_x(df[x]))["X"], 
#             'y': (df[y].astype("category")).cat.codes
#         }
#         print("getxy's x & y dict keys are:", result.keys())
#         return result
#     else:
#         result = {
#             'x': (get_my_x(df[x]))["X"], 
#             'y': (df[y].astype("category")).cat.codes
#         }
#         print("getxy's x & y dict keys are:", result.keys())
#         return result

# test-train split from xy output of frs_toxy
# def testtrain(xy = None, x = None, y = None):
#     from sklearn.model_selection import train_test_split
#     if x is not None and y is not None:
#         x_train, x_test, y_train, y_test = train_test_split(x, y, random_state=0, stratify = y)
#         result = [
#             x_train, x_test, y_train, y_test
#         ]
#         return result
#     else:
#         x_train, x_test, y_train, y_test = train_test_split(xy["x"], xy["y"], random_state=0, stratify = xy["y"])
#         result = [
#             x_train, x_test, y_train, y_test
#         ]
#         return result

# def get_my_x(x, front=False, end=False, firstlastonly=False, matrix=False):
#     x = dflat(x)
#     corpus = clean_feature(x, front=front, end=end, firstlastonly=firstlastonly)
#     from sklearn.feature_extraction.text import CountVectorizer
#     vectorizer = CountVectorizer()
#     X = vectorizer.fit(corpus)
#     result = {
#         'X': X.transform(corpus).toarray(),
#         'transformer': lambda y: X.transform(clean_feature(y, front=front, end=end, firstlastonly=firstlastonly)).toarray()
#     }
#     if matrix:
#         return X.transform(corpus).toarray()
#     else:
#         return result

# clean features: lowercase, alpha characters, tokenize, vectorize...
#----------------------------------------------------------
# def clean_feature(x, front=True, end=True, firstlastonly=False, endeach = True, fronteach = True, tokenize_by=2):
#     import re
#     import numpy as np
#     features = [] # x = vec or string
#     if isinstance(x, list) or isinstance(x, np.ndarray):
#         for xi in x:
#             features.extend(clean_feature(xi, front=front, end=end))
#     else:
#         x = "".join([ c if (c.isalpha()) else " " for c in x.lower()])
#         features.append(" ".join(split_n(x, n=tokenize_by)))
#     if front:
#         features = ["FRONT011one01" + feature for feature in features]
#     if fronteach:
#         features = [re.sub("  ", "  FRONT011one01", str(feature)) for feature in features]          
#     if end:
#         features = [feature + "END011one01" for feature in features]    
#     if endeach:
#         features = [re.sub("  ", "END011one01  ", str(feature)) for feature in features]          
#     if firstlastonly:
#         features = [feature.split(" ")[-0] + " " + feature.split(" ")[-1] for feature in features]
#     features = [(re.sub("  FRONT011one01END011one01  ", " ", str(feature))) + " " + (re.sub("FRONT011one01|END011one01","", " ".join([word for word in feature.split(" ") if any(letter in word for letter in 'FRONT011one01|END011one01')]))) for feature in features]
#     features = [re.sub("  ", " ", str(feature)) for feature in features]
#     return features
