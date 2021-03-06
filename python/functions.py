##******************************************************************************
######--------ways of importing py files/reloading them #### import functions.py file to get ur defs (works for reimporting too)--------######
# functions = (open('functions.py').read()); exec(functions.read())
###----or full path...----------------------------------------------------------------------
# functions = open('Users/srhoads/GitHub/namegender/functions.py'); exec(functions.read())
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
            ['imblearn', '*'],
            # ['', '*'],
            # ['', '*'],
            # ['', '*']
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
        print(e)
    import matplotlib.pyplot as plt
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
    import re
    import time
    from time import time
    import pickle
    import mglearn
    from scipy.stats import randint as sp_randint
    try:
        from xgboost import XGBClassifier
    except Exception as e:
        # print(e); os.system("brew install libomp")
        os.system("pip3 install xgboost")
        try:
            from xgboost import XGBClassifier
        except Exception as e:
            "xgboost.XGBClassifier not loading correctly, sadly :("
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
        print(e)
    # WRITING FILES #---------------------------------------------------
    import pickle
    #pickle.dump(model, open('/Users/srhoads/Documents/GitHub/name_race_gender/model/gendermodel_97.6%.sav', 'wb'))
    import feather
    #feather.write_dataframe(pd.DataFrame(X), "/Users/srhoads/Documents/GitHub/name_race_gender/model/X_gendermodel_97.6%.feather")
    #feather.write_dataframe(pd.DataFrame(y), "/Users/srhoads/Documents/GitHub/name_race_gender/model/y_gendermodel_97.6%.feather")
    #feather.write_dataframe(pd.DataFrame(nrg_nona['name12_first_space_last']), "/Users/srhoads/Documents/GitHub/name_race_gender/model/corpus_gendermodel_97.6%.feather")          
    # LOADING FILES #---------------------------------------------------
    # load pickle: model = pickle.load(open('/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.sav', 'rb'))
    # load feather: X = feather.read_dataframe("/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.feather")
    # (nrg4 = nrg.sample(3000)); (nrg4 = nrg)

#=================================================================================================================

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

def report(results, n_top=5):
    for i in range(1, n_top + 1):
        candidates = np.flatnonzero(results['rank_test_score'] == i)
        for candidate in candidates:
            print("Model with rank: {0}".format(i))
            print("Mean validation score: {0:.3f} (std: {1:.3f})".format(
                  results['mean_test_score'][candidate],
                  results['std_test_score'][candidate]))
            print("Parameters: {0}".format(results['params'][candidate]))

def scores(model, x_test, y_test, cv=10, accuracy=True, auc_cv=True, accuracy_cv=True, confmat=True, printmodel=False):
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


def clean_feature_nosplit(x, front=False, end=False):
    features = []
    if isinstance(x, list) or isinstance(x, np.ndarray):
        for xi in x:
            features.extend(clean_feature_nosplit(xi))
    else:
        x = "".join([ c if (c.isalnum() and (not c.isdigit())) else "" for c in x.lower()])
        features.append(" ".join(no_split(x)))
    if front:
        features = [(feature.split(" ")[-0] + "FRONT!11one!1 ") + feature for feature in features]
    if end:
        features = [feature + (" " + feature.split(" ")[-1] + "END!11one!1") for feature in features]    
    return features
def clean_feature_1split(x, front=False, end=False):
    features = []
    if isinstance(x, list) or isinstance(x, np.ndarray):
        for xi in x:
            features.extend(clean_feature_1split(xi))
    else:
        x = "".join([ c if (c.isalnum() and (not c.isdigit())) else "" for c in x.lower()])
        features.append(" ".join(one_split(x)))
    if front:
        features = [(feature.split(" ")[-0] + "FRONT!11one!1 ") + feature for feature in features]
    if end:
        features = [feature + (" " + feature.split(" ")[-1] + "END!11one!1") for feature in features]    
    return features
def clean_feature_2split(x, front=False, end=False):
    features = []
    if isinstance(x, list) or isinstance(x, np.ndarray):
        for xi in x:
            features.extend(clean_feature_2split(xi))
    else:
        x = "".join([ c if (c.isalnum() and (not c.isdigit())) else "" for c in x.lower()])
        features.append(" ".join(two_split(x)))
    if front:
        features = [(feature.split(" ")[-0] + "FRONT!11one!1 ") + feature for feature in features]
    if end:
        features = [feature + (" " + feature.split(" ")[-1] + "END!11one!1") for feature in features]    
    return features
def clean_feature_3split(x, front=False, end=False):
    features = []
    if isinstance(x, list) or isinstance(x, np.ndarray):
        for xi in x:
            features.extend(clean_feature_3split(xi))
    else:
        x = "".join([ c if (c.isalnum() and (not c.isdigit())) else "" for c in x.lower()])
        features.append(" ".join(three_split(x)))
    if front:
        features = [(feature.split(" ")[-0] + "FRONT!11one!1 ") + feature for feature in features]
    if end:
        features = [feature + (" " + feature.split(" ")[-1] + "END!11one!1") for feature in features]    
    return features
def clean_feature_4split(x, front=False, end=False):
    features = []
    if isinstance(x, list) or isinstance(x, np.ndarray):
        for xi in x:
            features.extend(clean_feature_4split(xi))
    else:
        x = "".join([ c if (c.isalnum() and (not c.isdigit())) else "" for c in x.lower()])
        features.append(" ".join(four_split(x)))
    if front:
        features = [(feature.split(" ")[-0] + "FRONT!11one!1 ") + feature for feature in features]
    if end:
        features = [feature + (" " + feature.split(" ")[-1] + "END!11one!1") for feature in features]    
    return features

def get_my_x_1122split(x, front=False, end=False, matrix=True):
    xa = pd.DataFrame(space_split(x))[0]
    xb = pd.DataFrame(space_split(x))[1]
    x1 = xa + " "
    x2 = xb + " "
    x3 = xa + " "
    x4 = xb + " "
    corpus1 = list(x1.values.flatten())
    corpus2 = list(x2.values.flatten())
    corpus3 = list(x3.values.flatten())
    corpus4 = list(x3.values.flatten())
    corpus_clean1 = clean_feature_1split(corpus1, front=front, end=end)
    corpus_clean2 = clean_feature_1split(corpus2, front=front, end=end)
    corpus_clean3 = clean_feature_2split(corpus3, front=front, end=end)
    corpus_clean4 = clean_feature_2split(corpus4, front=front, end=end)
    corpus_tagged1 = pd.DataFrame([re.sub(" ", "_TAGNUM1!! ", xx) for xx in corpus_clean1])
    corpus_tagged2 = pd.DataFrame([re.sub(" ", "_TAGNUM2!! ", xx) for xx in corpus_clean2])
    corpus_tagged3 = pd.DataFrame([re.sub(" ", "_TAGNUM3!! ", xx) for xx in corpus_clean3])
    corpus_tagged4 = pd.DataFrame([re.sub(" ", "_TAGNUM4!! ", xx) for xx in corpus_clean4])
    corpus_clean = pd.concat([corpus_tagged1, corpus_tagged2, corpus_tagged3, corpus_tagged4], axis = 1).apply(lambda x: ' '.join(x), axis = 1)
    vectorizer = CountVectorizer()
    X = vectorizer.fit(corpus_clean)
    if matrix:
        return X.transform(corpus_clean).toarray()
    else:
        return lambda y: X.transform(clean_feature_2split(y, front=front, end=end)).toarray()
 
def get_my_x_112233nonosplit(x, front=False, end=False, matrix=True):
    xa = pd.DataFrame(space_split(x))[0]
    xb = pd.DataFrame(space_split(x))[1]
    x1 = xa + " "
    x2 = xb + " "
    x3 = xa + " "
    x4 = xb + " "
    x5 = xa + " "
    x6 = xb + " "
    x7 = xa + " "
    x8 = xb + " "
    corpus1 = list(x1.values.flatten())
    corpus2 = list(x2.values.flatten())
    corpus3 = list(x3.values.flatten())
    corpus4 = list(x4.values.flatten())
    corpus5 = list(x5.values.flatten())
    corpus6 = list(x6.values.flatten())
    corpus7 = list(x7.values.flatten())
    corpus8 = list(x8.values.flatten())
    corpus_clean1 = clean_feature_1split(corpus1, front=front, end=end)
    corpus_clean2 = clean_feature_1split(corpus2, front=front, end=end)
    corpus_clean3 = clean_feature_2split(corpus3, front=front, end=end)
    corpus_clean4 = clean_feature_2split(corpus4, front=front, end=end)
    corpus_clean5 = clean_feature_3split(corpus5, front=front, end=end)
    corpus_clean6 = clean_feature_3split(corpus6, front=front, end=end)
    corpus_clean7 = clean_feature_nosplit(corpus7, front=front, end=end)
    corpus_clean8 = clean_feature_nosplit(corpus8, front=front, end=end)
    corpus_tagged1 = pd.DataFrame([re.sub(" ", "_TAGNUM1!! ", xx) for xx in corpus_clean1])
    corpus_tagged2 = pd.DataFrame([re.sub(" ", "_TAGNUM2!! ", xx) for xx in corpus_clean2])
    corpus_tagged3 = pd.DataFrame([re.sub(" ", "_TAGNUM3!! ", xx) for xx in corpus_clean3])
    corpus_tagged4 = pd.DataFrame([re.sub(" ", "_TAGNUM4!! ", xx) for xx in corpus_clean4])
    corpus_tagged5 = pd.DataFrame([re.sub(" ", "_TAGNUM5!! ", xx) for xx in corpus_clean5])
    corpus_tagged6 = pd.DataFrame([re.sub(" ", "_TAGNUM6!! ", xx) for xx in corpus_clean6])
    corpus_tagged7 = pd.DataFrame([re.sub(" ", "_TAGNUM7!! ", xx) for xx in corpus_clean7])
    corpus_tagged8 = pd.DataFrame([re.sub(" ", "_TAGNUM8!! ", xx) for xx in corpus_clean8])
    corpus_clean = pd.concat([corpus_tagged1, corpus_tagged2, corpus_tagged3, corpus_tagged4, corpus_tagged5, corpus_tagged6, corpus_tagged7, corpus_tagged8], axis = 1).apply(lambda x: ' '.join(x), axis = 1)
    vectorizer = CountVectorizer()
    X = vectorizer.fit(corpus_clean)
    if matrix:
        return X.transform(corpus_clean).toarray()
    else:
        return lambda y: X.transform(clean_feature_2split(y, front=front, end=end)).toarray()
          
 
def get_my_x_112233split(x, front=False, end=False, matrix=True):
    xa = pd.DataFrame(space_split(x))[0]
    xb = pd.DataFrame(space_split(x))[1]
    x1 = xa + " "
    x2 = xb + " "
    x3 = xa + " "
    x4 = xb + " "
    x5 = xa + " "
    x6 = xb + " "
    corpus1 = list(x1.values.flatten())
    corpus2 = list(x2.values.flatten())
    corpus3 = list(x3.values.flatten())
    corpus4 = list(x4.values.flatten())
    corpus5 = list(x5.values.flatten())
    corpus6 = list(x6.values.flatten())
    corpus_clean1 = clean_feature_1split(corpus1, front=front, end=end)
    corpus_clean2 = clean_feature_1split(corpus2, front=front, end=end)
    corpus_clean3 = clean_feature_2split(corpus3, front=front, end=end)
    corpus_clean4 = clean_feature_2split(corpus4, front=front, end=end)
    corpus_clean5 = clean_feature_3split(corpus5, front=front, end=end)
    corpus_clean6 = clean_feature_3split(corpus6, front=front, end=end)
    corpus_tagged1 = pd.DataFrame([re.sub(" ", "_TAGNUM1!! ", xx) for xx in corpus_clean1])
    corpus_tagged2 = pd.DataFrame([re.sub(" ", "_TAGNUM2!! ", xx) for xx in corpus_clean2])
    corpus_tagged3 = pd.DataFrame([re.sub(" ", "_TAGNUM3!! ", xx) for xx in corpus_clean3])
    corpus_tagged4 = pd.DataFrame([re.sub(" ", "_TAGNUM4!! ", xx) for xx in corpus_clean4])
    corpus_tagged5 = pd.DataFrame([re.sub(" ", "_TAGNUM5!! ", xx) for xx in corpus_clean5])
    corpus_tagged6 = pd.DataFrame([re.sub(" ", "_TAGNUM6!! ", xx) for xx in corpus_clean6])
    corpus_clean = pd.concat([corpus_tagged1, corpus_tagged2, corpus_tagged3, corpus_tagged4, corpus_tagged5, corpus_tagged6], axis = 1).apply(lambda x: ' '.join(x), axis = 1)
    vectorizer = CountVectorizer()
    X = vectorizer.fit(corpus_clean)
    if matrix:
        return X.transform(corpus_clean).toarray()
    else:
        return lambda y: X.transform(clean_feature_2split(y, front=front, end=end)).toarray()
          
def get_my_x_123nosplit(x, front=False, end=False, matrix=True):
    xa = pd.DataFrame(space_split(x))[0]
    xb = pd.DataFrame(space_split(x))[1]
    x1 = xa + " "
    x2 = xa + " "
    x3 = xa + " "
    x4 = xa + " "
    corpus1 = list(x1.values.flatten())
    corpus2 = list(x2.values.flatten())
    corpus3 = list(x3.values.flatten())
    corpus4 = list(x4.values.flatten())
    corpus_clean1 = clean_feature_1split(corpus1, front=front, end=end)
    corpus_clean2 = clean_feature_2split(corpus2, front=front, end=end)
    corpus_clean3 = clean_feature_3split(corpus3, front=front, end=end)
    corpus_clean4 = clean_feature_nosplit(corpus4, front=front, end=end)
    corpus_tagged1 = pd.DataFrame([re.sub(" ", "_TAGNUM1!! ", xx) for xx in corpus_clean1])
    corpus_tagged2 = pd.DataFrame([re.sub(" ", "_TAGNUM2!! ", xx) for xx in corpus_clean2])
    corpus_tagged3 = pd.DataFrame([re.sub(" ", "_TAGNUM3!! ", xx) for xx in corpus_clean3])
    corpus_tagged4 = pd.DataFrame([re.sub(" ", "_TAGNUM4!! ", xx) for xx in corpus_clean4])
    corpus_clean = pd.concat([corpus_tagged1, corpus_tagged2, corpus_tagged3, corpus_tagged4], axis = 1).apply(lambda x: ' '.join(x), axis = 1)
    vectorizer = CountVectorizer()
    X = vectorizer.fit(corpus_clean)
    if matrix:
        return X.transform(corpus_clean).toarray()
    else:
        return lambda y: X.transform(clean_feature_2split(y, front=front, end=end)).toarray()
        
def get_my_x_123split(x, front=False, end=False, matrix=True):
    xa = pd.DataFrame(space_split(x))[0]
    xb = pd.DataFrame(space_split(x))[1]
    x1 = xa + " "
    x2 = xa + " "
    x3 = xa + " "
    corpus1 = list(x1.values.flatten())
    corpus2 = list(x2.values.flatten())
    corpus3 = list(x3.values.flatten())
    corpus_clean1 = clean_feature_1split(corpus1, front=front, end=end)
    corpus_clean2 = clean_feature_2split(corpus2, front=front, end=end)
    corpus_clean3 = clean_feature_3split(corpus3, front=front, end=end)
    corpus_tagged1 = pd.DataFrame([re.sub(" ", "_TAGNUM1!! ", xx) for xx in corpus_clean1])
    corpus_tagged2 = pd.DataFrame([re.sub(" ", "_TAGNUM2!! ", xx) for xx in corpus_clean2])
    corpus_tagged3 = pd.DataFrame([re.sub(" ", "_TAGNUM3!! ", xx) for xx in corpus_clean3])
    corpus_clean = pd.concat([corpus_tagged1, corpus_tagged2, corpus_tagged3], axis = 1).apply(lambda x: ' '.join(x), axis = 1)
    vectorizer = CountVectorizer()
    X = vectorizer.fit(corpus_clean)
    if matrix:
        return X.transform(corpus_clean).toarray()
    else:
        return lambda y: X.transform(clean_feature_2split(y, front=front, end=end)).toarray()
          
def get_my_x_1234split(x, front=False, end=False, matrix=True):
    xa = pd.DataFrame(space_split(x))[0]
    xb = pd.DataFrame(space_split(x))[1]
    x1 = xa + " "
    x2 = xa + " "
    x3 = xa + " "
    x4 = xa + " "
    corpus1 = list(x1.values.flatten())
    corpus2 = list(x2.values.flatten())
    corpus3 = list(x3.values.flatten())
    corpus4 = list(x4.values.flatten())
    corpus_clean1 = clean_feature_1split(corpus1, front=front, end=end)
    corpus_clean2 = clean_feature_2split(corpus2, front=front, end=end)
    corpus_clean3 = clean_feature_3split(corpus3, front=front, end=end)
    corpus_clean4 = clean_feature_4split(corpus4, front=front, end=end)
    corpus_tagged1 = pd.DataFrame([re.sub(" ", "_TAGNUM1!! ", xx) for xx in corpus_clean1])
    corpus_tagged2 = pd.DataFrame([re.sub(" ", "_TAGNUM2!! ", xx) for xx in corpus_clean2])
    corpus_tagged3 = pd.DataFrame([re.sub(" ", "_TAGNUM3!! ", xx) for xx in corpus_clean3])
    corpus_tagged4 = pd.DataFrame([re.sub(" ", "_TAGNUM4!! ", xx) for xx in corpus_clean4])
    corpus_clean = pd.concat([corpus_tagged1, corpus_tagged2, corpus_tagged3, corpus_tagged4], axis = 1).apply(lambda x: ' '.join(x), axis = 1)
    vectorizer = CountVectorizer()
    X = vectorizer.fit(corpus_clean)
    if matrix:
        return X.transform(corpus_clean).toarray()
    else:
        return lambda y: X.transform(clean_feature_2split(y, front=front, end=end)).toarray()
    
# nrg4 = nrg = pd.read_csv("/Users/srhoads/GitHub/old/name_race_gender/nrg4.csv",  encoding='latin-1')
# nrg_nona = nrg4.dropna(subset=["firstname3", "lastname3", "race_binary"])
      
# xformer = get_my_x_1122split(nrg_nona['name12_first_space_last'], front=True, end=True, matrix = False)
def predict_race(x, model, xformer):
    prediction = float(model.predict(xformer(x)))
    if prediction == 0.0:
        return "Minority"
    elif prediction == 1.0:
        return "Non-Minority"
    else:
        return "Who Knows"    

# nrg4 = nrg = pd.read_csv("/Users/srhoads/Documents/GitHub/name_race_gender/nrg4.csv",  encoding='latin-1')
# nrg_nona = nrg4.dropna(subset=["firstname3", "gender_num"])
    
# xformer = get_my_x_1122split(nrg_nona['name12_first_space_last'], front=True, end=True, matrix = False)
def predict_gender(x, model, xformer):
    prediction = float(model.predict(xformer(x)))
    if prediction == 0.0:
        return "Female"
    elif prediction == 1.0:
        return "Male"
    else:
        return "Ambiguous or Androgynous" 



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