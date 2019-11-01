##******************************************************************************
###### ways of importing py files/reloading them #### import functions.py file to get ur defs (works for reimporting too)
##----------------------------------------------------------
# functions = (open('functions.py').read()); exec(functions.read())
### or full path... ##----------------------------------------------------------
# functions = open('Users/srhoads/GitHub/namegender/functions.py'); exec(functions.read())
##******************************************************************************

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
# IMPORTS!

import matplotlib.pyplot as plt
from sklearn import model_selection
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, BaggingClassifier
from sklearn.naive_bayes import GaussianNB, MultinomialNB
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.svm import SVC
from sklearn.multiclass import OneVsRestClassifier
from tpot import TPOTClassifier
from imblearn.ensemble import EasyEnsembleClassifier

#=================================================================================================================
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
#                     system('python3 -m pip install ' + package + " / y")
                    system('python3 -m pip install ' + package + " / y")
            if via == "conda":
                system('conda install ' + package)
        except:
            print("Sorry kid... can't install this guy")
            
    


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

# def dim(x):
#     import pandas as pd
#     if isinstance(x, pd.DataFrame): return x.shape
#     else: return len(x)

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
    if len(x) < n: return(x[0:len(x)])
    else: return x[0:n]

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


# # read feather files from a directory path
# #----------------------------------------------------------
# def readfrs_fromdir(dir = "data"):
#     files = list_files(dir)
#     data = readfrs(files)
#     return data

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

def source(pyfile="functions.py"):
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



## OTHER PPL'S FXNS FROM THE INTERNET:
##******************************************************************************# lemmatizing text--making consistent tenses and word forms like run and running
def lemmatize_stemming(text):
    return stemmer.stem(WordNetLemmatizer().lemmatize(text, pos='v'))

# not my own function--uses it's own thing to remove stopwords
def preprocess(text):
    result = []
    for token in gensim.utils.simple_preprocess(text):
        if token not in gensim.parsing.preprocessing.STOPWORDS and len(token) > 3:
            result.append(lemmatize_stemming(token))
    return result


















print("If u see this message, CONGRATS! You imported the most updated version of the functions file WEE")






# from gensim.corpora.dictionary import Dictionary
# from gensim.models.ldamodel import LdaModel
# from gensim.models import Word2Vec
# from sklearn.feature_extraction.text import CountVectorizer
# import collections
# import pandas as pd
# import matplotlib.pyplot as plt


# import sys
# sys.path.append('/Users/srhoads/GitHub/namegender')
# from functions_test import *

# flatten a list--like dplyr::combine()
def flatten(d):
     v = [[i] if not isinstance(i, list) else flatten(i) for i in d]
     return [i for b in v for i in b]
     
# head like in R
def head(x, n=5):
    if len(x) < n:
        return(x[0:len(x)])
    else:
        return x[0:n]

# list files from dir path like list.files() in r
def list_files(path = "data", pattern = ".f"):
    from os import listdir
    from os.path import isfile, join
    files = [path + "/" + f for f in listdir(path) if isfile(join(path, f))]
    files = [file for file in files if pattern in file]
    return files

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

# turn a character variable/feature into numeric (ie: gender to binary 0/1)
def char_to_num(vec):
    return((vec.astype("category")).cat.codes)

# not really sure... but split by some # of chars?
def split_n(s, n=2):
    return [s[idx:idx + n] for idx, val in enumerate(s) if idx%n == 0]

# split n by 2 characters
def two_split(s):
    return split_n(s, 2)

# # split by n # of characters (like two_split but ambiguous)
# def tokenize_by_n(s, n=2):
#     return split_n(s, n)

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

# flatten dataframe values i guess?
def dflat(x): # x = pd.DataFrame
    if not isinstance(x, list):
        return(list(x.values.flatten()))
    else:
        return(x)

def unlist(listofvecsofstrings):
    v = [[i] if not isinstance(i, list) else flatten(i) for i in listofvecsofstrings]
    return [i for b in v for i in b]


# return just a formatted array of features
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

def get_my_x(vec=None, df=None, xname = "name",max_features=None,front=True, end=True, firstlastonly=False, endeach = True, fronteach = True, matrix=False, feature_names=False, justfit=False, sample=None, justvectorizer=False):
    if sample is not None and df is not None:
        df=df.sample(sample)
    if df is not None:
        vec = df[xname]
    x = dflat(vec)
    corpus = clean_feature(x, front=front, end=end, firstlastonly=firstlastonly)
    from sklearn.feature_extraction.text import CountVectorizer
    vectorizer = CountVectorizer(max_features=max_features)
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

def getxy(df, x = "name", y = "gender", sample = None, dropna = True, both = True, justx = False, justy = False, feature_names=False, justfit=False,justvectorizer=False):
    if sample is not None:
        df = df.sample(sample)
    if dropna:
        df = (df[[x, y]]).dropna()
    if justfit:
        return get_my_x(vec=df[x],justfit=True, justvectorizer=justvectorizer)
    if justx and not justy:
        return (get_my_x(df[x], feature_names=feature_names, justfit=justfit))["X"]
    if justy and not justx:
        return (df[y].astype("category")).cat.codes
    if justx and justy and not justfit:
        result = {
            'x': (get_my_x(df[x], feature_names=feature_names, justfit=justfit))["X"], 
            'y': (df[y].astype("category")).cat.codes
        }
        print("getxy's x & y dict keys are:", result.keys())
        return result
    else:
        result = {
            'x': (get_my_x(df[x], feature_names=feature_names, justfit=justfit))["X"], 
            'y': (df[y].astype("category")).cat.codes
        }
        print("getxy's x & y dict keys are:", result.keys())
        return result

# feather path dir or filenames input to get x and y for model
def frs_toxy(path=None, filenames=None, data=None, x = "name", y = "gender", sample = None, dropna = True, both = True, justx = False, justy = False,feature_names=False, justfit=False):
    if filenames is not None:
        data = readfrs(filenames)
    elif data is not None:
        data = data
    else:
        data = readfrs_fromdir(path)
    result = getxy(data, x = x, y = y, sample = sample, dropna = dropna, both = both, justx = justx, justy = justy, feature_names=feature_names, justfit=justfit)
    return result

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

# read feather files and get test-train output
def frs_toxy_testtrain(path=None,filenames=None, x = "name", y = "gender", sample = None, dropna = True, both = True, justx = False, justy = False):
    xy = frs_toxy(path=path,filenames=filenames, x = x, y = y, sample = sample, dropna = dropna, both = both, justx = justx, justy = justy)
    return testtrain(xy = xy)

def source(pyfile="functions.py"):
    sourcedfile = open(pyfile)
    exec(sourcedfile.read())

# LDA output printed readably
def print_lda_output(output):
      for i in output:
            print(i, "n\n")
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


## practice+messy

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

# def checkup():
#     print("working?")




##### OTHER PPL'S FXNS FROM THE INTERNET:
# lemmatizing text--making consistent tenses and word forms like run and running
def lemmatize_stemming(text):
    return stemmer.stem(WordNetLemmatizer().lemmatize(text, pos='v'))

# not my own function--uses it's own thing to remove stopwords
def preprocess(text):
    result = []
    for token in gensim.utils.simple_preprocess(text):
        if token not in gensim.parsing.preprocessing.STOPWORDS and len(token) > 3:
            result.append(lemmatize_stemming(token))
    return result




# from gensim.corpora.dictionary import Dictionary
# from gensim.models.ldamodel import LdaModel


# import gensim
# from gensim.utils import simple_preprocess
# from gensim.parsing.preprocessing import STOPWORDS
# from nltk.stem import WordNetLemmatizer, SnowballStemmer
# from nltk.stem.porter import *
# import numpy as np






















# print("If u see this message, CONGRATS! You imported the most updated version of the functions file WEE")























print("below=way f-ing old ugh")

#nrg4 = nrg.sample(3000)
#nrg4 = nrg
import numpy as np
import pandas as pd
import patsy as ps
import re
import time
from time import time
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.metrics import confusion_matrix
from sklearn.feature_selection import VarianceThreshold
import pickle
import mglearn
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.datasets import make_classification
from sklearn.model_selection import cross_val_score
from sklearn.tree import DecisionTreeClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.decomposition import LatentDirichletAllocation
from xgboost import XGBClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from scipy.stats import randint as sp_randint
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import RandomizedSearchCV
from sklearn.datasets import load_digits
from sklearn.ensemble import RandomForestClassifier

# WRITING FILES
import pickle
#pickle.dump(model, open('/Users/srhoads/Documents/GitHub/name_race_gender/model/gendermodel_97.6%.sav', 'wb'))
import feather
#feather.write_dataframe(pd.DataFrame(X), "/Users/srhoads/Documents/GitHub/name_race_gender/model/X_gendermodel_97.6%.feather")
#feather.write_dataframe(pd.DataFrame(y), "/Users/srhoads/Documents/GitHub/name_race_gender/model/y_gendermodel_97.6%.feather")
#feather.write_dataframe(pd.DataFrame(nrg_nona['name12_first_space_last']), "/Users/srhoads/Documents/GitHub/name_race_gender/model/corpus_gendermodel_97.6%.feather")          
# LOADING FILES
import pickle
import feather
# load pickle: model = pickle.load(open('/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.sav', 'rb'))
# load feather: X = feather.read_dataframe("/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.feather")



def space_split(s):
    splits = []
    for x in s:
        splits.append(x.split(" "))
    return splits

def no_split(s):
    return [s[idx:idx + 25] for idx, val in enumerate(s) if idx%25 == 0]
def one_split(s):
    return [s[idx:idx + 1] for idx, val in enumerate(s) if idx%1 == 0]
def two_split(s):
    return [s[idx:idx + 2] for idx, val in enumerate(s) if idx%2 == 0]
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
def predict_race(x):
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
def predict_gender(x):
    prediction = float(model.predict(xformer(x)))
    if prediction == 0.0:
        return "Female"
    elif prediction == 1.0:
        return "Male"
    else:
        return "Ambiguous or Androgynous" 
  
# LOADING FILES
import pickle
import feather
# load pickle: model = pickle.load(open('/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.sav', 'rb'))
# load feather: X = feather.read_dataframe("/Users/srhoads/Documents/GitHub/name_race_gender/model/FILE.feather")

# WRITING FILES
import pickle
#pickle.dump(model, open('/Users/srhoads/Documents/GitHub/name_race_gender/model/gendermodel_97.6%.sav', 'wb'))
import feather
#feather.write_dataframe(pd.DataFrame(X), "/Users/srhoads/Documents/GitHub/name_race_gender/model/X_gendermodel_97.6%.feather")
#feather.write_dataframe(pd.DataFrame(y), "/Users/srhoads/Documents/GitHub/name_race_gender/model/y_gendermodel_97.6%.feather")
#feather.write_dataframe(pd.DataFrame(nrg_nona['name12_first_space_last']), "/Users/srhoads/Documents/GitHub/name_race_gender/model/corpus_gendermodel_97.6%.feather")

