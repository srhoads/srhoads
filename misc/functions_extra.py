
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


