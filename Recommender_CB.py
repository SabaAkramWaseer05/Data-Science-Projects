import pandas as pd
import numpy as np
import scipy
from scipy import sparse as sp
import sklearn
import sklearn.preprocessing as pp
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import linear_kernel


class Recommender_CB:
      
    def __init__(self, profile_type='plot'):
        self.profile_type = profile_type
        
    def get_item_titles(self, item_ids):
        return [self.items_meta[self.items_meta['item_id'] == id]['title'].item() for id in item_ids] 

    def build_item_contents(self):
        vectorizer = TfidfVectorizer(stop_words='english')  # Define a TF-IDF Vectorizer that removes all english stop words (e.g., 'the', 'a')

        ### BEGIN SOLUTION
        self.plot_tfidf = vectorizer.fit_transform(self.items_meta['plot'])
        self.plot_tfidf_tokens = vectorizer.get_feature_names_out()
        self.meta_tfidf = vectorizer.fit_transform(self.items_meta['metadata'])
        self.meta_tfidf_tokens = vectorizer.get_feature_names_out()
        self.set_content_type()
        ### END SOLUTION

    def set_content_type(self):
        if self.profile_type == 'plot':
            self.tfidf = self.plot_tfidf
            self.tfidf_tokens = self.plot_tfidf_tokens
        else:
            self.tfidf = self.meta_tfidf
            self.tfidf_tokens = self.meta_tfidf_tokens 
    
    def get_item_vectors(self, item_ids):
        i_ids = [self.i_id_to_item_id.index(item_id) for item_id in item_ids]
        item_vector = self.tfidf[i_ids]
        return item_vector 
    
    def get_user_profile(self, user_id, ratings):
        item_ids_rated_by_user_id = np.array(ratings.loc[ratings['user'] == user_id]['item'])
        user_ratings = np.array(ratings.loc[ratings['user'] == user_id]['rating'])

        ### BEGIN SOLUTION
        item_vectors = self.tfidf[[self.i_id_to_item_id.index(item_id) for item_id in item_ids_rated_by_user_id]]
        user_profile = sp.csr_matrix(user_ratings).T.multiply(item_vectors).sum(axis=0)
        # Convert to CSR format explicitly to ensure compatibility with sklearn's normalize
        user_profile = sp.csr_matrix(user_profile)
        user_profile = pp.normalize(user_profile)
        ### END SOLUTION
        return user_profile    

    def build_user_profiles(self):
        positive_ratings = self.ratings[self.ratings['rating'] > 3]
        self.user_profiles = {}
        for user_id in positive_ratings['user'].unique():
            self.user_profiles[user_id] = self.get_user_profile(user_id, positive_ratings)
    
    def recommend(self, user_id, from_item_ids=None, topN=20):
        item_ids_rated_by_user_id = self.ratings.loc[self.ratings['user'] == user_id]['item'].tolist()

        if from_item_ids is None:
            from_item_ids = self.item_ids
                
        ### BEGIN SOLUTION
        user_profile = self.user_profiles[user_id]
        sims = linear_kernel(user_profile, self.tfidf).flatten()
        sorted_indices = np.argsort(sims)[::-1]

        recommendations = []
        for i in sorted_indices:
            item_id = self.i_id_to_item_id[i]
            if item_id not in item_ids_rated_by_user_id and (from_item_ids is None or item_id in from_item_ids):
                recommendations.append(item_id)
                if len(recommendations) == topN:
                    break
        return recommendations
        ### END SOLUTION
    
    def in_rated(self, user_id, recommendations):
        item_ids_rated_by_user_id = self.ratings.loc[self.ratings['user'] == user_id]['item'].tolist()
        intersection = list(set(item_ids_rated_by_user_id) & set(recommendations))
        return len(intersection) > 0
    
    def get_recommendation_score_dict(self, user_id, recommendations):
        user_profile = self.user_profiles[user_id]
        item_vectors = self.get_item_vectors(recommendations)
        scores = linear_kernel(item_vectors, user_profile).flatten()
        score_dict = dict(zip(recommendations, scores))
        return score_dict
  
    def get_recommendation_scores(self, user_id, recommendations):
        score_dict = self.get_recommendation_score_dict(user_id, recommendations)
        scores = [score_dict[item] for item in recommendations]
        return scores
  
    def build_model(self, ratings, items_meta):
        self.ratings = ratings
        self.items_meta = items_meta
        
        # user_id and item_id are external ids; i_id is internal id
        self.item_ids = self.ratings.item.unique()
        self.item_ids.sort()
        
        self.user_ids = self.ratings.user.unique()
        self.user_ids.sort()
        
        self.i_id_to_item_id = self.items_meta['item_id'].tolist()
        
        self.build_item_contents()
        self.build_user_profiles()
