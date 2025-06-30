import pandas as pd
import numpy as np
from scipy import sparse as sp
from scipy.sparse.linalg import norm
import sklearn.preprocessing as pp


class Recommender_CF_UU:
    UU = {}  # user-user similarities; constructed lazily

    def create_Ratings_Matrix(self):
        self.item_ids = self.ratings['item'].unique()
        self.item_ids.sort()
        self.user_ids = self.ratings['user'].unique()
        self.user_ids.sort()
        self.m = self.user_ids.size

        # user_id and item_id are external ids; u_id and i_id are internal ids
        self.item_id_to_i_id = dict(zip(self.item_ids, range(0, self.item_ids.size)))
        self.i_id_to_item_id = dict(zip(range(0, self.item_ids.size), self.item_ids))

        self.user_id_to_u_id = dict(zip(self.user_ids, range(0, self.user_ids.size)))
        self.u_id_to_user_id = dict(zip(range(0, self.user_ids.size), self.user_ids))

        self.R = sp.csr_matrix((self.ratings['rating'], (self.ratings['user'].map(self.user_id_to_u_id), self.ratings['item'].map(self.item_id_to_i_id))))
        self.R_dok = self.R.todok()

    def compute_user_avgs(self):
        user_sums = self.R.sum(axis=1).A1  # matrix converted to 1-D array via .A1
        self.user_cnts = (self.R != 0).sum(axis=1).A1
        self.user_avgs = user_sums / self.user_cnts

    def compute_pairwise_user_similarity(self, u_id, v_id):
        u = self.R[u_id, :].copy()
        v = self.R[v_id, :].copy()

        ### BEGIN SOLUTION
        common_items = u.multiply(v).nonzero()[1]
        if len(common_items) == 0:
            return 0

        # Convert sparse matrix slices to dense arrays for subtraction
        u_common = u[0, common_items].toarray().flatten()
        v_common = v[0, common_items].toarray().flatten()
        u_centered = u_common - self.user_avgs[u_id]
        v_centered = v_common - self.user_avgs[v_id]

        numerator = np.dot(u_centered, v_centered)
        denominator = np.linalg.norm(u_centered) * np.linalg.norm(v_centered)

        if denominator == 0:
            return 0

        similarity = numerator / denominator
        ### END SOLUTION
        return similarity

    def compute_user_similarities(self, u_id):
        if u_id in self.UU.keys():  # persist
            return self.UU[u_id]

        uU = np.empty((self.m,))

        ########## BEGIN HERE ##########
        # Compute similarities for all users
        ########## END HERE ##########

        # Add the version without the for loop here if you have done it
        # """
        ########## BEGIN BONUS ##########
        ######### END BONUS ##########

        self.UU[u_id] = uU

        return self.UU[u_id]

    def create_user_neighborhood(self, u_id, i_id):
        nh = {}  # the neighborhood dict with (user id: similarity) entries
        # nh should not contain u_id and only include users that have rated i_id; there should be at most k neighbors
        self.compute_user_similarities(u_id)
        uU = self.UU[u_id].copy()

        uU_copy = uU.copy()  # so that we can modify it, but also keep the original

        ########## BEGIN HERE ##########
        rated_by_v = self.R[:, i_id].nonzero()[0]
        for v_id in rated_by_v:
            if v_id == u_id:
                continue
            sim = self.compute_pairwise_user_similarity(u_id, v_id)
            if self.with_abs_sim:
                sim = abs(sim)
            nh[v_id] = sim

        nh = dict(sorted(nh.items(), key=lambda item: item[1], reverse=True)[:self.k])
        ########## END HERE ##########

        return nh

    def predict_rating(self, u_id, i_id):
        nh = self.create_user_neighborhood(u_id, i_id)

        neighborhood_weighted_avg = 0.

        ########## BEGIN HERE ##########
        sum_scores = 0
        sum_weights = 0
        for v_id, sim in nh.items():
            if self.with_deviations:
                sum_scores += sim * (self.R[v_id, i_id] - self.user_avgs[v_id])
            else:
                sum_scores += sim * self.R[v_id, i_id]
            sum_weights += sim
        ########## END HERE ##########

        if sum_weights != 0:  # avoid division by zero
            neighborhood_weighted_avg = sum_scores / sum_weights

        if self.with_deviations:
            prediction = self.user_avgs[u_id] + neighborhood_weighted_avg
        else:
            prediction = neighborhood_weighted_avg

        return prediction

    def __init__(self, with_abs_sim=True, with_deviations=True, k=50):
        self.with_abs_sim = with_abs_sim
        self.with_deviations = with_deviations
        self.k = k

    def build_model(self, ratings, items_meta=None):
        self.ratings = ratings

        self.create_Ratings_Matrix()
        self.compute_user_avgs()

    def recommend(self, user_id, from_item_ids=None, topN=20):
        item_ids_rated_by_user_id = self.ratings[self.ratings['user'] == user_id]['item'].tolist()

        u_id = self.user_id_to_u_id[user_id]

        recommendations = []

        if from_item_ids is None:
            from_item_ids = self.item_ids

        for item_id in from_item_ids:
            if item_id in item_ids_rated_by_user_id:
                continue
            i_id = self.item_id_to_i_id[item_id]
            rating = self.predict_rating(u_id, i_id)
            recommendations.append((item_id, rating))

        recommendations = sorted(recommendations, key=lambda x: -x[1])[:topN]

        return [item_id for item_id, rating in recommendations]
