import pandas as pd
import numpy as np
import math
from scipy import sparse as sp
from scipy.sparse.linalg import norm
import sklearn.preprocessing as pp
from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.utils.validation import check_X_y
from sklearn.utils import shuffle
from sklearn.preprocessing import LabelBinarizer
from sklearn.metrics import mean_squared_error


def get_one_hot(targets):
    lb = pp.LabelBinarizer(sparse_output=True)
    lb.fit(targets.reshape(-1))
    return lb.transform(targets)


def to_Xy_format(R):
    n_users = R.shape[0]
    n_items = R.shape[1]

    users, items = R.nonzero()

    Xu = get_one_hot(users)
    Xi = get_one_hot(items)

    R = sp.csr_matrix(R)
    y = R.data
    X = sp.hstack([Xu, Xi])

    return X, y, n_users, n_items


def to_R_format(X, y, n_users, n_items):
    Xu = X.tocsc()[:, :n_users]
    Xi = X.tocsc()[:, n_users:]

    Xu = Xu.tocsr()
    Xi = Xi.tocsr()

    R_rec = sp.coo_matrix((y, (Xu.indices, Xi.indices)), shape=(n_users, n_items))

    return R_rec.tocsr()


def to_UI_arrays(X, n_users, n_items):
    Xu = X.tocsc()[:, :n_users]
    Xi = X.tocsc()[:, n_users:]

    U = Xu.argmax(axis=1).A1
    I = Xi.argmax(axis=1).A1

    return U, I


def iterate_minibatches(inputs, targets, batchsize, shuffle=False):
    assert inputs.shape[0] == targets.shape[0]
    if shuffle:
        indices = np.arange(inputs.shape[0])
        np.random.shuffle(indices)
    for start_idx in range(0, inputs.shape[0], batchsize):
        end_idx = min(start_idx + batchsize, inputs.shape[0])
        if shuffle:
            excerpt = indices[start_idx:end_idx]
        else:
            excerpt = slice(start_idx, end_idx)
        yield inputs[excerpt], targets[excerpt]


VERBOSE = False
DEBUG = True


class Recommender_MF(BaseEstimator, RegressorMixin):

    def create_Ratings_Matrix(self):
        self.item_ids = self.ratings.item.unique()
        self.item_ids.sort()
        self.user_ids = self.ratings.user.unique()
        self.user_ids.sort()
        self.m = self.user_ids.size

        self.item_id_to_i_id = dict(zip(self.item_ids, range(0, self.item_ids.size)))
        self.i_id_to_item_id = dict(zip(range(0, self.item_ids.size), self.item_ids))

        self.user_id_to_u_id = dict(zip(self.user_ids, range(0, self.user_ids.size)))
        self.u_id_to_user_id = dict(zip(range(0, self.user_ids.size), self.user_ids))

        self.R = sp.csr_matrix((self.ratings.rating, (self.ratings.user.map(self.user_id_to_u_id), self.ratings.item.map(self.item_id_to_i_id))))

    def __init__(self, k=5, eta=0.002, lam=0., n_epochs=5, s_batch=1, w_average=True, w_biases=True, rnd_mean=0, rnd_std=0.1):
        self.k = k
        self.eta = eta
        self.lam = lam
        self.n_epochs = n_epochs
        self.s_batch = s_batch
        self.w_average = w_average
        self.w_biases = w_biases
        self.rnd_mean = rnd_mean
        self.rnd_std = rnd_std

    def fit_init(self, X, y, n_users, n_items):
        X, y = check_X_y(X, y, accept_sparse=True)

        self.n_users_ = n_users
        self.n_items_ = n_items
        self.n_ratings_ = X.shape[0]

        self.X_ = X
        self.y_ = y

        if DEBUG:
            np.random.seed(42)

        self.P_ = np.random.normal(self.rnd_mean, self.rnd_std, (self.n_users_, self.k))
        self.Q_ = np.random.normal(self.rnd_mean, self.rnd_std, (self.n_items_, self.k))

        if self.w_biases:
            self.bu_ = np.zeros(self.n_users_)
            self.bi_ = np.zeros(self.n_items_)

        if self.w_average:
            self.mu_ = np.mean(y)
        else:
            self.mu_ = 0

        self.X_, self.y_ = shuffle(self.X_, self.y_)

        return self

    def fit_sgd(self):
        U, I = to_UI_arrays(self.X_, self.n_users_, self.n_items_)

        if VERBOSE:
            print("start of training")

        for epoch in range(self.n_epochs):
            epoch_loss = 0.

            for index in range(self.y_.shape[0]):
                u = U[index]
                i = I[index]
                r_ui = self.y_[index]

                err = r_ui - (self.P_[u].dot(self.Q_[i]) + self.mu_ + self.bu_[u] + self.bi_[i])
                self.P_[u] += self.eta * (err * self.Q_[i] - self.lam * self.P_[u])
                self.Q_[i] += self.eta * (err * self.P_[u] - self.lam * self.Q_[i])

                if self.w_biases:
                    self.bu_[u] += self.eta * (err - self.lam * self.bu_[u])
                    self.bi_[i] += self.eta * (err - self.lam * self.bi_[i])

                epoch_loss += err * err

            epoch_loss /= self.n_ratings_
            if VERBOSE:
                print(f'epoch {epoch} loss = {epoch_loss:.4f}')

        return self

    def fit_mgd(self):
        if VERBOSE:
            print("start training")

        for epoch in range(self.n_epochs):
            epoch_loss = 0.

            for XB, yB in iterate_minibatches(self.X_, self.y_, self.s_batch):
                # Vectorized computation for minibatch gradient descent can be implemented here
                pass

            epoch_loss /= self.n_ratings_

            if VERBOSE:
                print(f'epoch {epoch} loss = {epoch_loss:.4f}')

        return self

    def fit(self, X, y, n_users, n_items):
        self.fit_init(X, y, n_users, n_items)

        if VERBOSE:
            print(self.get_params())

        if self.s_batch == 1:
            self.fit_sgd()
        else:
            self.fit_mgd()

        return self

    def predict(self, X, y=None):
        try:
            getattr(self, "n_users_")
        except AttributeError:
            raise RuntimeError("You must first train before predicting!")

        Xu = X.tocsc()[:, :self.n_users_]
        Xi = X.tocsc()[:, self.n_users_:]

        P_U = Xu.tocsr().dot(self.P_)
        Q_I = Xi.tocsr().dot(self.Q_)

        y_pred = (P_U * Q_I).sum(axis=1)

        if self.w_average:
            y_pred += self.mu_

        if self.w_biases:
            bu_U = Xu.tocsr().dot(self.bu_)
            bi_I = Xi.tocsr().dot(self.bi_)

            y_pred += bu_U + bi_I

        if y is not None:
            mse = mean_squared_error(y_pred, y)
            rmse = math.sqrt(mse)
            print(f'RMSE {rmse:.4f}')

        return y_pred

    def computeRMSE(self):
        y_pred = self.predict(self.X_)
        mse = mean_squared_error(y_pred, self.y_)
        rmse = math.sqrt(mse)
        return rmse, mse

    def score(self, X, y=None):
        if y is None:
            rmse, mse = self.computeRMSE()
            return -rmse
        else:
            y_pred = self.predict(X)
            mse = mean_squared_error(y_pred, y)
            return -math.sqrt(mse)

    def build_model(self, ratings, items_meta=None):
        self.ratings = ratings
        self.create_Ratings_Matrix()

        X, y, n_users, n_items = to_Xy_format(self.R)
        self.fit(X, y, n_users, n_items)

    def recommend(self, user_id, from_item_ids=None, topN=20):
        item_ids_rated_by_user_id = self.ratings[self.ratings['user'] == user_id]['item'].tolist()

        u_id = self.user_id_to_u_id[user_id]

        one_hot = np.zeros(self.n_users_)
        one_hot[u_id] = 1
        one_hot.reshape(1, -1)
        one_hot = sp.csr_matrix(one_hot)
        Xu = sp.csr_matrix(np.ones([self.n_items_, 1])) * one_hot

        Xi = sp.diags(np.repeat(1, self.n_items_))

        X = sp.hstack([Xu, Xi])

        y_pred = self.predict(X)

        recommendations = []

        if from_item_ids is None:
            from_item_ids = self.item_ids

        for item_id in from_item_ids:
            if item_id in item_ids_rated_by_user_id:
                continue
            i_id = self.item_id_to_i_id[item_id]
            rating = y_pred[i_id]
            recommendations.append((item_id, rating))

        recommendations = sorted(recommendations, key=lambda x: -x[1])[:topN]

        return [item_id for item_id, rating in recommendations]
