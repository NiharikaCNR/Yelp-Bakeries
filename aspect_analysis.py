import json
import numpy as np
import pandas as pd
import nltk
from csv import DictReader
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

with open('data/bakery_reviews.csv', encoding='utf8') as f:
    reader = DictReader(f)
    bakery_reviews = [row for row in reader]

def get_adjectives(text):
    for (word, tag) in nltk.pos_tag(nltk.tokenize.word_tokenize(text)):
        if tag == "JJ" or tag == "ADJ":
            return word
    return None

def segment_text(review_text, dictionary, default=''):
    result = []
    c_score = []

    def get_all(review_text, aspect, default):
        if aspect not in review_text:
            return ""

        curr_index = review_text.index(aspect)
        temp = curr_index

        if temp != 0:
            curr_index -= 2

            while review_text[curr_index] not in ',.!?;\n' and curr_index >= 0:
                curr_index -= 1
            if review_text[curr_index + 1] == " ":
                curr_index += 2
            else:
                curr_index += 1

        cur_aspect_end_index_begin = temp + len(aspect)
        cur_aspect_end_index_end = cur_aspect_end_index_begin
        end_pos = len(review_text) - 1

        stop_punct_map = {c: None for c in ',.!?;\n'}
        relation_punct_list = ["and", "when", "but"]

        cur_aspect_des = get_adjectives(review_text[cur_aspect_end_index_begin:end_pos])

        while cur_aspect_end_index_end <= end_pos:
            cur_str = review_text[cur_aspect_end_index_end:min(cur_aspect_end_index_end + 1, end_pos)]
            if cur_str in stop_punct_map:
                break
            cur_strs = review_text[cur_aspect_end_index_begin:cur_aspect_end_index_end]
            relation_store = ""
            for relation in relation_punct_list:
                if relation in cur_strs.lower():
                    relation_store = relation
                    break

            if relation_store != "":
                cur_aspect_end_index_end -= len(relation_store)
                break
            if cur_aspect_des is not None:
                if cur_aspect_des in cur_strs:
                    break

            cur_aspect_end_index_end += 1

        cur_aspect_end_index_end = min(cur_aspect_end_index_end, end_pos)

        sub = review_text[curr_index:cur_aspect_end_index_end]
        sentence = sub
        for word in default:
            sentence = sentence.replace(word, '')

        if aspect in sentence and sub not in result:
            text_tokens = nltk.tokenize.word_tokenize(sub)
            tokens_without_sw = [word for word in text_tokens if word not in nltk.corpus.stopwords.words('english')]
            sub = (" ").join(tokens_without_sw)
            vs = SentimentIntensityAnalyzer().polarity_scores(sub)
            if vs.get('compound') != 0:
                result.append(sub)
                c_score.append(float(vs.get('compound')))

        text = review_text[cur_aspect_end_index_end:]
        get_all(text, aspect, default)

    for words in dictionary:
        get_all(review_text, str(words), default)
        for item in result:
            review_text = review_text.replace(item, "")

    if len(c_score) > 0:
        return [result, sum(c_score)/len(c_score)]
      


## FOOD:
# default = ['food']
# dictionary = ['croissant', 'cake', 'cupcakes', 'cupcake', 'sandwich', 'wine', 'bread', 'toast', 'chocolate', 'salad', 'cheese', 'brunch', 'eggs', 'egg', 'chicken', 'donut', 'donuts', 'pastries', 'pastry', 'benedict', 'bacon', 'sandwiches', 'avocado', 'sourdough', 'cookie', 'fruit', 'croissants', 'salmon', 'butter', 'macarons', 'cream', 'burrito', 'turkey', 'strawberry', 'lobster', 'almond', 'caramel', 'blackberry', 'burger', 'blueberry', 'muffin', 'gourmet', 'sausage', 'cinnamon', 'grilled', 'raspberry', 'bagels', 'tomatoes', 'macaron', 'pasta', 'desserts', 'Steak', 'omelette', 'apple', 'soup', 'potatoes', 'brioche', 'danish', 'pie', 'biscuit', 'scones', 'tuna', 'brownie', 'tart']      

## DRINKS:
# default = ['drink']
# dictionary = ['coffee', 'latte', 'lattes', 'vanilla', 'chocolate', 'milk', 'Cold brew', 'caramel', 'almond', 'cappuccino', 'cinnamon', 'coconut', 'cream', 'espresso', 'mocha', 'matcha', 'juice', 'mint', 'americano']

## SERVICE:
# default = ['service', 'staff']
# dictionary = ['time', 'friendly', 'free', 'recommend', 'perfect', 'lunch', 'vegan', 'gluten', 'rude', 'horrible', 'worst', 'awful', 'wait', 'counter', 'Seating', 'excellent', 'disappointed', 'busy', 'prices', 'tasty', 'helpful', 'warm', 'perfectly', 'wait', 'healthy', 'glad', 'fast', 'attentive', 'slow', 'quickly', 'recommended', 'cashier', 'excited', 'terrible', 'veggie', 'waitress', 'welcoming', 'fabulous', 'awful', 'satisfied', 'appreciated', 'breakfast', 'special', 'wedding']

## PRICE:
# default = ['costume', 'at all cost','accosted']
# dictionary = ['price', 'cheap', 'affordable', 'pricy', 'expensive', 'cost', 'free', 'refund']

## AMBIANCE:
# default = ['encountered']
# dictionary = ['cute', 'town', 'beautiful', 'atmosphere', 'Spot', 'Table', 'Tables', 'parking', 'lovely', 'clean', 'ambiance', 'downtown', 'seat', 'authentic', 'dirty', 'poor', 'empty', 'adorable', 'comfortable', 'crowded', 'nearby', 'decorated', 'ambience', 'locations', 'divine', 'driving', 'neighborhood', 'quiet', 'window', 'convenient']


business_ids = [review['business_id'] for review in bakery_reviews]
review_date = [review['date'] for review in bakery_reviews]
star_rating = [review['stars'] for review in bakery_reviews]
aspect_scores = [[None for _ in dictionary] for _ in bakery_reviews]

for i in range(len(bakery_reviews)):
    score_i = [None for asp in dictionary]
    for j in range(len(dictionary)):
        segmented = segment_text(bakery_reviews[i]['text'].lower(), [dictionary[j]], default)
        if segmented:
            score_i[j] = segmented[1]
    aspect_scores[i][:] = score_i



aspect_scores = pd.DataFrame(data=aspect_scores)
description_cols = pd.DataFrame(data={'business_id': business_ids, 'date': review_date, 'stars': star_rating})
aspect_scores = pd.concat([description_cols,aspect_scores], axis=1)
aspect_scores.columns = ['business_id', 'date', 'stars'] + dictionary

# aspect_scores.to_csv("data/food_scores.csv", index=False, sep=',')
