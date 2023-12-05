import json
import numpy as np
import pandas as pd
import nltk
from csv import DictReader
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

with open('data/bakery_reviews.csv', encoding='utf8') as reviews_file:
    reader = DictReader(reviews_file)
    reviews_data = [row for row in reader]

def get_adjectives(text):
    for (word, tag) in nltk.pos_tag(nltk.tokenize.word_tokenize(text)):
        if tag == "JJ" or tag == "ADJ":
            return word
    return None

def get_segment(review_text, Aspects, default=''):
    review_text = review_text.lower()
    res = []
    c_score = []

    def get_all(review_text, aspect, default):
        if aspect not in review_text:
            return ""

        cur_aspect_index = review_text.index(aspect)
        temp = cur_aspect_index

        if temp != 0:
            cur_aspect_index -= 2

            while review_text[cur_aspect_index] not in ',.!?;\n' and cur_aspect_index >= 0:
                cur_aspect_index -= 1
            if review_text[cur_aspect_index + 1] == " ":
                cur_aspect_index += 2
            else:
                cur_aspect_index += 1

        cur_aspect_end_index_begin = temp + len(aspect)
        cur_aspect_end_index_end = cur_aspect_end_index_begin
        end_pos = len(review_text) - 1

        stop_punct_map = {c: None for c in ',.!?;\n'}
        relation_punct_list = ["and", "when", "but"]

        # next_aspect = self.get_next_aspect(review_text[cur_aspect_end_index_begin:end_pos])
        cur_aspect_des = get_adjectives(review_text[cur_aspect_end_index_begin:end_pos])

        while cur_aspect_end_index_end <= end_pos:
            # 在标点符号处截取
            cur_str = review_text[cur_aspect_end_index_end:min(cur_aspect_end_index_end + 1, end_pos)]
            if cur_str in stop_punct_map:
                break

            # 在转移符号处截取
            cur_strs = review_text[cur_aspect_end_index_begin:cur_aspect_end_index_end]
            relation_store = ""
            for relation in relation_punct_list:
                if relation in cur_strs.lower():
                    relation_store = relation
                    break

            if relation_store != "":
                cur_aspect_end_index_end -= len(relation_store)
                break

                # 在下一个aspect截取
                # if next_aspect != None:
                # 	if next_aspect in aspects_dic and next_aspect in cur_strs:
                # 		cur_aspect_end_index_end -= len(next_aspect)
                # 		break

                # 在aspect最近的形容词截取
            if cur_aspect_des is not None:
                if cur_aspect_des in cur_strs:
                    break

            cur_aspect_end_index_end += 1

        cur_aspect_end_index_end = min(cur_aspect_end_index_end, end_pos)

        aim = review_text[cur_aspect_index:cur_aspect_end_index_end]
        sentence = aim
        for item in default:
            sentence = sentence.replace(item, '')

        if aspect in sentence and aim not in res:
            text_tokens = nltk.tokenize.word_tokenize(aim)
            tokens_without_sw = [word for word in text_tokens if word not in nltk.corpus.stopwords.words('english')]
            aim = (" ").join(tokens_without_sw)
            vs = SentimentIntensityAnalyzer().polarity_scores(aim)
            if vs.get('compound') != 0:
                # print(vs.get('compound'))
                res.append(aim)
                c_score.append(float(vs.get('compound')))

        text = review_text[cur_aspect_end_index_end:]
        get_all(text, aspect, default)

    for words in Aspects:
        get_all(review_text, str(words), default)
        for item in res:
            review_text = review_text.replace(item, "")

    if len(c_score) > 0:
        compound = sum(c_score)/len(c_score)
        return [res, compound]
      








msg='hi'
'''
FOOD:
default = ['fast food', 'food chains', 'taco bell', 'making the food', 'order food']
Aspects = ['food', 'bowl', 'burrito', 'taco', 'rice', 'chicken', 'beans', 'salsa', 'chips', 'meat',
           'steak', 'cheese', 'sour cream', 'lettuce', 'corn', 'salad', 'ingredient', 'delicious', 'taste',
           'black', 'steak', 'guac', 'sauce', 'fresh', 'veggie', 'cilantro', 'stale', 'cooked', 'salty', 'portion',
           'amount', 'size','tortilla', 'scoop', 'meal', 'lunch', 'water']
'''
'''
SERVICE:

default = ['fast food', 'fast-food', 'am wrong', 'can\'t go wrong', 'I\'m wrong', 'I was wrong']
Aspects = ['service', 'time', 'attitude', 'staff', 'friendly', 'welcoming', 'manager', 'supervisor',
            'helpful', 'employee', 'polite', 'fast', 'customer', 'train', 'rude', 'wrong', 
            'serving', 'line', 'girl', 'guy', 'experience', 'cashier', 'lady','worker', 'minute', 'wait','online',
            'ready','slow','am','pm', 'pickup','hour','management','gloves', 'hand','app','deliver','missing',
            'card','phone', 'quick', 'busy', 'counter']#dinnning
'''
'''
PRICE:
default = ['costume', 'at all cost','accosted']
Aspects = ['price', 'cheap', 'affordable', 'pricy', 'expensive', 'cost', 'free', 'refund']
'''
'''
AMBIANCE:
'''
default = ['encountered']
Aspects = ['ambiance', 'environment', 'atmosphere', 'dirty', 'filthy', 'seat', 'music', 'wifi', 'window',
            'floor', 'decor', 'door', 'table', 'clean','trash','gloves','disgusting','mess','parking',] #crowd

Aspects = ['cute', 'town', 'beautiful', 'atmosphere', 'Spot', 'Table', 'Tables', 'parking', 'lovely', 'clean', 'ambiance', 'downtown', 'seat', 'authentic', 'dirty', 'poor', 'empty', 'adorable', 'comfortable', 'crowded', 'nearby', 'decorated', 'ambience', 'locations', 'divine', 'driving', 'neighborhood', 'quiet', 'window', 'convenient']

# nltk.download('punkt')
# text = "I want to be clear in the review. I LOVE chipotles food and i give it 5 stars. \nBut i want to review this location. The most of the staff SUCKS. Especially the manager guy (shaved hair, usually wear a black tshirt). \n\nHe is aggressive toward the customers. One one visit my bf couldn't hear what the girl on the line was asking him so he leaned over a little bit and asked \"what?\" all the sudden the manager guy was staring at him angrily asking if there was a problem. And then continued to stare as we checked out. He is lucky I didn't jump over the counter and ask him if he needed that look taken off his face. To top it off the food was sub par as to what we are used to. \n\nWe didn't visit chipotle for a while and we did again last week. The same guy is still the manager and while he didn't give us any problems, I saw his approach another guest who got lemonade in a water cup. Now I understand you shouldn't be saying you want water and getting a different drink but the manner in which he approached this guy was aggressive. Again the food was bad, my barbacoa tasted like soap and they have lessen the already pitiful amount of chips they give you. Seeing other reviews I see this location needs to GET IT TOGETHER. \n*oh and the parking does suck but i know there isnt much they can do about it."
# print(get_segment(text, Aspects, default))

# chipotle_reviews_sub = [0 for r in reviews_data]
b_ids = [r['business_id'] for r in reviews_data]
date = [r['date'] for r in reviews_data]
rating = [r['stars'] for r in reviews_data]
score = [[None for _ in Aspects] for _ in reviews_data]

for j in range(len(reviews_data)):
    temp = [None for asp in Aspects]
    for i in range(len(Aspects)):
        seg = get_segment(reviews_data[j]['text'], [Aspects[i]], default)
        if seg:
            temp[i] = seg[1]
    score[j][:] = temp

score = pd.DataFrame(data=score)
chip_info = pd.DataFrame(data={'business_id': b_ids, 'date': date,
                               'stars': rating})
chip_score_chart = pd.concat([chip_info,score], axis=1)
chip_score_chart.columns = ['business_id', 'date', 'stars']+Aspects

chip_score_chart.to_csv("data/ambiance_scores.csv", index=False, sep=',')
