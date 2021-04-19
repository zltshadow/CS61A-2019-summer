""" Typing Test implementation """

from utils import *
from ucb import main

# BEGIN Q1-5
def lines_from_file(path):
    """Returns a list containing each row of the file as a string.

    path: indicates the location of a text file.
    """
    with open(path) as f:
        return [line.strip() for line in f]  # Should be rstrip() if input file is well-written

def new_sample(path, i):
    """
    Returns the i-th line of a text file.

    path: indicates the location of the sample_paragraphs.txt file in your directory.

    i: a non-negative interger that indicates which line to return.
    Lines are 0-indexed, so the 0-th line is the top line of the file.
    """
    with open(path) as f:
        for _ in range(i):
            next(f)  # Discard first i-1 lines
        return next(f).strip()

def analyze(sample_paragraph, typed_string, start_time, end_time):
    """Returns a list containing two values:
    words per minute and accuracy percentage.

    This function takes in a string sample_paragraph,
    a string provided by user input typed_string,
    a number start_time and
    a number end_time.

    Both start_time and end_time are measured in seconds. 
    """
    from functools import reduce
    from operator import truediv

    num_of_wrds = len(typed_string) / 5

    try:
        wrds_per_min = num_of_wrds / (end_time - start_time) * 60
    except ZeroDivisionError:
        wrds_per_min = float('Inf')

    words_pair = zip(sample_paragraph.split(), typed_string.split())
    alg = lambda cnt_length, sp_ts: (cnt_length[0] + (sp_ts[0] == sp_ts[1]), cnt_length[1] + 1)  # the algebra for catamorphism

    try:
        accuracy = truediv(*reduce(alg, words_pair, (0, 0))) * 100
    except ZeroDivisionError:
        accuracy = 0.0

    return [wrds_per_min, accuracy]

def pig_latin(wrd):
    """Returns the Pig Latin version of a word.

    For words that begin with a consonant, take the consonant/consonant cluster
    and move it to the end of the word, adding the suffix 'ay' to the end of the word.

    For words that begin with a vowel, leave the word as is and add the
    suffix 'way' to the end of the word.

    >>> pig_latin('dog')
    'ogday'
    >>> pig_latin('brush')
    'ushbray'
    >>> pig_latin('elephant')
    'elephantway'
    >>> pig_latin('awesome')
    'awesomeway'
    >>> pig_latin('rhythm')
    'rhythmay'
    """
    idx = next((i for i, v in enumerate(wrd) if v in 'aeiou'), len(wrd))
    return wrd[idx:] + (wrd[:idx] or 'w') + 'ay'

def autocorrect(user_input, words_list, score_function):
    """Autocorrect the user_input if it is no a correct word.

    user_input represents a single word.

    words_list is a list of all valid words.

    score_function calculates the difference between two words.

    If the user_input string is contained inside the words_list,
    the autocorrect function simply return that word.
    Otherwise, the autocorrect function should return the word
    from words_list that has the lowest difference from the provided
    user_input string based on the score_function.
    """
    assert words_list, 'words_list must be non-empty.'
    it = iter(words_list)

    ever_min_wrd = next(it)
    if user_input == ever_min_wrd:
        return user_input
    else:
        ever_min_score = score_function(user_input, ever_min_wrd)

    try:
        while True:
            wrd = next(it)
            if user_input == wrd:
                return user_input  # Need not to compare the remaining list if matched exactly
            else:
                score = score_function(user_input, wrd)
                if score < ever_min_score:
                    ever_min_wrd, ever_min_score = wrd, score
    except StopIteration:
        return ever_min_wrd

def swap_score(xs, ys):
    """Return the number of characters that needs to be substituted
    to change the characters in the first string into the second string.

    If the strings not of equal length, we will disregard all
    "extra" characters in the larger string.

    This function only considers the leftmost character of each string to
    correspond to each other. For example, if the two inputs are
    "word" and "weird", we'll only consider potential substitutions
    for "word" and "weir".
    """
    return sum(x != y for x, y in zip(xs, ys))  # bool is a subclass of int

# END Q1-5

# Question 6

def score_function(word1, word2):
    """A score_function that computes the edit distance between word1 and word2."""

    if not word1: # Fill in the condition
        # BEGIN Q6
        return len(word2)
        # END Q6

    elif not word2: # Feel free to remove or add additional cases
        # BEGIN Q6
        return len(word1)
        # END Q6

    elif word1[0] == word2[0]:
        return score_function(word1[1:], word2[1:])

    else:
        add_char = score_function(word1, word2[1:])  # Fill in these lines
        remove_char = score_function(word1[1:], word2)
        substitute_char = score_function(word1[1:], word2[1:])
        # BEGIN Q6
        return 1 + min(add_char, remove_char, substitute_char)
        # END Q6

KEY_DISTANCES = get_key_distances()

# BEGIN Q7-8
def score_function_accurate(word1, word2):
    """A score_function that computes the edit distance between word1 and word2."""
    if not word1:
        return float(len(word2))
    elif not word2:
        return float(len(word1))
    elif word1[0] == word2[0]:
        return score_function_accurate(word1[1:], word2[1:])
    else:
        add_char = 1.0 + score_function_accurate(word1, word2[1:])
        remove_char = 1.0 + score_function_accurate(word1[1:], word2)
        substitute_char = KEY_DISTANCES[word1[0], word2[0]] + score_function_accurate(word1[1:], word2[1:])
        return min(add_char, remove_char, substitute_char)

def score_function_final(word1, word2):
    """A score_function that computes the edit distance between word1 and word2."""
    import functools

    # Use memorization to cache the results and use indices to avoid slicing
    @functools.lru_cache(maxsize=None)
    def helper(i, j):
        if i == len(word1):
            return float(len(word2) - j)
        elif j == len(word2):
            return float(len(word1) - i)
        elif word1[i] == word2[j]:
            return helper(i + 1, j + 1)
        else:
            add_char = 1.0 + helper(i, j + 1)
            remove_char = 1.0 + helper(i + 1, j)
            substitute_char = KEY_DISTANCES[word1[i], word2[j]] + helper(i + 1, j + 1)
            return min(add_char, remove_char, substitute_char)

    return helper(0, 0)

# END Q7-8
