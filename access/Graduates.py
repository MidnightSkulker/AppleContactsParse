#!/usr/local/bin/python3
from datetime import date

def mmddyyyyToDate(d:str):
    d1 = d.split('/')
    if len(d1) == 3:
        (yyyy, mm, dd) = (int(d1[2]), int(d1[0]), int(d1[1]))
        # print ('(yyyy,mm,dd)', (yyyy, mm, dd))
        return date(yyyy, mm, dd)
    else:
        return None

# class DateExtended(date):
#     def __new__(self, childDate:str):
#         # FOR THIS CLASS ONLY
#         # because __new__ for the date class creates the instance you need
#         # to pass the arguments to the superclass here
#         # and **not** in the __init__
#         return super().__new__(mmddyyyyToDate(childDate))
#     def __init__(self, year: int, month: int, day: int, date_format: str=None):
#         # datetime.date.__init__ is just object.__init__ so it takes no arguments.
#         super().__init__()  
