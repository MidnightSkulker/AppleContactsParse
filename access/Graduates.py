#!/usr/local/bin/python3
from datetime import date

class StudentDate(date):
    # Convert string of the form mm/dd/yyyy to a Date
    def mmddyyyyToDate(self, d:str):
        print('mmddyyyy', d)
        d1 = d.split('/')
        if len(d1) == 3:
            (yyyy, mm, dd) = (int(d1[2]), int(d1[1]), int(d1[0]))
            # isoDate = d1[2] + '-' + d1[1] + '-' + d1[0]
            # return date.fromisoformat(isoDate)
            return date(yyyy, mm, dd)
        else:
            return None

    def __init__(self, childDate:str):
        print('__init__', childDate)
        # return mmddyyyyToDate(childDate)

    def isGraduating(self, currentYear:str, studentBirthDate:str):
        olderThan = mmddyyyyToDate('09/01/' ++ str(currentYear - 5))
        youngerThan = mmddyyyyToDate('09/01/' ++ str(currentYear - 4))
        birthDate = mmddyyyyToDate(studentBirthDat)
        return ((olderThan <= studentBirthDate) and (studentBirthDate <= youngerThan))

def mdyToDate(d:str):
    d1 = d.split('/')
    if len(d1) == 3:
        (yyyy, mm, dd) = (int(d1[2]), int(d1[1]), int(d1[0]))
        return date(yyyy, mm, dd)
    else:
        return None

def graduates(currentYear:str, studentBirthDate: str):
    cutOffDate = '09/01/'
    youngerThan = mdyToDate(cutOffDate + str(int(currentYear) - 5))
    olderThan = mdyToDate(cutOffDate + str(int(currentYear) - 6))
    birthDate = mdyToDate(studentBirthDate)
    return ((olderThan <= birthDate) and (birthDate <= youngerThan))
