#!/usr/local/bin/python3
from datetime import date

class StudentDate(date):
    # Convert string of the form mm/dd/yyyy to a Date
    def mmddyyyyToDate(d:str):
        d1 = d.split('/')
        if len(d1) == 3:
            (yyyy, mm, dd) = (d1[2], d1[1], d1[0])
            # isoDate = d1[2] + '-' + d1[1] + '-' + d1[0]
            # return date.fromisoformat(isoDate)
            return datetime.date(d1[2], d1[1], d1[0])
        else:
            return None

    def __init__(childDate:str):
        return mmddyyyyToDate(childDate)

    def isGraduating(currentYear:str, studentBirthDate:str):
        olderThan = mmddyyyyToDate('09/01/' ++ str(currentYear - 5))
        youngerThan = mmddyyyyToDate('09/01/' ++ str(currentYear - 4))
        birthDate = mmddyyyyToDate(studentBirthDat)
        return ((olderThan <= studentBirthDate) and (studentBirthDate <= youngerThan))

def graduates(currentYear:str):
    print('Graduates!', date.today())
    olderThan = StudentDate('09/01/2015')
    youngerThan = StudentDate('09/01/' + str(int(currentYear) - 4))
    studentBirthDate = '09/12/2015'
    birthDate = StudentDate('09/12/2015')
    print('Test1: ', olderThan <=  birthDate)
