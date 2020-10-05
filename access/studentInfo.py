#!/usr/local/bin/python3
import sys
import re
import argparse
import json
from datetime import date
import Graduates

argumentParser = argparse.ArgumentParser(description='Student Information from Apple Contacts JSON file')
# List of students who have not paid.
argumentParser.add_argument('--List', help='List of students who have not paid')
argumentParser.add_argument('--Graduates', help='Report on who is Graduating for the specified year')
argumentParser.add_argument('--Immunization', help='print out list of students who have no immunization, with email addresses', action='store_true')
argumentParser.add_argument('--Emails', help='print out list of students full names and email addresses', action='store_true')

# Now parse the arguments passed in
parsedArguments = argumentParser.parse_args()

# Read the .json file with the students
with open("outputs/students.json", "r") as read_file:
    students = json.load(read_file)

# Find information about a student from the list of students in the JSON file.
def findStudentInJSON(student, students):
    for s in students:
        if re.match(student, s['fields']['FN']):
            return s
        else:
            continue
    return None

# Print out information about the specified list of students
def getList(sought:str, students:dict):
    print('sought', sought)
    # Read in the sought after students, and find their JSON record
    try:
        with open('inputs/' + sought, 'r') as soughtFile:
            with open('outputs/' + sought + '.StudentInfo', 'w') as studentInfo,\
                 open('outputs/' + sought + '.Emails', 'w') as studentEmail:
                for current in soughtFile:
                    print('current', current.rstrip())
                    s = findStudentInJSON(current.rstrip(), students)
                    if s:
                        fields = s['fields']
                        if 'EMAIL1' in fields:
                            fn = s['fields']['FN']
                            email = s['fields']['EMAIL1']
                            studentInfo.write(fn + ' ' + email + '\n')
                            studentEmail.write(email + '\n')
                        else:
                            studentInfo.write('No Email: ' + current.rstrip() + '\n')
                            print('No Email', fn)
                    else:        
                        studentInfo.write('No student record: ' + current.rstrip() + '\n')
    except IOError as error:
        print('Could not open file', sought, error)


# Process the immunization records
def immunization(students):
    with open('outputs/Immunization.StudentInfo', 'w') as studentInfo:
        for s in students:
            fields = s['fields']
            fn = fields['FN']
            if 'Immunization' in fields:
                if re.match('No', s['fields']['Immunization']):
                    if 'EMAIL1' in fields:
                        email = fields['EMAIL1']
                    else:
                        email = 'NO EMAIL'
                    studentInfo.write(fn + ' ' + email + '\n')
                else:
                    continue
            else:
                print('No Immunization field for', fn)

# Determine if a student is graduating for a given year
def isGraduating(currentYear:str, birthDate:str):
    olderThan = str(int(currentYear) - 6) + '-09-01'
    youngerThan = str(int(currentYear) - 5) + '-09-01'
    return ((olderThan <= birthDate) and (birthDate <= youngerThan))

# Find Student information for the graduates of a specified year
def graduates(graduationYear:str, students:dict):
    with open('outputs/Graduates.StudentInfo', 'w') as studentInfo:
        for s in students:
            fields = s['fields']
            fn = fields['FN']
            if 'Birthday' in fields:
                studentBirthday = fields['Birthday']
                if isGraduating(graduationYear, studentBirthday):
                    studentInfo.write(fn + ' ' + studentBirthday + '\n')
                else:
                    continue
            else:
                print('No Birthday for', fn)

# Find student information, focusing on the emails
def emails(students:dict):
    with open('outputs/Emails.StudentInfo', 'w') as studentInfo,\
         open('outputs/Emails', 'w') as studentEmail:
        for s in students:
            fields = s['fields']
            fn = fields['FN']
            if 'EMAIL1' in fields:
                email = fields['EMAIL1']
                studentInfo.write(fn + ' ' + email + '\n')
                studentEmail.write(email + '\n')
            else:
                print('No Email for', fn)

# Performm the requested tasks.
if parsedArguments.List is not None: getList(parsedArguments.List, students)
if parsedArguments.Immunization: immunization(students)
if parsedArguments.Graduates: graduates(parsedArguments.Graduates, students)
if parsedArguments.Emails: emails(students)
