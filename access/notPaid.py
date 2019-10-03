#!/usr/local/bin/python3
import sys
import re
import argparse
import json
from datetime import date
import Graduates

argumentParser = argparse.ArgumentParser(description='Not Paid Students')
# List of students who have not paid.
argumentParser.add_argument('--NonPayers', help='List of students who have not paid')
argumentParser.add_argument('--Graduates', help='List of students who have not paid')
argumentParser.add_argument('--Immunization', help='print out list of students who have no immunization, with email addresses', action='store_true')

# Now parse the arguments passed in
parsedArguments = argumentParser.parse_args()
# print('parsedArguments.nonPayers', parsedArguments.nonPayers)
# print('parsedArguments.immunization', parsedArguments.immunization)

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

# Print out information about those who have not paid
def notPaid(nonPayers:str, students:dict):
    nonPayersFile = open(nonPayers)
    # Read in the non paying students, and find their JSON record
    with open('outputs/notPaid.studentInfo', 'w') as studentInfo,\
         open('outputs/notPaid.emails', 'w') as studentEmail:
        for nonPayer in nonPayersFile:
            jsonStudent = findStudentInJSON(nonPayer.rstrip(), students)
            if jsonStudent:
                fields = jsonStudent['fields']
                if 'EMAIL1' in fields:
                    fn = jsonStudent['fields']['FN']
                    email = jsonStudent['fields']['EMAIL1']
                    studentInfo.write(fn + ' ' + email + '\n')
                    studentEmail.write(email + '\n')
                else:
                    studentInfo.write('No Email: ' + nonPayer.rstrip() + '\n')
            else:        
                studentInfo.write('No student record: ' + nonPayer.rstrip() + '\n')

    # Clean up open files
    nonPayersFile.close()

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

def graduates(graduationYear:str, students:dict):
    print ('----- graduationYear', graduationYear)
    with open('outputs/Graduation.StudentInfo', 'w') as studentInfo:
        for s in students:
            fields = s['fields']
            fn = fields['FN']
            if 'Birthday' in fields:
                studentBirthday = fields['Birthday']
                if Graduates.isGraduating(graduationYear, studentBirthday):
                    studentInfo.write(fn + ' ' + studentBirthday + '\n')
                else:
                    continute
            print('No Birthday for', fn)

# Performm the requested tasks.
if parsedArguments.NonPayers is not None: notPaid(parsedArguments.NonPayers, students)
if parsedArguments.Immunization: immunization(students)
if parsedArguments.Graduates: graduates(parsedArguments.Graduates, students)
