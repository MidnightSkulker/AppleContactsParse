#!/usr/local/bin/python3
import sys
import re
import argparse
import json

argumentParser = argparse.ArgumentParser(description='Not Paid Students')
# List of students who have not paid.
argumentParser.add_argument('--nonPayers', help='List of students who have not paid')
argumentParser.add_argument('--immunization', help='print out list of students who have no immunization, with email addresses', action='store_true')

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
def notPaid(nonPayers:str):
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
def immunization():
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
                    studentInfo.write(fn +' ' + email + '\n')
                else:
                    continue
            else:
                print('No Immunization field for', fn)

# Performm the requested tasks.
if parsedArguments.nonPayers is not None: notPaid(parsedArguments.nonPayers)
if parsedArguments.immunization: immunization()