#!/usr/local/bin/python3
import sys
import re
import argparse
import json

argumentParser = argparse.ArgumentParser(description='Not Paid Students')
# List of students who have not paid.
argumentParser.add_argument('--students', help='List of students who have not paid')

# Now parse the arguments passed in
parsedArguments = argumentParser.parse_args()
nonPayersFile = open(parsedArguments.students)

# Read the .json file with the students
with open("outputs/students.json", "r") as read_file:
    students = json.load(read_file)

# print(students)
def findStudentInJSON(student, students):
    for s in students:
        if re.match(student, s['fields']['FN']):
            return s
        else:
            continue
    return None

with open("outputs/test.json", "w") as write_file:
    json.dump(students, write_file, indent=4)

# for s in students:
#   print(s['fields']['FN'])

# f1 = findStudentInJSON('Madhesh', students)
# print('f1', f1)
# f2 = findStudentInJSON('Sri Gowri', students)
# print('f2', f2)
# f3 = findStudentInJSON('Advaita', students)
# print('f3', f3)

# Read in the non paying students, and find their JSON record
for nonPayer in nonPayersFile:
    jsonStudent = findStudentInJSON(nonPayer.rstrip(), students)
    if jsonStudent:
        fields = jsonStudent['fields']
        if 'EMAIL1' in fields:
            fn = jsonStudent['fields']['FN']
            email = jsonStudent['fields']['EMAIL1']
            print(fn, email)
        else:
            print('No Email: ', nonPayer.rstrip())
    else:        
        print('No student record: ', nonPayer.rstrip())

# Clean up open files
nonPayersFile.close()
