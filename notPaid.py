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
# Read the check register of hand written checks
for student in nonPayersFile:
    print(student.rstrip())

# Read the .json file with the students
with open("outputs/all.json", "r") as read_file:
    students = json.load(read_file)

# print(students)

with open("outputs/test.json", "w") as write_file:
    json.dump(students, write_file, indent=4)

outStudents = json.dumps(students, indent=4)

# Clean up open files
nonPayersFile.close()
