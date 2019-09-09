#!/usr/local/bin/python3
import sys
import re
import argparse

argumentParser = argparse.ArgumentParser(description='Angels Academy Taxes')
# List of students who have not paid.
argumentParser.add_argument('--students', help='List of students who have not paid')

# Now parse the arguments passed in
parsedArguments = argumentParser.parse_args()
nonPayersFile = open(parsedArguments.students)
# Read the check register of hand written checks
for student in nonPayersFile:
    print(student.rstrip())

# Clean up open files
nonPayersFile.close()
