# Clean up the outputs
clean:
	rm outputs/*

# Generate a very messy text file with student records from the Mac Address Book
outputs/all.vcf: ~/Library/Application\ Support/AddressBook/ABAssistantChangelog.aclcddb-shm
	osascript backupAddressBook.scpt
	mv all.vcf outputs
	ls -l outputs/all.vcf

# Run the address book parser and the text version of the .vcf file, to generate
# a JSON output with all the address book records.
# There may be duplicates in this list.
outputs/all.json: outputs/all.vcf
	AppleContactsParse --vcf=outputs/all.vcf --json=outputs/all.json --NoPhoto --NoProdID --NoABUID --NoN --NoAdr

# Get only the student records from the JSON file of the Mac Address Book.
# There may be duplicates in this file.
outputs/KasalukuyangWithDups.json KasalukyangWithDups KasalukyuangWithDups: outputs/all.json
	cat $< | jq '.[].fields | select((.ORG != null) and (.ORG[0] | contains("Kasalukuyang")))' >outputs/KasalukuyangWithDups.json

outputs/Kasalukuyang.json Kasalukuyang: outputs/KasalukuyangWithDups.json
	cat $< | jq -c '. | {Name: .FN, Email: .EMAIL1, TEL: .TEL1, Immunization: .Immunization, Birthday: .Birthday, EMAILS: .EMAILS, TEL: .TELS}' | sort -u >outputs/Kasalukuyang.json

# Generate Current Email List
outputs/Emails.json Emails: outputs/Kasalukuyang.json
	cat $< | jq -c '. | {Email: .Email}' >outputs/Emails.json

# Generate list of graduates for 2019.
outputs/Graduates.json Graduates: outputs/Kasalukuyang.json
	cat $< | jq -c '. | select((.Birthday <= "2014-09-01") and (.Birthday >= "2013-09-01") ) | {Name: .Name, Birthday: .Birthday}' >outputs/Graduates.json
#	cat $< | jq -c '. | select((.Birthday <= "2014-09-01") and (.Birthday >= "2013-09-01") ) | {Name: .Name, Birthday: .Birthday}' | jq -s 'unique_by(.Name)' >outputs/Graduates.json

# Generate Current Birthdays
outputs/Birthdays.json Birthdays: outputs/Kasalukuyang.json
	cat $< | jq -c '. | {Name: .Name, Birthday: .Birthday}' >outputs/Birthdays.json

# Generate the list of students with no immunization record.
# This list may have duplicate entries.
outputs/NoImmunization.json NoImmunization.json NoImmunization: outputs/KasalukuyangWithDups.json
	cat $< | jq '. | select((.Immunization != null) and (.Immunization == "No"))' >outputs/NoImmunization.json

# Build a list of students with no immunization records.
# This list should have no duplicates.
outputs/NoImmunizationEmail.json NoImmunizationEmail: outputs/NoImmunization.json
	cat $< | jq -c '. | {Immunization: .Immunization, Name: .FN, Email: .EMAIL1}' | sort -u >outputs/NoImmunizationEmail.json
	cat outputs/NoImmunizationEmail.json | jq -c '. | {Immunization: .Immunization, Name: .Name, Email: .Email}'

# Build a list of email addresses for students with no immunization record.
# This list should have no duplicates
outputs/NoImmunizationEmail: outputs/NoImmunizationEmail.json
	cat $< | jq -r '.Email' | grep -v null >outputs/NoImmunizationEmail.json

# Build the list of student e-mails, where one email (of possibly more that one)
# is chosen for the student. This list may have duplicate entries for a student.
outputs/Email1WithDups Email1WithDups: outputs/KasalukuyangWithDups.json
	cat $< | jq -r '.EMAIL1' >outputs/Email1WithDups

# Build the list of student e-mails, where one email (of possibly more that one)
# is chosen for the student. This list should have no duplicate entries for
# a student.
outputs/Email1 Email1: Email1WithDups
	cat outputs/$< | sort -u | grep -v null >outputs/Email1

# Count lines of code
cloc:
	cloc --exclude-dir=outputs,.git,tutorial-env,ARCHIVE .
