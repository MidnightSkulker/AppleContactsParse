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
outputs/KasalukuyangWithDups.json KasalukyangWithDups Kasalukyuang: outputs/all.json
	cat ~/Google\ Drive/Learn/Haskell/AppleContactsParse/ARCHIVE/all.json | jq '.[].fields | select((.ORG != null) and (.ORG[0] | contains("Kasalukuyang")))' >outputs/KasalukuyangWithDups.json

# Generate the list of students with no immunization record.
# This list may have duplicate entries.
outputs/NoImmunization.json NoImmunization.json: outputs/KasalukuyangWithDups.json
	cat $< | jq '. | select((.Immunization != null) and (.Immunization == "No"))' >outputs/NoImmunization.json

# Build a list of students with no immunization records.
# This list should have no duplicates.
outputs/NoImmunizationEmail.json NoImmunizationEmail: outputs/NoImmunization.json
	cat $< | jq -c '. | {Immunization: .Immunization, Name: .FN, Email: .EMAIL1}' | sort -u >outputs/NoImmunizationEmail.json

# Build a list of email addresses for students with no immunization record.
# This list should have no duplicates
outputs/NoImmunizationEmail: outputs/NoImmunizationEmail.json
	cat $< | jq -r '.Email' | grep -v null >outputs/NoImmunizationEmail.json

# Build the list of student e-mails, where one email (of possibly more that one)
# is chosen for the student. This list may have duplicate entries for a student.
outputs/Email1WithDups Email1WithDups: outputs/KasalukuyangWithDups.json
	cat $< | jq '.EMAIL1' >outputs/Email1WithDups

# Build the list of student e-mails, where one email (of possibly more that one)
# is chosen for the student. This list should have no duplicate entries for
# a student.
outputs/Email1 Email1: Email1WithDups
	cat outputs/$< | sort -u | grep -v null >outputs/Email1

