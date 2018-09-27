outputs/all.vcf: ~/Library/Application\ Support/AddressBook/ABAssistantChangelog.aclcddb-shm
	osascript backupAddressBook.scpt
	mv all.vcf outputs
	ls -l outputs/all.vcf

outputs/all.json: outputs/all.vcf
	AppleContactsParse --vcf=outputs/all.vcf --json=outputs/all.json --NoPhoto --NoProdID --NoABUID --NoN --NoAdr

outputs/KasalukuyangWithDups.json: outputs/all.json
	cat ~/Google\ Drive/Learn/Haskell/AppleContactsParse/ARCHIVE/all.json | jq '.[].fields | select((.ORG != null) and (.ORG[0] | contains("Kasalukuyang")))' >$@

outputs/NoImmunization.json: outputs/KasalukuyangWithDups.json
	cat $< | jq '. | select((.Immunization != null) and (.Immunization == "No"))' >$@

outputs/NoImmunizationEmail.json: outputs/NoImmunization.json
	cat $< | jq -c '. | {Immunization: .Immunization, Name: .FN, Email: .EMAIL1}' | sort -u >$@

outputs/NoImmunizationEmail: outputs/NoImmunizationEmail.json
	cat $< | jq -r '.Email' | grep -v null >$@
NoImmunizationEmail: outputs/NoImmunizationEmail
