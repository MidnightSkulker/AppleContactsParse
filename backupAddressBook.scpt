tell application "Contacts"
	set the clipboard to (vcard of people) as text
	do shell script "pbpaste >~/Google\\ Drive/Learn/Haskell/AppleContactsParse/all.vcf"
end tell

