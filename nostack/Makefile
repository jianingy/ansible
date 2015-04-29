%: %.yml
	ansible-playbook -u root -i "$(TARGET)," -e "target=$(TARGET)" $< $(EXTRA)
