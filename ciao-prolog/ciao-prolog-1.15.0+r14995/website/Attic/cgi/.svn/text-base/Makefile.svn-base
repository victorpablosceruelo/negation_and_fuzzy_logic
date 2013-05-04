all:
	ciaoc buttonbar
	ciaoc load_file
	mv buttonbar buttonbar.cgi
	mv load_file load_file.cgi
	echo "PLEASE LOOK IN THE MAKEFILE AND CHANGE THE PROPER PATH"
	ciaoc -u /home/asapdemo/lib/ciaopp/ciaopp-1.0/paths -a 'actmods/tmpbased_publish' ciaopp_daemon
	gcc ciaopp_daemon_wrapper.c -o ciaopp_daemon_wrapper


clean:
	rm -f buttonbar.itf buttonbar.po buttonbar
	rm -f load_file.itf load_file.po load_file
	rm -f *.cgi *~ ciaopp_daemon *.itf *.po ciaopp_daemon_wrapper

