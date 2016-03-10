all:
	@ erlc httpd.erl;
clean:
	@ rm -f httpd.beam;
	@ rm -f erl_crash.dump;