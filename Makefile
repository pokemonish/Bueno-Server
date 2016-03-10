all:
	@ erlc httpd.erl;
clean:
	@ rm httpd.beam;
	@ rm -rf erl_crush.dump;