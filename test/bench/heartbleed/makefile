ALL += heartbleed/heartbleed
FULL += heartbleed/heartbleed
SNSCOUNT += heartbleed/heartbleed
RELAX += heartbleed/heartbleed
CONCR += heartbleed/heartbleed

#heartbleed/heartbleed.setup:
#	sleep 30 && python3 heartbleed/heartbleed.py localhost -p 8080 &

heartbleed/heartbleed.run:
	cd heartbleed && nix-build

heartbleed/heartbleed.setup:
	$(call mk_check_run,$(@:.setup=)) && sleep 30 && (echo | heartbleed/result/openssl s_client -connect localhost:8080) && sleep 10 && python3 heartbleed/heartbleed.py localhost -p 8080 &

heartbleed/heartbleed.snscount.setup:
	$(call mk_check_run,$(@:.setup=)) && sleep 30 && (echo | heartbleed/result/openssl s_client -connect localhost:8080) && sleep 10 && python3 heartbleed/heartbleed.py localhost -p 8080 &

heartbleed/heartbleed.relax.setup:
	$(call mk_check_run,$(@:.setup=)) && sleep 30 && python3 heartbleed/heartbleed.py localhost -p 8080 &

heartbleed/heartbleed.concr.setup:
	$(call mk_check_run,$(@:.setup=)) && sleep 30 && python3 heartbleed/heartbleed.py localhost -p 8080 &
