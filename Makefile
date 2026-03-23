default:
	go run ./tool

tgz:
	tar czhf output.tgz output

clean:
	rm -rf output output.tgz

.PHONY: clean tgz default
