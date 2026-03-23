default:
	mkdir -p output
	[ -L output/assets ] || ln -sf ../assets output/assets
	go run ./cmd/site -o output

tgz:
	tar czhf output.tgz output

clean:
	rm -rf output output.tgz

.PHONY: clean tgz default
