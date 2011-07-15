submodules:
	! [ -d .git ] || git submodule update --init

install: submodules
	./.symlink_it.sh

.PHONY: install
