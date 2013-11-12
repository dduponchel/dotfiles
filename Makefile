deps: submodules
	$(MAKE) -C external/dmenu
	chmod +x external/dmenu/dmenu_path

submodules:
	! [ -d .git ] || git submodule update --init --recursive

install: submodules deps
	./.symlink_it.sh

.PHONY: submodules deps install
