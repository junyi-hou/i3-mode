BINDIR := $(HOME)/.local/bin

.PHONY: install

install:
	@mkdir -p $(BINDIR)
	@if command -v i3-msg > /dev/null 2>&1; then \
		ln -sf $(CURDIR)/i3-call $(BINDIR)/i3-call; \
		echo "installed i3-call"; \
	fi
	@if command -v swaymsg > /dev/null 2>&1; then \
		ln -sf $(CURDIR)/sway-call $(BINDIR)/sway-call; \
		echo "installed sway-call"; \
	fi
	@if command -v aerospace > /dev/null 2>&1; then \
		ln -sf $(CURDIR)/aerospace-call $(BINDIR)/aerospace-call; \
		echo "installed aerospace-call"; \
	fi
