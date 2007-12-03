NODENAME=openmoko
OPENMOKO_ARGS=
SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))
ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info

MNESIA_DIR=/tmp/openmoko-$(NODENAME)-mnesia
LOG_FILE=/tmp/openmoko-$(NODENAME).log
SASL_LOG_FILE=/tmp/openmoko-$(NODENAME)-sasl.log

ERL_CMD=erl \
	-boot start_sasl \
	-sname $(NODENAME) \
	+W w \
	-pa $(EBIN_DIR) \
	+K true \
	+A10 \
	-kernel inet_default_listen_options '[{sndbuf, 16384}, {recbuf, 4096}]' \
	-sasl errlog_type error \
	-mnesia dir '"$(MNESIA_DIR)"' \
	$(OPENMOKO_ARGS)

ERL_CALL=erl_call -sname $(NODENAME) -e

all: $(TARGETS) chmod-executable

chmod-executable:
	chmod a+x ./copy-to-neo ./runit ./monitor-erl priv/*.sh priv/erlang-openmoko.matchbox.session

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	erlc $(ERLC_OPTS) $<

clean: cleandb
	rm -f $(TARGETS)
	rm -f ebin/*.beam

cleandb: stop-node
	erl -mnesia dir '"$(MNESIA_DIR)"' -noshell -eval 'lists:foreach(fun file:delete/1, filelib:wildcard(mnesia:system_info(directory) ++ "/*")), halt().'

############ various tasks to interact with erlang-openmoko ###################

run: all
	$(ERL_CMD) -s openmoko

run-node: all
	$(ERL_CMD)

start-openmoko-on-node: all
	echo "openmoko:start()." | $(ERL_CALL)

stop-openmoko-on-node: all
	echo "openmoko:stop()." | $(ERL_CALL)

stop-node:
	-$(ERL_CALL) -q
