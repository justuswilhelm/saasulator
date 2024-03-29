.PHONY=all clean format reactor serve

TEST=tests
APPLICATION=application
BUILD=build
TARGET=build/main.js build/index.html build/CNAME build/circle.yml
SRC=$(APPLICATION)/Main.elm $(wildcard $(APPLICATION)/*.elm) $(wildcard $(APPLICATION)/Native/*.js)
ELM_MAKE=elm-make
ELM_MAKE_FLAG=--warn --yes

GIT_BRANCH=gh-pages

all: $(TARGET)

$(TARGET): $(BUILD)

$(BUILD):
	mkdir $@

$(BUILD)/main.js: $(SRC)
	$(ELM_MAKE) $< --output $@ $(ELM_MAKE_FLAG)

$(BUILD)/index.html: pages/index.html
	cp $< $@

$(BUILD)/CNAME: CNAME
	cp $< $@

$(BUILD)/circle.yml: circle.yml
	cp $< $@

format:
	elm-format-0.18 $(APPLICATION) $(TEST)/*.elm --yes

clean:
	rm -rf elm-stuff/build-artifacts/
	rm -f $(TARGET)

reactor:
	elm-reactor

dependencies:
	elm-package install
	npm install -g elm-test

serve:
	cd $(BUILD) && python -m SimpleHTTPServer 3000

deploy: all
	./deploy.sh

test:
	elm test --watch

watch:
	watch make all
