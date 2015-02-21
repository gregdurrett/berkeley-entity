# some random useful functions

TARGET = target/scala-2.11/berkeley-entity-assembly-1.jar

all: $(TARGET)

$(TARGET): $(wildcard src/**)
	sbt assembly

aceTester: $(TARGET)
	java -cp $(TARGET) edu.berkeley.nlp.entity.wiki.ACETester -dataPath data/ace05/ace05-all-conll

queryModel: $(TARGET)
	java -cp $(TARGET) edu.berkeley.nlp.entity.wiki.QueryChooser -wikiDBPath models/wiki-db-ace.ser.gz
