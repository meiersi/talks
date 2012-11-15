slides.html: bytestring-builder-talk.md
	pandoc -f markdown+lhs -w slidy -s bytestring-builder-talk.md --self-contained -o slides.html

handout.html: bytestring-builder-talk.md
	pandoc -f markdown+lhs -s -S --toc bytestring-builder-talk.md -o handout.html

all: handout.html slides.html

clean:
	rm -f handout.html
	rm -f slides.html

