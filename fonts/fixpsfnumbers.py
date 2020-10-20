outlines = []
glyphindex = 0
with open('ter-powerline-v14n.txt', 'r') as f:
    lines = f.readlines()
    outlines.append(lines[0])
    for line in lines[1:]:
        if line.startswith('@'):
            rest = line.split(':')[1]
            outlines.append('@' + str(glyphindex) + ':' + rest)
            glyphindex += 1
        else:
            outlines.append(line)

with open('output.txt', 'w') as f:
    f.writelines(outlines)
