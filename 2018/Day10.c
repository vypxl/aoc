#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void step();
int display();

int main(int argc, char* argv) {
    // Parsing
    char* buf;
    int length;
    
    FILE *f = fopen("10.in", "r");
    fseek(f, 0, SEEK_END);
    length = ftell(f);
    fseek(f, 0, SEEK_SET);
    buf = malloc(length + 1);
    fread(buf, 1, length, f);
    fclose(f);
    buf[length] = '\0';

    int linecount = 0;
    for(int i = 0; i < length; i++) if(buf[i] == '\n') linecount++;
    
    int* data = malloc(linecount * 4 * sizeof(int));

    int pos = 0;
    for(int i = 0; i < linecount; i++) {
        for(int j = 0; j < 4; j++) {
            while(buf[pos] != '<' && buf[pos] != ',') pos++;
            pos++;
            if(buf[pos] == ' ') pos++;
            sscanf(buf + pos, "%d", &data[i * 4 + j]);
        }
    }

    // Loop until answer is found APPROXIMATELY
    // 10800 works for me but could not work for others!
    for(int i = 0; i < 10800; i++) {
        step(data, linecount);
        if(display(data, linecount))
            printf("Solution for part 2: %d\n", i);
    }
}

void step(int* data, int count) {
    // Add to each point it's velocity vector
    for(int i = 0; i < count; i++) {
        data[i * 4 + 0] += data[i * 4 + 2];
        data[i * 4 + 1] += data[i * 4 + 3];
    }
}

int display(int* data, int count) {
    int maxx = 0;
    int maxy = 0;
    int minx = 0;
    int miny = 0;
    for(int i = 0; i < count; i++) {
        int x = data[i * 4 + 0];
        int y = data[i * 4 + 1];
        if(maxx < x) maxx = x;
        if(maxy < y) maxy = y;
        if(minx > x) minx = x;
        if(miny > y) miny = y;
    }
    int w = abs(maxx - minx) + 2;
    int h = abs(maxy - miny) + 1;
    int wh = w * h;

    // Only display if reasonable size
    if(w > 300 || h > 200) return 0;

    char* txt = malloc(wh + 1);
    for(int i = 0; i < wh + 1; i++) txt[i] = ' ';
    txt[wh] = '\0';
    for(int i = 0; i < h; i++) txt[i * w + w - 1] = '\n';

    for(int i = 0; i < count-1; i++) {
        int x = data[i * 4 + 0];
        int y = data[i * 4 + 1];
        txt[x + y * w] = '#';
    }
    printf("%s\n-- Solution for part 1: ^^^^\n", txt);
    return 1;
}

// Solution part 1: GEJKHGHZ
// Solution part 2: 10681
