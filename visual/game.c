#ifdef __GLASGOW_HASKELL__
#include "DAWG_stub.h"
#endif

#include "game.h"
#include "loaders.h"
#include "consts.h"
#include <stdbool.h>
#include <math.h>

SDL_Surface* screen = NULL;
SDL_Texture* tCharacter = NULL;
SDL_Texture* tFont = NULL;
SDL_Texture* tDimensions = NULL;
SDL_Texture* tTiles = NULL;

double totalTime = 0;
int dimension = 3;
int dimensionColors[] = {
    0xff0000, 0x00ff00, 0x3333ff, 0x00ffff,
    0xff00ff, 0xffff00, 0xff8000, 0x0080ff,
    0x8080ff, 0xff0080, 0x80ff80, 0x8000ff };
int position[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
struct Axis selectedAxis = { -1, 1 };

void init()
{
    tFont = loadBMP("resources/font.bmp");
}

int events(SDL_Event* event)
{
    switch (event->window.type) {
        case SDL_QUIT:
            return CODE_QUIT;
    }
    return CODE_NONE;
}

void loop(SDL_Renderer* renderer, double dt, int frame)
{
    totalTime += dt;


    drawText(renderer, "Dimensional Navigator", 10, 10);

    if ( (frame - 100) % 5000 == 0 ) {
        // printf("%f\n", 1 / dt);
    }
}

void drawText(SDL_Renderer* renderer, char text[], int x, int y)
{
    int i = 0;
    char character = text[0];
    while (character != 0) {
        int index = (int)character - 32;
        SDL_Rect srcRect = {
            CHAR_WIDTH * (index % 16),
            CHAR_HEIGHT * (index / 16),
            CHAR_WIDTH,
            CHAR_HEIGHT
        };

        SDL_Rect dstRect = {
            x + i * CHAR_WIDTH,
            y,
            CHAR_WIDTH,
            CHAR_HEIGHT
        };

        SDL_RenderCopy(renderer, tFont, &srcRect, &dstRect);

        i++;
        character = text[i];
    }
}


Uint8 r(int color) {
    return color >> 16 & 0xff;
}

Uint8 g(int color) {
    return color >> 8 & 0xff;
}

Uint8 b(int color) {
    return color & 0xff;
}

void handleMouseMove(int x, int y, int xrel, int yrel) {

}

void handleClick(int button, int x, int y) {

}
