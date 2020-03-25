#ifndef INIT_H
#define INIT_H

#include <SDL.h>

void init();
int events(SDL_Event* event);
void loop(SDL_Renderer* renderer, double dt, int frame);
void drawMenu(SDL_Renderer* renderer);
void drawNavigator(SDL_Renderer* renderer);
void drawAt(SDL_Renderer* renderer, SDL_Texture* texture, int x, int y);
void drawText(SDL_Renderer* renderer, char text[], int x, int y);
void drawDimensions(SDL_Renderer* renderer, int position[], int x, int y);
Uint8 r(int color);
Uint8 g(int color);
Uint8 b(int color);
void handleClick(int button, int x, int y);
void handleMouseMove(int x, int y, int xrel, int yrel);

struct Axis {
    int axis;
    int direction;
};

#endif
