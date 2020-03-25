#include <HsFFI.h>
#include "game.h"
#include "consts.h"
#include "main.h"
#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>

SDL_Window* window = NULL;
SDL_Renderer* renderer = NULL;

bool initialize(int scale)
{
    //Initialization flag
    bool success = true;

    //Initialize SDL
    if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) {
        printf( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError() );
		return false;
    }

    window = SDL_CreateWindow( "SDL", 100, 100, SCREEN_WIDTH * scale, SCREEN_HEIGHT * scale, SDL_WINDOW_SHOWN );
    if( window == NULL ) {
        printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
		return false;
    }

	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

	int error = SDL_RenderSetScale(renderer, scale, scale);
	// printf("error %s %p\n", SDL_GetError(), renderer);

	init();

    return success;
}

void stop()
{
    SDL_DestroyWindow( window );
    window = NULL;
    SDL_Quit();
}

int parseScale(int argc, char* args[]) {
	int scale = DEFAULT_SCALE;
	if (argc <= 1) {
		printf("Supply a scale argument for a different scale.\n");
		return scale;
	}
	int scaleArg = atoi(args[1]);
	if (scaleArg != 0) {
		return scaleArg;
	} else {
		return scale;
	}
}


int main( int argc, char* args[] )
{
    hs_init(&argc, &args);

	int scale = parseScale(argc, args);

	if( !initialize(scale) ) {
		printf( "Failed to initialize!\n" );
		return 1;
	}

    bool quit = false;


    int frame = 0;
    double prevTime = 0;
    int lastHandledTime = -1;

    while ( !quit ) {
        SDL_Event e;
        int result;
        do {
            if (e.window.timestamp != lastHandledTime) {
                lastHandledTime = e.window.timestamp;
                int code = events(&e);
                switch (code) {
                    case CODE_QUIT:
                    quit = true;
                }
            }
        } while ( SDL_PollEvent(&e) != 0 );

        struct timeval tv;
        gettimeofday(&tv, NULL);

        double time = (double)tv.tv_usec / 1e6;
        double dt = time - prevTime;

        if ( dt < 0 ) {
            dt = (time + 1) - prevTime;
        }
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
		SDL_RenderClear(renderer);

        loop(renderer, dt, frame);

        prevTime = time;

		SDL_RenderPresent(renderer);

        frame ++;
    }

    stop();
    hs_exit();
    return 0;
}
