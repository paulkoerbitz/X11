/* * Basic X program: * * This is a very simple X program. It shows the basics needed to get the * simplest X program working. Compile as: * cc hello_x11.c -lX11 -o hello_x11 * * When run, pops up a window with Hello World written in it. * When the mouse is clicked, the word Hi! appears at the spot. * If the window is covered, then uncovered ("exposed"), the * Hello World is redrawn - but not the various Hi! items. * If a key is pressed nothing happens except if it is q in which * case the program quits. * */

#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

main(argc,argv)
int argc;
char **argv;
{
	Display *mydisplay;
	Window mywindow;
	GC mygc;
	XEvent myevent;
	KeySym mykey;
	XSizeHints myhint;
	int myscreen;
	unsigned long myforeground, mybackground;
	int i;
	char text[10];
	int done;
	char *hello = "Hello World", *hi = "Hi!";

	mydisplay = XOpenDisplay("");
	myscreen = DefaultScreen(mydisplay);
	mybackground = WhitePixel(mydisplay,myscreen);
	myforeground = BlackPixel(mydisplay,myscreen);

	/* Suggest where to position the window: */
	myhint.x = 200;
	myhint.y = 200;
	myhint.width = 300;
	myhint.height = 300;
	myhint.flags = PPosition | PSize;

	/* Create a window - not displayed yet however: */
	mywindow = XCreateSimpleWindow(mydisplay,DefaultRootWindow(mydisplay),
																 myhint.x,myhint.y,myhint.width,myhint.height,5,myforeground,mybackground);

	// XSetStandardProperties(mydisplay,mywindow,hello,hello,None,argv,argc,&myhint );

	/* Create a Graphics Context (GC) for the window: */
	mygc = XCreateGC(mydisplay,mywindow,0,0);
	XSetBackground(mydisplay,mygc,mybackground);
	XSetForeground(mydisplay,mygc,myforeground);
	/* Select input devices to listen to: */
	XSelectInput(mydisplay,mywindow,ButtonPressMask|KeyPressMask|ExposureMask);
	/* Actually display the window: */
	XMapRaised(mydisplay,mywindow);

	/* Main Event Loop: This is the core of any X program: */
	done = 0;
	while (done==0) {
		XNextEvent(mydisplay,&myevent);
		printf("%d\n", myevent.type);
		switch(myevent.type) {
			case Expose: /* Repaint window on expose */
				if (myevent.xexpose.count==0)
					XDrawImageString(myevent.xexpose.display,myevent.xexpose.window, mygc,50,50,hello,strlen(hello));
				break;
			case MappingNotify: /* Process keyboard mapping changes: */
				// XRefreshKeyboardMapping(&myevent);
				break;
			case ButtonPress: /* Process mouse click - output Hi! at mouse: */
				XDrawImageString(myevent.xbutton.display,myevent.xbutton.window, mygc,myevent.xbutton.x,myevent.xbutton.y,hi,strlen(hi));
				break;
			case KeyPress: /* Process key press - quit on q: */
				i = XLookupString(&myevent,text,10,&mykey,0);
				if (i==1 && text[0]=='q')
					done = 1;
				break;
		}
	}

	XFreeGC(mydisplay,mygc);
	XDestroyWindow(mydisplay,mywindow);
	XCloseDisplay(mydisplay);
	exit(0);
}
