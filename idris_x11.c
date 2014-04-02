#include "idris_x11.h"

int idr_getEventType(XEvent* event)
{
	return event->type;
}

int idr_getXExposeCount(XEvent* event)
{
	return event->xexpose.count;
}

void* idr_getXExposeDisplay(XEvent* event)
{
	return event->xexpose.display;
}

int idr_getXExposeWindow(XEvent* event)
{
	return event->xexpose.window;
}

void* idr_getXButtonDisplay(XEvent* event)
{
	return event->xbutton.display;
}

int idr_getXButtonWindow(XEvent* event)
{
	return event->xbutton.window;
}

int idr_getXButtonX(XEvent* event)
{
	return event->xbutton.x;
}

int idr_getXButtonY(XEvent* event)
{
	return event->xbutton.y;
}
