#ifndef _IDRIS_NET_H_
#define _IDRIS_NET_H_

// #include <X11/X.h>
#include <X11/Xlib.h>

int idr_getEventType(XEvent* event);
int idr_getEventType(XEvent* event);
int idr_getXExposeCount(XEvent* event);
void* idr_getXExposeDisplay(XEvent* event);
int idr_getXExposeWindow(XEvent* event);
void* idr_getXButtonDisplay(XEvent* event);
int idr_getXButtonWindow(XEvent* event);
int idr_getXButtonX(XEvent* event);
int idr_getXButtonY(XEvent* event);

#endif
