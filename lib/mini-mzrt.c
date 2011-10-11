/*
  Racket
  Copyright (c) 2009-2011 PLT Scheme Inc.
 
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.
*/

// I've extracted the smallest possible piece of mzrt.c that I need
// to make this work on windows.  This code is totally unneeded
// on other platforms.

#ifdef WIN32
# include <windows.h>
# include <process.h>

struct mzrt_sema {
  HANDLE ws;
};

int mzrt_sema_post(mzrt_sema *s)
{
  ReleaseSemaphore(s->ws, 1, NULL);  
  return 0;
}

int mzrt_sema_destroy(mzrt_sema *s)
{
  CloseHandle(s->ws);
  free(s);

  return 0;
}

#endif
