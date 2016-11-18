/* qterminal - a terminal widget for Qt
 * Copyright (C) 2011, 2013 Jacob Dawid (jacob.dawid@cybercatalyst.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "unix/SelfListener.h"

SelfListener::SelfListener(int a, QObject *parent) :
    QThread(parent) {
    _a = a;
}

void SelfListener::run() {
    char buf[4096 + 1];
    int len;
    bool running = true;
    while(running) {
         while((len = ::read(_a, buf, 4096)) > 0) {
            buf[len] = 0; // Just in case.
            emit recvData(buf, len);
            msleep(30);
         }
         if(len < 0)
           running = false;
    }
}
