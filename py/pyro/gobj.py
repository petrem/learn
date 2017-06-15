import datetime
import Pyro.core
import Pyro.naming
import threading
import gobject
import time
import fcntl

class PyroThread(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        print "Pyro thread initialized"

    def run(self):
        while True:
            print "bubu"
            time.sleep(1)

count = 0


def main():
    pyt = PyroThread()
    pyt.daemon = True
    main_loop = gobject.MainLoop()
    gobject.threads_init()
    pyt.start()

    def process_loop():
        global count
        print "Process loop starts", count
        time.sleep(1)
        count = count+1
        if count >= 3:
            raise Exception
        return True

    try:
        gobject.idle_add(process_loop)
        main_loop.run()
    except KeyboardInterrupt:
        pass
    except:
        print "Yak yak yak"
    finally:
        print "Gata"

if __name__ == "__main__":
    main()
