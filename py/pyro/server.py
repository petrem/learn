import datetime
import Pyro.core
import Pyro.naming
import threading
import gobject
import time
import fcntl

class BlockingThread(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)

    def run(self):
        print "Thread", self, "started."
        #raise Exception
        # fd = open("lock", "r")
        # while True:
        #     print "Before the lock"
        #     fcntl.flock(fd, fcntl.LOCK_EX)
        #     time.sleep(1)
        #     fcntl.flock(fd, fcntl.LOCK_UN)
        #     print "After the lock"

class PyroThread(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        print "Pyro thread initialized"

    def run(self):
        Pyro.core.initServer()
        daemon = Pyro.core.Daemon(host="localhost")
        locator = Pyro.naming.NameServerLocator()
        ns = locator.getNS()
        daemon.useNameServer(ns)

        serverobj = TimeServer()
        print "Timeserver Request Handler started",
        if self.daemon:
            print "as daemon"
        else:
            print "as non daemon"

        try:
            ns.unregister('time_server')
        except:
            pass

        try:
            daemon.connect(serverobj, 'time_server')
            print "time_server connected"

            while True:
                print "Handling requests"
                daemon.handleRequests(1.0)
        except:
            raise
        finally:
            daemon.disconnect(serverobj)
            daemon.shutdown()

class RequestHandler(Pyro.core.ObjBase):

    def __init__(self):
        Pyro.core.ObjBase.__init__(self)


class TimeServer(RequestHandler):

    def __init__(self):
        super(TimeServer, self).__init__()


    def get_time(self):
        return datetime.datetime.now()

    def get_exception(self):
        raise Exception("An exception occuring remotely")


count = 0


def main():
    pyt = PyroThread()
    #    pyt.daemon = True
    main_loop = gobject.MainLoop()
    pyt.start()

#    delayedt1=threading.Timer(10, lambda : t1.start())
#    delayedt1.start()

    def process_loop():
        global count
        print "Process loop starts", count
        time.sleep(1)
        print "Process loop ends"
        count = count+1
        try:
            if count > 3:
                raise Exception
        except:
            main_loop.quit()
            print "Last thing"
            return False
        return True

    gobject.idle_add(process_loop)
    main_loop.run()
    print "Gata"

if __name__ == "__main__":
    main()
