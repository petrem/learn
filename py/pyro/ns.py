import Pyro.core
import Pyro.naming
import Pyro.errors

def main():
    Pyro.core.initServer()
    daemon = Pyro.core.Daemon(host="", port=Pyro.config.PYRO_NS_PORT, norange=True)
    persistent_dir = "nsstorage"
    try:
        #name_server = Pyro.naming.PersistentNameServer(persistent_dir)
        name_server = Pyro.naming.NameServer()
        daemon.useNameServer(name_server)
        uri = daemon.connectPersistent(name_server, Pyro.constants.NAMESERVER_NAME)
        print "Listening on %s" % uri
        #print "Persisting database in %s" % name_server.getDBDir()

        while True:
            daemon.handleRequests(timeout=1)

    except KeyboardInterrupt:
        pass
    finally:
        try:
            daemon.disconnect(name_server)
        except Pyro.errors.PyroError:
            print "cannot disconnect name_server"

if __name__ == "__main__":
        main()
