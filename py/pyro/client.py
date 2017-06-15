import Pyro.core
import Pyro.naming


def main():
    Pyro.core.initClient()
    locator = Pyro.naming.NameServerLocator()
    ns = locator.getNS()

    uri = ns.resolve('time_server')
    timeserver = Pyro.core.getProxyForURI(uri)

    print "time we've got:", timeserver.get_time()
    # timeserver.get_exception()

if __name__ == "__main__":
    main()
