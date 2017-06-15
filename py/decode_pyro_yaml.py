import pickle
import struct
import sys
import zlib

import Pyro.protocol

import yaml


headerFmt = Pyro.protocol.PYROAdapter.headerFmt
headerSize = Pyro.protocol.PYROAdapter.headerSize
pyroFlags = {
    Pyro.protocol.PFLG_CHECKSUM: 'CHECKSUM',
    Pyro.protocol.PFLG_COMPRESSED: 'COMPRESSED',
    Pyro.protocol.PFLG_XMLPICKLE_GNOSIS: 'XMLPICKLE_GNOSIS',
}
pyroMsgTypes = (
    'CONNECT',
    'GRANTED',
    'DENIED',
)


def convert_message(data):
    if len(data) >= headerSize:
        return (struct.unpack(headerFmt, data[:headerSize]),
                data[headerSize:])
    else:
        return ((), data)


def print_message(message, peer):

    def _decode_flags(flags):
        rem = flags
        for flag in pyroFlags:
            if flags & flag:
                yield pyroFlags[flag]
            rem = rem & (0xff & ~flag)
        if rem:
            raise RuntimeError('Unknown flags remaining: %x' % rem)

    def _decode_body(body):
        for type_ in pyroMsgTypes:
            if body.startswith(type_):
                return type_
                break
        return 'Unknown message type: %s' % body

    print peer, ':'
    header, body = message
    if header:
        header_id, version, hsize, bsize, flags, crc = header
        print '\tHeader: ver: %s, hsize: %s, bsize: %s, flags: %s, CRC' % (
            version, hsize, bsize, ','.join(_decode_flags(flags))
        ),
        if crc == zlib.adler32(body):
            print 'OK'
        else:
            print 'NOK'
    else:
        print '\t No Header'
    print '\tBody:', _decode_body(body)


def main(argv):
    if not argv:
        yaml_stream = sys.stdin
    else:
        yaml_stream = file(argv[0], 'rb')
    pyro_yaml = yaml.load(yaml_stream)
    conversation = [0] * len(pyro_yaml)
    for peer_and_index, data in pyro_yaml.iteritems():
        # 'peer1_5', 'peer0_5'...
        peer, index = peer_and_index[4:].split('_')
        peer = int(peer)
        index = int(index)
        # for some reason, peer1 is the first one
        conversation[index * 2 + 1 ^ peer] = (
            peer,
            convert_message(data))

    for peer, message in conversation:
        print_message(message, peer)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
