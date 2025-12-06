# Network Protocol Specification

**Navigation**: [README](README.md) | [Keyboard Protocol](keyboard-protocol.md) | [Mouse Protocol](mouse-protocol.md) | [File System](file-system.md)

Complete specification of network communication protocols, including Ethernet and Internet (TCP/IP) protocols.

## Overview

The network subsystem provides network communication capabilities, supporting both Ethernet (raw packets) and Internet (TCP/IP) protocols.

## Network Types

### Ethernet

Raw Ethernet packet communication:

- **Purpose**: Low-level network communication
- **Protocol**: Raw Ethernet frames
- **Use**: Interlisp network protocols

### Internet (TCP/IP)

Standard TCP/IP communication:

- **Purpose**: Standard network communication
- **Protocol**: TCP sockets
- **Use**: General network I/O

## Ethernet Protocol

### Ethernet Packet Format

```pseudocode
struct EthernetPacket:
    destination: uint48      // Destination MAC address (6 bytes)
    source: uint48           // Source MAC address (6 bytes)
    type: uint16             // EtherType (protocol type)
    data: bytes[]            // Packet payload
    checksum: uint16         // Checksum (optional)
```

### Ethernet Address

```pseudocode
struct EthernetAddress:
    bytes: array[6] of uint8  // 48-bit MAC address
```

### Ethernet Operations

**Initialize Ethernet**:

```pseudocode
function InitializeEthernet():
    // Open Ethernet interface
    ether_fd = OpenEthernetInterface()

    // Get host MAC address
    ether_host = GetMACAddress()

    // Initialize interface page
    InterfacePage->nshost0 = (ether_host[0] << 8) | ether_host[1]
    InterfacePage->nshost1 = (ether_host[2] << 8) | ether_host[3]
    InterfacePage->nshost2 = (ether_host[4] << 8) | ether_host[5]
```

**Send Packet**:

```pseudocode
function SendEthernetPacket(destination, packet_data, length):
    // Build Ethernet frame
    frame = BuildEthernetFrame(destination, ether_host, packet_data, length)

    // Send frame
    bytes_sent = Write(ether_fd, frame, frame_length)

    return bytes_sent
```

**Receive Packet**:

```pseudocode
function ReceiveEthernetPacket(buffer, max_length):
    // Read Ethernet frame
    frame = Read(ether_fd, max_length + ETHERNET_HEADER_SIZE)

    if frame == null:
        return 0

    // Extract packet data
    packet_data = ExtractPacketData(frame)

    // Copy to buffer
    CopyToBuffer(buffer, packet_data)

    return packet_data.length
```

### Checksum Calculation

```pseudocode
function CalculateChecksum(data, length, initial_sum):
    checksum = initial_sum or 0

    for i = 0 to length - 1:
        checksum = checksum + data[i]
        if checksum > 0xFFFF:
            checksum = (checksum & 0xFFFF) + 1  // Add carry

        // Rotate left 1 bit
        if checksum > 0x7FFF:
            checksum = ((checksum & 0x7FFF) << 1) | 1
        else:
            checksum = checksum << 1

    if checksum == 0xFFFF:
        return 0  // Checksum valid
    else:
        return checksum
```

## TCP/IP Protocol

### TCP Socket Operations

**Create Socket**:

```pseudocode
function CreateTCPSocket():
    socket_fd = Socket(AF_INET, SOCK_STREAM, 0)
    return socket_fd
```

**Connect**:

```pseudocode
function TCPConnect(hostname_or_address, port):
    // Resolve hostname
    if IsNumericAddress(hostname_or_address):
        address = ParseNumericAddress(hostname_or_address)
    else:
        host = GetHostByName(hostname_or_address)
        address = host->h_addr

    // Set up socket address
    sockaddr.sin_family = AF_INET
    sockaddr.sin_addr.s_addr = address
    sockaddr.sin_port = htons(port)

    // Connect
    result = Connect(socket_fd, sockaddr, sizeof(sockaddr))

    if result < 0:
        return NIL

    // Set non-blocking
    SetNonBlocking(socket_fd)

    return socket_fd
```

**Send Data**:

```pseudocode
function TCPSend(socket_fd, buffer, length):
    // Byte swap if needed
    if BYTESWAP:
        SwapBytes(buffer, length)

    bytes_sent = Send(socket_fd, buffer, length, 0)

    // Byte swap back
    if BYTESWAP:
        SwapBytes(buffer, length)

    if bytes_sent < 0:
        return NIL

    return bytes_sent
```

**Receive Data**:

```pseudocode
function TCPReceive(socket_fd, buffer, max_length):
    bytes_received = Receive(socket_fd, buffer, max_length, 0)

    if bytes_received < 0:
        if errno == EWOULDBLOCK:
            return ATOM_T  // No data available
        else:
            return NIL  // Error

    // Byte swap if needed
    if BYTESWAP:
        SwapBytes(buffer, bytes_received)

    return bytes_received
```

**Close Socket**:

```pseudocode
function TCPClose(socket_fd):
    Close(socket_fd)
    return T
```

## Network Event Handling

### Event Detection

```pseudocode
function CheckNetworkEvents():
    // Check Ethernet events
    if ether_fd >= 0:
        if HasEthernetData(ether_fd):
            SetInterruptFlag(ETHERInterrupt)
            ETHEREventCount++

    // Check TCP events
    for socket in open_tcp_sockets:
        if HasTCPData(socket):
            SetInterruptFlag(IOInterrupt)
```

### Event Processing

```pseudocode
function ProcessNetworkEvents():
    // Process Ethernet packets
    if ETHEREventCount > 0:
        ProcessEthernetPackets()
        ETHEREventCount--

    // Process TCP data
    ProcessTCPData()
```

## Platform-Specific Implementations

### Ethernet Backends

**DLPI (Data Link Provider Interface)**:

- Used on Solaris
- Raw packet access
- Packet filtering

**NIT (Network Interface Tap)**:

- Used on older Unix systems
- Raw packet access
- Packet filtering

**Nethub**:

- TCP-based Ethernet emulation
- Connects to nethub server
- XIP packet format

### TCP/IP Backends

**Standard Sockets**:

- POSIX socket API
- Works on Unix, Linux, macOS, Windows
- Standard TCP/IP

## Related Documentation

- [File System](file-system.md) - Network file access
- [VM Core](../vm-core/interrupt-handling.md) - Network interrupt handling
- [Platform Abstraction](../platform-abstraction/) - Platform-specific networking
