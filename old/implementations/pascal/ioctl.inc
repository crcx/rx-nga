// *******************************************************
//  Copyright (c)} 2016 Rob Judd <judd@ob-wan.com>
//  ISC License - see included file LICENSE
// *******************************************************

type winsize = record
  ws_row    : Word;  // rows, in characters 
  ws_col    : Word;  // columns, in characters 
  ws_xpixel : Word;  // horizontal size, pixels 
  ws_ypixel : Word;  // vertical size, pixels 
end;

type ttysize = record
  ts_lines : Word;
  ts_cols  : Word;
  ts_xxx   : Word;
  ts_yyy   : Word;
end;

{$define TIOCGSIZE TIOCGWINSZ}
{$define TIOCSSIZE TIOCSWINSZ}


// Ioctl's have the command encoded in the lower word, and the size of
// any in or out parameters in the upper word.  The high 3 bits of the
// upper word are used to encode the in/out status of the parameter.
 
{$define IOCPARM_MASK        := $1fff}    // parameter length, at most 13 bits 
{$define IOCPARM_LEN(x)      := (((x) shr 16) and IOCPARM_MASK)}
{$define IOCBASECMD(x)       := ((x) and not(IOCPARM_MASK)}
{$define IOCGROUP(x)         := (((x) shr 8) and $ff)}

{$define IOCPARM_MAX := NBPG}             // max size of ioctl, mult. of NBPG 
{$define IOC_VOID    :=        $20000000} // no parameters 
{$define IOC_OUT     :=        $40000000} // copy out parameters 
{$define IOC_IN      :=        $80000000} // copy in parameters 
{$define IOC_INOUT   := (IOC_IN or IOC_OUT)}
{$define IOC_DIRMASK :=        $E0000000} // mask for IN/OUT/VOID 

{$define _IOC(inout,group,num,len) := (inout or ((len & IOCPARM_MASK) shl 16) or ((group) shl 8) or (num))}
{$define _IO(g,n)    := _IOC(IOC_VOID, (g), (n), 0)}
{$define _IOR(g,n,t) := _IOC(IOC_OUT, (g), (n), sizeof(t))}
{$define _IOW(g,n,t) := _IOC(IOC_IN, (g), (n), sizeof(t))}
// this should be _IORW, but stdio got there first 
{$define _IOWR(g,n,t) := _IOC(IOC_INOUT, (g), (n), sizeof(t))}

{$define TIOCMODG    := _IOR('t', 3, int)}  // get modem control state 
{$define TIOCMODS    := _IOW('t', 4, int)}  // set modem control state 
{$define TIOCM_LE            :=      0001}  // line enable 
{$define TIOCM_DTR           :=      0002}  // data terminal ready 
{$define TIOCM_RTS           :=      0004}  // request to send 
{$define TIOCM_ST            :=      0010}  // secondary transmit 
{$define TIOCM_SR            :=      0020}  // secondary receive 
{$define TIOCM_CTS           :=      0040}  // clear to send 
{$define TIOCM_CAR           :=      0100}  // carrier detect 
{$define TIOCM_CD            := TIOCM_CAR}
{$define TIOCM_RNG           :=      0200}  // ring 
{$define TIOCM_RI            := TIOCM_RNG}
{$define TIOCM_DSR           :=      0400}  // data set ready 
      // 8-10 compat 
{$define TIOCEXCL         := _IO('t', 13)}  // set exclusive use of tty 
{$define TIOCNXCL         := _IO('t', 14)}  // reset exclusive use of tty 
      // 15 unused 
{$define TIOCFLUSH := _IOW('t', 16, int)}   // flush buffers 
      // 17-18 compat 
{$define TIOCGETA  := _IOR('t', 19, struct termios)} // get termios struct 
{$define TIOCSETA  := _IOW('t', 20, struct termios)} // set termios struct 
{$define TIOCSETAW := _IOW('t', 21, struct termios)} // drain output, set 
{$define TIOCSETAF := _IOW('t', 22, struct termios)} // drn out, fls in, set 
{$define TIOCGETD  := _IOR('t', 26, int)}  // get line discipline 
{$define TIOCSETD  := _IOW('t', 27, int)}  // set line discipline 
      // 127-124 compat 
{$define TIOCSBRK        := _IO('t', 123)} // set break bit 
{$define TIOCCBRK        := _IO('t', 122)} // clear break bit 
{$define TIOCSDTR        := _IO('t', 121)} // set data terminal ready 
{$define TIOCCDTR        := _IO('t', 120)} // clear data terminal ready 
{$define TIOCGPGRP := _IOR('t', 119, int)} // get pgrp of tty 
{$define TIOCSPGRP := _IOW('t', 118, int)} // set pgrp of tty 
      // 117-116 compat 
{$define TIOCOUTQ := _IOR('t', 115, int)} // output queue size 
{$define TIOCSTI := _IOW('t', 114, char)} // simulate terminal input 
{$define TIOCNOTTY      := _IO('t', 113)} // void tty association 
{$define TIOCPKT  := _IOW('t', 112, int)} // pty: set/clear packet mode 
{$define TIOCPKT_DATA             := $00} // data packet 
{$define TIOCPKT_FLUSHREAD        := $01} // flush packet 
{$define TIOCPKT_FLUSHWRITE       := $02} // flush packet 
{$define TIOCPKT_STOP             := $04} // stop output 
{$define TIOCPKT_START            := $08} // start output 
{$define TIOCPKT_NOSTOP           := $10} // no more ^S, ^Q 
{$define TIOCPKT_DOSTOP           := $20} // now do ^S ^Q 
{$define TIOCPKT_IOCTL            := $40} // state change of pty driver 
{$define TIOCSTOP       := _IO('t', 111)} // stop output, like ^S 
{$define TIOCSTART      := _IO('t', 110)} // start output, like ^Q 
{$define TIOCMSET := _IOW('t', 109, int)} // set all modem bits 
{$define TIOCMBIS := _IOW('t', 108, int)} // bis modem bits 
{$define TIOCMBIC := _IOW('t', 107, int)} // bic modem bits 
{$define TIOCMGET := _IOR('t', 106, int)} // get all modem bits 
{$define TIOCREMOTE := _IOW('t', 105, int)} // remote input editing 
{$define TIOCGWINSZ := _IOR('t', 104, struct winsize)} // get window size 
{$define TIOCSWINSZ := _IOW('t', 103, struct winsize)} // set window size 
{$define TIOCUCNTL             := _IOW('t', 102, int)} // pty: set/clr usr cntl mode 
{$define UIOCCMD(n)      := _IO('u', n)}  // usr cntl op "n" 
{$define TIOCCONS := _IOW('t', 98, int)}  // become virtual console 
{$define TIOCSCTTY      := _IO('t', 97)}  // become controlling tty 
{$define TIOCEXT  := _IOW('t', 96, int)}  // pty: external processing 
{$define TIOCSIG        := _IO('t', 95)}  // pty: generate signal 
{$define TIOCDRAIN      := _IO('t', 94)}  // wait till output drained 

{$define TTYDISC  := 0}  // termios tty line discipline 
{$define TABLDISC := 3}  // tablet discipline 
{$define SLIPDISC := 4}  // serial IP discipline 


{$define FIOCLEX                 := _IO('f', 1)}         // set close on exec on fd 
{$define FIONCLEX                := _IO('f', 2)}         // remove close on exec 
{$define FIONREAD                := _IOR('f', 127, int)} // get # bytes to read 
{$define FIONBIO                 := _IOW('f', 126, int)} // set/clear non-blocking i/o 
{$define FIOASYNC                := _IOW('f', 125, int)} // set/clear async i/o 
{$define FIOSETOWN               := _IOW('f', 124, int)} // set owner 
{$define FIOGETOWN               := _IOR('f', 123, int)} // get owner 

// socket i/o controls 
{$define SIOCSHIWAT               := _IOW('s',  0, int)}  // set high watermark 
{$define SIOCGHIWAT               := _IOR('s',  1, int)}  // get high watermark 
{$define SIOCSLOWAT               := _IOW('s',  2, int)}  // set low watermark 
{$define SIOCGLOWAT               := _IOR('s',  3, int)}  // get low watermark 
{$define SIOCATMARK               := _IOR('s',  7, int)}  // at oob mark? 
{$define SIOCSPGRP                := _IOW('s',  8, int)}  // set process group 
{$define SIOCGPGRP                := _IOR('s',  9, int)}  // get process group 

{$define SIOCADDRT    := _IOW('r', 10, struct ortentry)}  // add route 
{$define SIOCDELRT    := _IOW('r', 11, struct ortentry)}  // delete route 

{$define SIOCSIFADDR     := _IOW('i', 12, struct ifreq)}  // set ifnet address 
{$define OSIOCGIFADDR    := _IOWR('i',13, struct ifreq)}  // get ifnet address 
{$define SIOCGIFADDR     := _IOWR('i',33, struct ifreq)}  // get ifnet address 
{$define SIOCSIFDSTADDR  := _IOW('i', 14, struct ifreq)}  // set p-p address 
{$define OSIOCGIFDSTADDR := _IOWR('i',15, struct ifreq)}  // get p-p address 
{$define SIOCGIFDSTADDR  := _IOWR('i',34, struct ifreq)}  // get p-p address 
{$define SIOCSIFFLAGS    := _IOW('i', 16, struct ifreq)}  // set ifnet flags 
{$define SIOCGIFFLAGS    := _IOWR('i',17, struct ifreq)}  // get ifnet flags 
{$define OSIOCGIFBRDADDR := _IOWR('i',18, struct ifreq)}  // get broadcast addr 
{$define SIOCGIFBRDADDR  := _IOWR('i',35, struct ifreq)}  // get broadcast addr 
{$define SIOCSIFBRDADDR  := _IOW('i',19, struct ifreq)}   // set broadcast addr 
{$define OSIOCGIFCONF    := _IOWR('i',20, struct ifconf)} // get ifnet list 
{$define SIOCGIFCONF     := _IOWR('i',36, struct ifconf)} // get ifnet list 
{$define OSIOCGIFNETMASK := _IOWR('i',21, struct ifreq)}  // get net addr mask 
{$define SIOCGIFNETMASK  := _IOWR('i',37, struct ifreq)}  // get net addr mask 
{$define SIOCSIFNETMASK  := _IOW('i',22, struct ifreq)}   // set net addr mask 
{$define SIOCGIFMETRIC   := _IOWR('i',23, struct ifreq)}  // get IF metric 
{$define SIOCSIFMETRIC   := _IOW('i',24, struct ifreq)}   // set IF metric 
{$define SIOCDIFADDR     := _IOW('i',25, struct ifreq)}   // delete IF addr 
{$define SIOCAIFADDR     := _IOW('i',26, struct ifaliasreq)} // add/chg IF alias 

{$define SIOCSARP        := _IOW('i', 30, struct arpreq)} // set arp entry 
{$define OSIOCGARP       := _IOWR('i',31, struct arpreq)} // get arp entry 
{$define SIOCGARP        := _IOWR('i',38, struct arpreq)} // get arp entry 
{$define SIOCDARP        := _IOW('i', 32, struct arpreq)} // delete arp entry 

{$ifndef KERNEL}

type
  ioctl =  Integer;

{$endif} // !KERNEL 
