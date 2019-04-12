      subroutine MEACPT
     $(RECORD,LENGTH,IADDRS)
C     Rudolf Loeser, 1987 Dec 02
C---- Accepts a record into the "in-memory" scratch file.
C     !DASH
      save
C     !DASH
      real*8 RECORD, XLNGTH
      integer I, IADDRS, J, LAST, LENGTH
C     !COM
C---- CORTEX      as of 1997 Jun 16
      integer     MELEFT,MENEXT,MENRAC,MENRUP,MENRRE,MELIMT
      real*8      SIOBUF,SENWAC,SENWUP,SENWRE
C
      dimension   SIOBUF( 1 )
C     (The   r e a l   length of SIOBUF is set in PANDORA!)
C
      common      /CORTEX1/ MELEFT,MENEXT,MENRAC,MENRUP,MENRRE
      common      /CORTEX2/ SENWAC,SENWUP,SENWRE
      common      /CORTEX3/ MELIMT
      common      /CORTEX4/ SIOBUF
C
C     Control parameters for the MEMOIR subroutines:
C
C     MELIMT = length of entire buffer (SIOBUF), in words;
C     MELEFT = number of unused words left in buffer;
C     MENEXT = index of next available word in buffer;
C     MENRAC = number of logical records accepted;
C     MENRUP = number of logical records updated;
C     MENRRE = number of logical records returned;
C     SENWAC = number of words accepted;
C     SENWUP = number of words updated;
C     SENWRE = number of words returned.
C     .
C     !DASH
      dimension RECORD(*)
C     !EJECT
C
C     !BEG
      if(MELEFT.lt.LENGTH) then
        stop 'MEACPT: buffer full'
      end if
C
      IADDRS = MENEXT
      MENRAC = MENRAC+1
C
      if(LENGTH.gt.0) then
        XLNGTH = LENGTH
        SENWAC = SENWAC+XLNGTH
        MENEXT = MENEXT+LENGTH
        MELEFT = MELEFT-LENGTH
C
        LAST   = MENEXT-1
        J = 0
        do 100 I=IADDRS,LAST
          J = J+1
          SIOBUF(I) = RECORD(J)
  100   continue
C
      end if
C     !END
C
      return
      end
