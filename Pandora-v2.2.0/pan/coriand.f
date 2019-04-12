      subroutine CORIAND
     $(QNAME,A,NPL,KODE)
C
C     Rudolf Loeser, 1980 Mar 04
C---- Reads MINK-type indexed arrays.
C
C     KODE=1 means "A" is RRCPNJ (length MRS);
C          2       "A" is YRATE, RQCPNJ (length MRS);
C          3       "A" is RKC, TKR, YKR (lengths MLS); and
C          4       "A" is WRAT (length MRS+NSL+1).
C
C     "NPL" can be either MRJ (length NSL) or LRJ (length NL).
C
C     (This is version 3 of CORIAND.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer IN, IP, JLEV, KODE, MODE, NPL
      character QNAME*8
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MINT, MINK, BASIL, LANCER, HALT, HI, BYE
C
      dimension A(*), NPL(*)
C     !EJECT
C
      call HI ('CORIAND')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.4)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1, 2, 3 or 4.')
        call HALT  ('CORIAND', 1)
      end if
C
      call MINT    (QNAME, JLEV)
      call MINK    (JLEV, NPL, IN, IP)
C
      call LANCER  (JLEV,MODE)
      if((MODE.lt.1).or.(MODE.gt.2)) then
        write (MSSLIN(1),101) MODE
  101   format('MODE =',I12,', which is not 1 or 2.')
        call HALT  ('CORIAND', 1)
      end if
C
      if((KODE.eq.1).or.((KODE.eq.4).and.(MODE.eq.1))) then
        call BASIL (A(IP+1), NPL(JLEV)  , QNAME)
C
      else if((KODE.eq.2).or.((KODE.eq.4).and.(MODE.eq.2))) then
        call BASIL (A(IP)  , NPL(JLEV)+1, QNAME)
C
      else if(KODE.eq.3) then
        call BASIL (A(IN)  , NPL(JLEV)  , QNAME)
C
      end if
C     !END
      call BYE ('CORIAND')
C
      return
      end
