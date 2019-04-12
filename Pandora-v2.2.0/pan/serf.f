      subroutine SERF
     $(NO,NL,N,HND,XNE,POP,POPK,KODE,TIT,NT,LIMP,IPOP,IPSW,BD,LC,KOTE,
     $ PRINTN,PRINTB)
C
C     Rudolf Loeser, 1972 Aug 18
C---- Prints Level Populations and Departure Coefficients.
C     (This is version 2 of SERF.)
C     !DASH
      save
C     !DASH
      real*8 BD, HND, POP, POPK, XNE
      integer IOVER, IPOP, IPSW, KODE, KOTE, LC, LIMP, LIMPLIM, LU, N,
     $        NL, NO, NT
      logical PRINTB, PRINTN
      character LINE*9, TIT*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C     !DASH
      external HALT, ZEUS, PRIAM, FUNGUS, MILDEW, PELLA, HI, BYE
C
C               HND(N), XNE(N), POP(N,LIMP), POPK(N), BD(N,LIMP)
      dimension HND(*), XNE(*), POP(*),      POPK(*), BD(*)
C
      parameter (LIMPLIM=50)
      dimension LINE(LIMPLIM)
C     !EJECT
C
      call HI ('SERF')
C     !BEG
      if(LIMP.gt.LIMPLIM) then
C       Don't print if too much data
        write (MSSLIN(1),100) LIMP,LIMPLIM
  100   format('LIMP =',I12,' is greater than ',I2,
     $         'which is unacceptable.')
        call HALT    ('SERF', 1)
      end if
C
      call ZEUS      (NO, IPSW, LU)
      if(LU.gt.0) then
C----   Print header
        call PRIAM   (LU, TIT, NT)
C----   Print number densities
        call MILDEW  (LU, XNE, HND, N, POPK, POP, NL, KODE, LINE, LIMP,
     $                IOVER, PRINTN)
        if(KOTE.gt.0) then
C----     Print departure coefficients
          call PELLA (LU, LC, LIMP, N, BD, IOVER, LINE, PRINTB)
        end if
C----   Print ion-model data (but only once per run)
        call FUNGUS  (LU, IPOP, TIT)
      end if
C     !END
      call BYE ('SERF')
C
      return
      end
