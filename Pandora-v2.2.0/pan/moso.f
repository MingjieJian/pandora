      subroutine MOSO
     $(LU,IBNVW,N,NL,BDIJ,XNKPR,XNDPR,BDIPR)
C
C     Rudolf Loeser, 2003 Jun 26
C---- Prints initial values, for MYTU.
C     (This is version 2 of MOSO.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, BDIPR, XNDPR, XNKPR, dummy
      integer IBNVW, LU, N, NL
      character LABB*15, LABK*15, LABN*15, LABR*15, qummy*8
C     !DASH
      external LINER, MOTE, HI, BYE
C
C               BDIJ(N,NL), XNDPR(N,NL), BDIPR(N,NL), XNKPR(N)
      dimension BDIJ(N,*),  XNDPR(N,*),  BDIPR(N,*),  XNKPR(*)
C
      data LABR,LABB /'   BDratio', '   BD(old)'/
      data LABK,LABN /'   NK(old)', '   ND(old)'/
C
      call HI ('MOSO')
C     !BEG
      if(LU.gt.0) then
        call LINER (1, LU)
        write (LU,100)
  100   format(' ','Values of BD-ratio (cf. preceding output section ',
     $             'RHO AND RBD).')
        call MOTE  (LU, NL, 1, 1, IBNVW, N, BDIJ, LABR, dummy, qummy,
     $              dummy, qummy)
C
        call LINER (1, LU)
        write (LU,101)
  101   format(' ','Initial (old) values of NK (ionized number ',
     $             'density), ND (level population), and BD ',
     $             '(departure coefficient).')
C
        call LINER (1, LU)
        write (LU,102) LABK,XNKPR(IBNVW)
  102   format(' ',A15,1PE16.8)
C
        call MOTE  (LU, NL, 2, 0, IBNVW, N, XNDPR, LABN, BDIPR, LABB,
     $              dummy, qummy)
      end if
C     !END
      call BYE ('MOSO')
C
      return
      end
