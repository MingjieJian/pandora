      subroutine GITANA
     $(TE,V,XNUU,XNUL,XMASS,PU,PL,AUL,XNL,BDU,BDL,DW,GTN,DMPI,ITAU)
C
C     Rudolf Loeser, 2004 Apr 19
C---- Computes DW and GTN for built-in background contributor lines.
C     !DASH
      save
C     !DASH
      real*8 AUL, BDL, BDU, BRAK, CON41, DNU, DW, GTN, ONE, PL, PU, RT,
     $       SET, T22, TE, TERM, V, XDEN, XMASS, XNL, XNUL, XNUM, XNUU
      integer ITAU, LUEO, MODE
      logical DMPI
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external DOPPLER, RIGEL, DIVIDE, KUYUK, LINER, HI, BYE
C
      data T22,MODE /1.D-22, 1/
C
      call HI ('GITANA')
C     !BEG
      DNU = XNUU-XNUL
      call DOPPLER (DNU, TE, XMASS, (V**2), RT, DW)
C
      call RIGEL   (41, CON41)
      call KUYUK   (DNU, MODE, BDU, BDL, TE, SET)
      BRAK = ONE-SET
      XNUM = PU*AUL*XNL
      XDEN = PL*(DNU**4)*DW
      call DIVIDE  (XNUM, XDEN, TERM)
      GTN = (CON41*T22)*TERM*BRAK
C
      if(DMPI) then
        call LINER (1, LUEO)
        write (LUEO,100) ITAU,DNU,TE,V,XNL,BDU,BDL,DW,SET,XNUM,XDEN,GTN
  100   format(' ','Dump of DW and GTN',T50,'at depth #',I5,20X,
     $             'DNU =',F10.6/
     $         ' ','TE =',1PE14.6,', V =',E14.6,', N(l) =',E14.6,
     $             ', BD(u) =',E14.6,', BD(l) =',E14.6/
     $         ' ','DW =',E16.8,';    SE-term =',E14.6,', num =',E14.6,
     $             ', den =',E14.6,';   GTN =',E16.8)
      end if
C     !END
      call BYE ('GITANA')
C
      return
      end
