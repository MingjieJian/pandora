      subroutine ESSEN
     $(XNUU,XNUL,TE,BDU,BDL,S,DMPI,ITAU)
C
C     Rudolf Loeser, 2004 Apr 17
C---- Computes source function for built-in background contribution
C     lines.
C     !DASH
      save
C     !DASH
      real*8 BDL, BDU, CON7, DNU, ONE, RSET, S, SET, TE, XDEN, XNUL,
     $       XNUM, XNUU
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
      external RIGEL, DIVIDE, KUYUK, LINER, HI, BYE
C
      data MODE /1/
C
      call HI ('ESSEN')
C     !BEG
      call RIGEL   (7, CON7)
      DNU  = XNUU-XNUL
      XNUM = (DNU**3)*CON7
C
      call KUYUK   (DNU, MODE, BDU, BDL, TE, SET)
      call DIVIDE  (ONE, SET, RSET)
      XDEN = RSET-ONE
C
      call DIVIDE  (XNUM, XDEN, S)
C
      if(DMPI) then
        call LINER (1, LUEO)
        write (LUEO, 100) TE,ITAU,BDU,BDL,DNU,SET,XNUM,XDEN,S
  100   format(' ','Dump of S',T50,'TE =',1PE14.6,' at depth #',I5/
     $         ' ','BD(u) =',E14.6,', BD(l) =',E14.6,', DNU =',0PF10.6,
     $             ', SE-term =',1PE16.8/
     $         ' ','num =',E16.8,', den =',E16.8,';   S =',E16.8)
      end if
C     !END
      call BYE ('ESSEN')
C
      return
      end
