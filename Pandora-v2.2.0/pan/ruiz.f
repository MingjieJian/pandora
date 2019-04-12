      subroutine RUIZ
     $(XNEL,HNDL,XNHML,HN1L,SIG,IBEG,IEND,BOT,TOP)
C
C     Rudolf Loeser, 1982 May 12
C---- Sets up ordinate limits, for MELANIE.
C     !DASH
      save
C     !DASH
      real*8 BOT, HN1L, HNDL, ONE, SIG, TOP, XNEL, XNHML, ZAX, ZIN
      integer IBEG, IEND, KNT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external CHROME, ABOVED, BELOWD, HI, BYE
C
C               XNEL(N), HNDL(N), XNHML(N), HN1L(N)
      dimension XNEL(*), HNDL(*), XNHML(*), HN1L(*)
C
      call HI ('RUIZ')
C     !BEG
      ZIN = +ZZLARGE
      ZAX = -ZZLARGE
      KNT = IEND-(IBEG-1)
      call CHROME (KNT,1,XNEL (IBEG),1,SIG,ZAX,ZIN)
      call CHROME (KNT,1,HNDL (IBEG),1,SIG,ZAX,ZIN)
      call CHROME (KNT,1,XNHML(IBEG),1,SIG,ZAX,ZIN)
      call CHROME (KNT,1,HN1L (IBEG),1,SIG,ZAX,ZIN)
      call BELOWD (ZIN,ONE,BOT)
      call ABOVED (ZAX,ONE,TOP)
C     !END
      call BYE ('RUIZ')
C
      return
      end
