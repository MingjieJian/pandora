      subroutine VOOM
     $(AR,XNU,MRP,N,TE,RNU,JLEV,NL,WJ,UJ,UPJ,KSHL,KODE)
C
C     Rudolf Loeser, 1974 Jun 14
C---- Does some precalculations, for MANU.
C     When KODE=1, AR = Jnu, an array of the form (N,MRP);
C     when KODE=2, AR = TR , an array of the form (N,NL).
C     (This is version 2 of VOOM.)
C     !DASH
      save
C     !DASH
      real*8 AR, ARG, DIV, R, RNU, SARG, TE, UJ, UPJ, WJ, XK, XNU, Z,
     $       ZERO
      integer I, JLEV, KODE, MRP, N, NL
      logical KSHL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  KONE, DIVIDE, HI, BYE
      intrinsic abs
C
C               TE(N), RNU(MRX+1), UJ(N), UPJ(N), AR(N,#), XNU(NSL)
      dimension TE(*), RNU(*),     UJ(*), UPJ(*), AR(N,*), XNU(*)
C
      call HI ('VOOM')
C     !BEG
      call KONE         (XNU(JLEV),KSHL,WJ,Z)
      do 100 I = 1,N
        call DIVIDE     (Z,TE(I),UJ(I))
  100 continue
C
      if(KODE.eq.2) then
        do 101 I = 1,N
          call DIVIDE   (Z,AR(I,JLEV),UPJ(I))
  101   continue
      else
C
        XK  = RNU(MRP)
        DIV = WJ*(XK**3)
        do 102 I = 1,N
          call DIVIDE   (AR(I,MRP),DIV,ARG)
          if(ARG.ne.ZERO) then
            SARG = log(abs(ARG))
            call DIVIDE (SARG,XK,R)
            UPJ(I) = -R
          else
            UPJ(I) = ZERO
          end if
  102   continue
C
      end if
C     !END
      call BYE ('VOOM')
C
      return
      end
