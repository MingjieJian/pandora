      subroutine FAGNI
     $(PL,PU,TEX,DNU,FAC,DMP)
C
C     Rudolf Loeser, 1991 May 08
C---- Computes FAC for DAMALA.
C     !DASH
      save
C     !DASH
      real*8 DNU, EX, FAC, HNUKT, ONE, PL, PU, REX, RP, TEX
      integer LUEO
      logical DMP
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
      external PROD, DIVIDE, HI, BYE
C
      call HI ('FAGNI')
C     !BEG
      call PROD   (TEX, DNU, 1, HNUKT, EX)
      call DIVIDE (ONE, EX, REX)
      call DIVIDE (PL, PU, RP)
C
      FAC = RP*REX
C
      if(DMP) then
        write (LUEO,100) PL,PU,TEX,DNU,FAC
  100   format(' ',6X,'P(l)=',1PE12.4,2X,'P(u)=',E12.4,2X,'TEX=',E16.8,
     $             2X,'delta nu=',E16.8,2X,'Term=',E16.8)
      end if
C     !END
      call BYE ('FAGNI')
C
      return
      end
