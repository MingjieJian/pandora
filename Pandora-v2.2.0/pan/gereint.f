      subroutine GEREINT
     $(N,RR,NSHL,CSHL,XASHL,MRR,CDSK,XADSK,SUM,XJNU,DUMP)
C
C     Rudolf Loeser, 1982 Feb 09
C---- Computes default XJNU, for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CDSK, CSHL, ONE, RR, SUM, XADSK, XASHL, XDEN, XJNU
      integer I, LUEO, MM, MRR, N, NSHL
      logical DUMP
      character LINE*127
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
      external ZERO1, TATAR, THELA, DIVIDE, LABFIL, VECOUT, HI, BYE
C
C               CSHL(N,NSHL), XASHL(N,N,NSHL), RR(N), SUM(N), XJNU(N),
      dimension CSHL(N,*),    XASHL(N,N,*),    RR(*), SUM(*), XJNU(*),
C
C               CDSK(N,MRR ), XADSK(N,N,MRR )
     $          CDSK(N,*),    XADSK(N,N,*)
C     !EJECT
C
      call HI ('GEREINT')
C     !BEG
C---- Compute sum
      call ZERO1    (SUM, N)
C
C     Shell part
      I = 0
      do 100 MM = 1,NSHL
        call TATAR  (I)
        call THELA  (CSHL(1,MM), I, XASHL(1,1,MM), SUM)
  100 continue
C
C     Disk part
      do 101 MM = 1,MRR
        call THELA  (CDSK(1,MM), N, XADSK(1,1,MM), SUM)
  101 continue
C
      if(DUMP) then
        call LABFIL ('Angle-integral of YA', LINE)
        call VECOUT (LUEO, SUM, N, LINE)
      end if
C
C---- Compute XJNU
      do 102 I = 1,N
        XDEN = ONE-SUM(I)
        call DIVIDE (RR(I), XDEN, XJNU(I))
  102 continue
C     !END
      call BYE ('GEREINT')
C
      return
      end
