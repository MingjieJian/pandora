      subroutine OWEIN
     $(N,LG,XA,RR,CI,SUM,XJNU,DUMP)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Computes default XJNU, for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CI, ONE, RR, SUM, XA, XJNU
      integer I, LG, LUEO, MM, N
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
      external ZERO1, THELA, DIVIDE, LABFIL, VECOUT, HI, BYE
C
C               CI(N,LG), XA(N,N,LG), RR(N), SUM(N), XJNU(N)
      dimension CI(N,*),  XA(N,N,*),  RR(*), SUM(*), XJNU(*)
C
      call HI ('OWEIN')
C     !BEG
C---- Initialize
      call ZERO1    (SUM, N)
C
C---- Compute sum
      do 100 MM = 1,LG
        call THELA  (CI(1,MM), N, XA(1,1,MM), SUM)
  100 continue
C
      if(DUMP) then
        call LABFIL ('Angle-integral of XA', LINE)
        call VECOUT (LUEO, SUM, N, LINE)
      end if
C
C---- Compute XJNU
      do 101 I = 1,N
        call DIVIDE (RR(I), (ONE-SUM(I)), XJNU(I))
  101 continue
C     !END
      call BYE ('OWEIN')
C
      return
      end
