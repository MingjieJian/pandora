      subroutine GILO
     $(N,LG,XA,YA,XJNU,CI,SUM1,SUM2,CSF,DUMP)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Computes CSF from JNU, for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CI, CSF, SUM1, SUM2, XA, XJNU, YA
      integer LG, LUEO, MM, N
      logical DUMP
      character LINE*127
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ZERO1, TICINUM, THELA, LABFIL, VECOUT, ARRADD, HI, BYE
C
C               CI(N,LG), XA(N,N,LG), YA(N,N,LG), SUM2(N), XJNU(N),
      dimension CI(N,*),  XA(N,N,*),  YA(N,N,*),  SUM2(*), XJNU(*),
C
C               SUM1(N), CSF(N)
     $          SUM1(*), CSF(*)
C
      call HI ('GILO')
C     !BEG
C---- Compute SUM1 and SUM2
      call ZERO1     (SUM1, N)
      call ZERO1     (SUM2, N)
C
      do 100 MM = 1,LG
        call TICINUM (CI(1,MM), N, XA(1,1,MM), XJNU, SUM1)
        call THELA   (CI(1,MM), N, YA(1,1,MM),       SUM2)
  100 continue
C
      if(DUMP) then
        call LABFIL  ('Angle-integral of XA*JNU', LINE)
        call VECOUT  (LUEO, SUM1, N, LINE)
        call LABFIL  ('Angle-integral of YA'    , LINE)
        call VECOUT  (LUEO, SUM2, N, LINE)
      end if
C
C---- Compute CSF
      call ARRADD    (SUM1, SUM2, CSF, N)
C     !END
      call BYE ('GILO')
C
      return
      end
