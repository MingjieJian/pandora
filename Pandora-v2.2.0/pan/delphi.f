      subroutine DELPHI
     $(N,NSHL,CSHL,XASHL,YASHL,MRR,CDSK,XADSK,YADSK,XJNU,SUM1,SUM2,CSF,
     $ DUMP)
C
C     Rudolf Loeser, 1982 Feb 09
C---- Computes CSF from Jnu, for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CDSK, CSF, CSHL, SUM1, SUM2, XADSK, XASHL, XJNU, YADSK,
     $       YASHL
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
C     !DASH
      external ZERO1, TATAR, TICINUM, THELA, LABFIL, VECOUT, ARRADD,
     $         HI, BYE
C
C               CSHL(N,NSHL), XASHL(N,N,NSHL), YASHL(N,N,NSHL), SUM1(N),
      dimension CSHL(N,*),    XASHL(N,N,*),    YASHL(N,N,*),    SUM1(*),
C
C               CDSK(N,MRR ), XADSK(N,N,MRR ), YADSK(N,N,MRR ), SUM2(N),
     $          CDSK(N,*),    XADSK(N,N,*),    YADSK(N,N,*),    SUM2(*),
C
C               CSF(N), XJNU(N)
     $          CSF(*), XJNU(*)
C     !EJECT
C
      call HI ('DELPHI')
C     !BEG
C---- Compute SUM1 and SUM2
      call ZERO1     (SUM1, N)
      call ZERO1     (SUM2, N)
C
      I = 0
C     Shell part
      do 100 MM = 1,NSHL
        call TATAR   (I)
        call TICINUM (CSHL(1,MM), I, XASHL(1,1,MM), XJNU, SUM1)
        call THELA   (CSHL(1,MM), I, YASHL(1,1,MM),       SUM2)
  100 continue
C
C     Disk part
      do 101 MM = 1,MRR
        call TICINUM (CDSK(1,MM), N, XADSK(1,1,MM), XJNU, SUM1)
        call THELA   (CDSK(1,MM), N, YADSK(1,1,MM),       SUM2)
  101 continue
C
      if(DUMP) then
        call LABFIL  ('Angle-integral of XA*JNU', LINE)
        call VECOUT  (LUEO, SUM1, N, LINE)
C
        call LABFIL  ('Angle-integral of YA'    , LINE)
        call VECOUT  (LUEO, SUM2, N, LINE)
      end if
C
C---- Compute CSF
      call ARRADD    (SUM1, SUM2, CSF, N)
C     !END
      call BYE ('DELPHI')
C
      return
      end
