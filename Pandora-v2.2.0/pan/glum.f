      subroutine GLUM
     $(NO)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Prints a RHO-fudging summary.
C     (This is version 4 of GLUM.)
C     !DASH
      save
C     !DASH
      real*8 ZERO
      integer I, NO
C     !COM
C---- SELGI       as of 1999 Sep 13
      integer     KNFMX
      parameter   (KNFMX=50)
C     (Remember to recompile all users when changing KNFMX)
      integer     KNTF,INF1,INF2,INF3,INF4
      real*8      FUJJ,FVAL
      dimension   INF1(KNFMX),INF2(KNFMX),INF3(KNFMX),INF4(KNFMX),
     $            FUJJ(KNFMX),FVAL(KNFMX)
      common      /SELGI1/ KNTF,INF1,INF2,INF3,INF4
      common      /SELGI2/ FUJJ,FVAL
C     Saves B-ratios computation fudging data, for later printing.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external NOTCH, HI, BYE
C
      call HI ('GLUM')
C     !BEG
      if((NO.gt.0).and.(KNTF.gt.0)) then
        do 100 I = 1,KNTF
          if((FUJJ(I).ne.ZERO).or.(FVAL(I).ne.ZERO)) then
            call NOTCH (NO,INF1(I),INF2(I),INF3(I),INF4(I),FUJJ(I),
     $                  FVAL(I))
          end if
  100   continue
      end if
C     !END
      call BYE ('GLUM')
C
      return
      end
