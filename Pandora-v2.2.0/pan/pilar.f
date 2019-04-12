      subroutine PILAR
     $(NO,KILROY,ZLAM,WT,OPAC,I,K)
C
C     Rudolf Loeser, 1979 Jan 11
C---- Produces a dump of raw Statistical Line Opacity Data.
C     !DASH
      save
C     !DASH
      real*8 OPAC, WT, ZLAM
      integer I, II, IJ, IK, K, NO
      logical KILROY
C     !DASH
      external  ABJECT, LINER, HI, BYE
      intrinsic mod
C
C               WT(10), OPAC(11,9,10)
      dimension WT(*),  OPAC(11,9,*)
C
      call HI ('PILAR')
C     !BEG
      if(K.gt.0) then
C
        if(KILROY) then
          KILROY = .false.
          call ABJECT  (NO)
          write (NO,100)
  100     format(' ','Dump of unprocessed Statistical Line Opacity ',
     $               'Data.')
          call LINER   (1, NO)
        end if
C
        if(mod(I,K).eq.0) then
          do 102 IK = 1,10
            write (NO,101) ZLAM,IK,WT(IK),
     $                     ((OPAC(II,IJ,IK),IJ=1,9),II=1,11)
  101       format(' ',F12.7,I13,F13.7/
     $            (' ',F12.7,8F13.7))
            call LINER (1, NO)
  102     continue
        end if
C
      end if
C     !END
      call BYE ('PILAR')
C
      return
      end
